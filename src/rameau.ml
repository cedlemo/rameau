(*
 * Copyright 2017 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of OCaml-libmpdclient.
 *
 * OCaml-libmpdclient is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * OCaml-libmpdclient is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-libmpdclient.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Lwt.Infix
open Notty
open Notty_lwt

module Terminal = Notty_lwt.Term

open Types
open Types.Internal_data

let gen_state_img idata =
  let state_img = match idata.state with
    | Mpd.Status.Play -> I.(string A.(fg green) "play")
    | Mpd.Status.Pause -> I.(string A.(fg lightblack) "Pause")
    | Mpd.Status.Stop -> I.(string A.(fg black ++ bg lightblack) "Stop")
    | Mpd.Status.ErrState -> I.(string A.(fg red) "State Error")
  in
  I.(string A.(fg white) "[state ] : " <|> state_img)

let gen_volume_img idata =
  I.(strf ~attr:A.(fg white)   "[volume] : %d" idata.volume)

(** Assemble a list of list of Notty.image in a grid image *)
let grid xxs = xxs |> List.map I.hcat |> I.vcat

(** Organize ascii code for decorations of the same kind *)
type ascii_corners = { tl: Notty.image; (** top left *)
                       tr: Notty.image; (** top right *)
                       bl: Notty.image; (** bottom left *)
                       br: Notty.image  (** bottom right *)
                     }

let gen_ugly_title_bar idata (w,h) =
  let attr = A.(fg lightred ) in
  let tab_corners = { tl = I.uchar attr (Uchar.of_int 0x256d) 1 1;
                      tr = I.uchar attr (Uchar.of_int 0x256e) 1 1;
                      bl = I.uchar attr (Uchar.of_int 0x2570) 1 1;
                      br = I.uchar attr (Uchar.of_int 0x256f) 1 1
                    } in
  let tab_hbar w = I.uchar attr (Uchar.of_int 0x2500) w 1 in
  let tab_vbar h = I.uchar attr (Uchar.of_int 0x2502) 1 h in
  let background =
    grid [
         [tab_corners.tl; tab_hbar (w - 2); tab_corners.tr];
         [tab_vbar 1    ; I.void (w - 2) 1 ; tab_vbar 1    ];
         [tab_corners.bl; tab_hbar (w - 2); tab_corners.br];
    ] in
  let foreground = grid [ [I.void w 1];
                          [I.(hcat [I.void 1 1; gen_state_img idata; I.void 1 1; gen_volume_img idata])];
                          [I.void w 1]] in
  I.(foreground </> background)

let gen_title_bar idata (w,h) =
  let attr = A.(fg lightgreen ) in
  let app_name = I.(string A.(fg magenta) "♯ ℛameau ♫ ") in
  let view = I.(string A.(fg white) "< Queue >") in
  let tab_h_dotted_bar w = I.uchar attr (Uchar.of_int 0x2508) w 1 in
  I.(vcat [void w 1;
           hcat [void 1 1; app_name; void 1 1; view; void 1 1; gen_state_img idata; void 1 1; gen_volume_img idata];
           hcat [void 1 1; tab_h_dotted_bar (w - 2); void 1 1];])

(* artist title track album time *)
let build_song_line song current selected term_width =
  let norm_attr = A.(fg lightblack) in
  let curr_attr = A.(fg blue) in
  let sel_attr = A.(fg lightblue ++ bg black) in
  let attr = match selected, current with
    | true, _ -> sel_attr
    | false, true -> curr_attr
    | false, false -> norm_attr
  in
  let duration_to_string d =
    let m = mod_float d 60. in
    let min = int_of_float ((d -. m) /. 60.) in
    let sec = int_of_float m in
    Printf.sprintf "%d:%.2d" min sec in
  let title = Mpd.Song.title song in
  let artist = Mpd.Song.artist song in
  let album = Mpd.Song.album song in
  let time = Mpd.Song.duration song in
  let track = Mpd.Song.track song in
  let perc p i =
    let i' = float_of_int i in
    int_of_float (i' *. p /. 100.)
  in
  let w = term_width - 1 in
  let current_mark = I.(uchar attr (Uchar.of_int 0x25C8) 1 1) in
  let background_bar = I.(uchars attr (Array.make term_width (Uchar.of_char ' '))) in
  let foreground_bar = I.hcat [
    if current then current_mark else I.(void 1 1);
    I.(void 1 1);
    I.(hsnap ~align:`Left (perc 20. w) (string attr artist));
    I.(hsnap ~align:`Left (perc 50. w) (string attr title));
    I.(hsnap ~align:`Left (perc 20. w) (string attr album));
    I.(hsnap ~align:`Middle (perc 5. w) (string attr track));
    I.(hsnap ~align:`Right (perc 5. w) (string attr (duration_to_string time)));
  ] in
  I.(foreground_bar </> background_bar)

let gen_playlist_img selected idata (w, h) =
  match idata.queue with
  | PlaylistError message -> Lwt.return I.(strf ~attr:A.(fg red) "Error: %s" message)
  | Playlist songs ->
    let lines = List.mapi (fun i song ->
      build_song_line song (idata.song = i) (selected = i) (w - 2)
    ) songs in
    I.(vcat lines
    |> hpad 1 1
    |> vpad 1 1)
    |> Lwt.return

let render idata selected (w, h) =
    match idata with
    | Error message -> Lwt.return I.(strf ~attr:A.(fg red) "[there is a pb %s]" message)
    | Ok idata' ->
        let title_bar = gen_title_bar idata' (w,h) in
      gen_playlist_img selected idata' (w, h)
      >>= fun songs_img ->
      Lwt.return I.(title_bar <-> songs_img)

let listen_mpd_event client =
  Mpd.Client_lwt.idle client >|= fun evt -> `Mpd_event evt

let event term = Lwt_stream.get (Terminal.events term) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let result_status_playlist_length = function
  | Error _ -> -1
  | Ok status -> match status.queue with | Playlist p -> List.length p | _ -> 0

let rec loop term (e, t) dim client idata selected =
  (e <?> t) >>= function
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) ->
      Mpd.Client_lwt.close client
  | `Mpd_event event_name ->
      Internal_data.fetch client
      >>= fun idata' ->
        render idata' selected dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (e, listen_mpd_event client) dim client idata' selected
  | `Resize dim ->
      Internal_data.update idata client
      >>= fun idata' ->
        render idata' selected dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (event term, t) dim client idata' selected
  | `Key (`ASCII 'j', []) ->
      let pl_len = result_status_playlist_length idata in
      let sel = if selected + 1 >= pl_len then 0 else selected + 1
      in Internal_data.update idata client
      >>= fun idata' ->
        render idata' selected dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (event term, t) dim client idata' sel
  | `Key (`ASCII 'k', []) ->
      let pl_len = result_status_playlist_length idata in
      let sel = if selected - 1 < 0 then pl_len - 1 else selected - 1
      in Internal_data.update idata client
      >>= fun idata' ->
        render idata' selected dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (event term, t) dim client idata'  sel
  | `Key (`Enter, []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata selected
      | Ok d -> (
            Commands.rameau_play client d selected
            >>= fun () ->
              loop term (event term, t) dim client idata selected
      )
  )
  | `Key (`ASCII 's', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata selected
      | Ok d -> (
            Commands.rameau_stop client d
            >>= fun () ->
              loop term (event term, t) dim client idata selected
      )
  )
  | `Key (`ASCII 'p', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata selected
      | Ok d -> (
            Commands.rameau_toggle_pause client d
            >>= fun () ->
              loop term (event term, t) dim client idata selected
      )
  )
  | `Key (`ASCII '+', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata selected
      | Ok d -> (
            Commands.rameau_inc_vol client d
            >>= fun () ->
              loop term (event term, t) dim client idata selected
      )
  )
  | `Key (`ASCII '-', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata selected
      | Ok s -> (
            Commands.rameau_decr_vol client s
            >>= fun () ->
              loop term (event term, t) dim client idata selected
      )
  )
  | _ -> render idata selected dim
         >>= fun img ->
           Terminal.image term img
           >>= fun () ->
             loop term (event term, t) dim client idata selected

let interface client =
  let term = Terminal.create () in
  let size = Terminal.size term in
  Internal_data.fetch client
  >>= fun idata ->
    render idata 0 size
    >>= fun img ->
      Terminal.image term img
      >>= fun () ->
        loop term (event term, listen_mpd_event client) size client idata 0

let run host port =
  let open Mpd in
  let main_thread =
    Mpd.Connection_lwt.initialize host port
    >>= fun connection ->
      Mpd.Client_lwt.initialize connection
      >>= fun client ->
        interface client
  in
  Lwt_main.run (
    Lwt.catch
      (fun () -> main_thread)
      (function
        | Mpd.Connection_lwt.Lwt_unix_exn message ->
            Lwt_io.write_line Lwt_io.stderr message
        | _ -> Lwt_io.write_line Lwt_io.stderr "Exception not handled. Exit ..."
      )
  )

let () =
  (run "127.0.0.1" 6600)
