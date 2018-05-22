(*
 * Copyright 2018 Cedric LE MOIGNE, cedlemo@gmx.com
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
open Types.Internal_data

(**
 * view layouts :
   *
 * title bar:
   *
 * app_name view_indicator empty_space state_indicator volume_indicator
 * separator
 *
 * viewport width = term_width height = term_height - 2 (from title_bar)
 *
 * *)

let duration_to_string d =
  let m = mod_float d 60. in
  let min = int_of_float ((d -. m) /. 60.) in
  let sec = int_of_float m in
  Printf.sprintf "%d:%.2d" min sec

let gen_state_img s =
  let state_img = match s.state with
    | Mpd.Status.Play -> I.(string A.(fg lightgreen) "Playing ▶️")
    | Mpd.Status.Pause -> I.(string A.(fg lightblack) "Paused =")
    | Mpd.Status.Stop -> I.(string A.(fg yellow) "Stopped ■")
    | Mpd.Status.ErrState -> I.(string A.(fg red) "State Error")
  in
  I.(string A.(fg white) "[state ] : " <|> state_img)

let gen_volume_img s =
  I.(strf ~attr:A.(fg white)   "[volume] : %d" s.volume)

  (** Assemble a list of list of Notty.image in a grid image *)
let grid xxs = xxs |> List.map I.hcat |> I.vcat

(** Organize ascii code for decorations of the same kind *)
type ascii_corners =
  { tl: Notty.image; (** top left *)
                       tr: Notty.image; (** top right *)
                       bl: Notty.image; (** bottom left *)
                       br: Notty.image; (** bottom right *)
}

let gen_title_bar internal_data (w,h) =
  let _gen_title_bar s view_name =
    let attr = A.(fg lightgreen ) in
    let app_name = I.(string A.(fg magenta) "♯ rameau ♫ ") in
    let view = I.(string A.(fg white) view_name) in
    let view_tab = I.(hcat [uchar attr (Uchar.of_int 0x23A1 (*⎡*)) 1 1; view; uchar attr (Uchar.of_int 0x23A4 (*⎤*)) 1 1]) in
    let tab_h_dotted_bar w = I.uchar attr (Uchar.of_int 0x2508) w 1 in
    let state_img = gen_state_img s in
    let vol_img = gen_volume_img s in
    let used_width = List.fold_left (fun acc i -> (I.width i) + acc) 4 [app_name; view_tab; state_img; vol_img] in
    let empty_width = w - used_width in
    I.(vcat [void w 1;
             hcat [void 1 1; app_name; void 1 1; view_tab; void empty_width 1; state_img; void 1 1; vol_img];
             hcat [void 1 1; tab_h_dotted_bar (w - 2); void 1 1];])
    in
  match internal_data with
  | Help {status} -> _gen_title_bar status "Help"
  | Queue {status; plist; selected} -> _gen_title_bar status "Queue"
  | Music_db {status; db} -> _gen_title_bar status "Music database"

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
  let title = Mpd.Song.title song in
  let artist = Mpd.Song.artist song in
  let album = Mpd.Song.album song in
  let time = Mpd.Song.duration song in
  let track = Mpd.Song.track song in
  let perc p i =
    let i' = float_of_int i in
    int_of_float (i' *. p /. 100.)
    in
  let w = term_width - 3 in
  let current_mark = I.(uchar attr (Uchar.of_int 0x25C8) 1 1) in
  let space_char = Uchar.of_char ' ' in
  let background_bar = I.(uchars attr (Array.make term_width space_char)) in
  let track_img = I.(string attr track) in
  let duration_img = I.(string attr (duration_to_string time)) in
  let sep = I.(string A.(fg lightgreen) " ⋅ ") (* 0x22C5 *) in
  let non_fixed_width =
    w - ( 4 * I.((width sep) + (width track_img) + (width duration_img))) in
  let foreground_bar = I.hcat [
    if current then current_mark else I.(void 1 1);
    sep;
    I.(hsnap ~align:`Left (perc 20. non_fixed_width) (string attr artist));
    sep;
    I.(hsnap ~align:`Left (perc 60. non_fixed_width) (string attr title));
    sep;
    I.(hsnap ~align:`Left (perc 20. non_fixed_width) (string attr album));
    sep;
    duration_img;
    sep;
    track_img ;
    I.(void 1 1);
    ] in
  I.(foreground_bar </> background_bar)

open Mpd.Queue_lwt

let gen_playlist_img selected plist current_song (w, h) =
  match plist with
  | PlaylistError message ->
      Lwt.return I.(strf ~attr:A.(fg red) "Error: %s" message)
  | Playlist songs ->
      let lines = List.mapi (fun i song ->
        build_song_line song (current_song = i) (selected = i) (w - 2)
        ) songs in
      let padding = 1 in
      let size_diff = h - (selected + 1 + padding) in
      let to_crop = if size_diff < 0 then abs size_diff else 0 in
      I.(vcat lines |> hpad padding padding |> vpad padding padding)
      |> I.vcrop to_crop 0
      |> Lwt.return

open Mpd.Music_database_lwt

let build_pan_line is_selected artist_info =
  let sel_attr = A.(fg lightblue ++ bg black) in
  let norm_attr = A.(fg white) in
  let attr = if is_selected then sel_attr else norm_attr in
  let name = String.escaped artist_info in
  I.hcat [I.(void 1 1); I.(string attr name )]

let gen_pan_list {items; selected} (w, h) empty_message =
  match items with
  | [] ->
      I.string A.(fg red) empty_message |> Lwt.return
  | _ ->
      let lines =
        List.mapi (fun i inf -> build_pan_line (i = selected) inf) items in
      let padding = 1 in
      let align = `Left in
      I.(vcat lines
        |> hsnap ~align (w - padding * 2)
        |> vsnap (w - padding * 2)
        |> hpad padding padding
        |> vpad padding padding)
      |> Lwt.return

let gen_help_view (w, h) =
  let shortcut = I.(string A.(fg white) "shortcuts") in
  grid [[I.(void 1 1); shortcut; I.(void 1 1); shortcut;];
        [I.(void 1 1); shortcut; I.(void 1 1); shortcut;];
        [I.(void 1 1); shortcut; I.(void 1 1); shortcut;];
        [I.(void 1 1); shortcut; I.(void 1 1); shortcut;];
        [I.(void 1 1); shortcut; I.(void 1 1); shortcut;];
        ]
  |> Lwt.return

let render internal_data (w, h) =
  match internal_data with
    | Error message ->
        let err_message = Printf.sprintf "[render internal data]: %s" message in
        Loggin.err err_message
        >>= fun () ->
          Lwt.return I.(string A.(fg red) err_message)
    | Ok data ->
        let title_bar = gen_title_bar data (w,h) in
        begin
          match data with
        | Help {status} ->
            gen_help_view (w, h)
        | Music_db {status; db} ->
            let view_port_height =  h - I.(height title_bar) in
            gen_pan_list db.artist ((w - 3) / 3, view_port_height) "No artist(s)"
            >>= fun artists ->
              gen_pan_list db.album ((w - 3) / 3, view_port_height) "No album(s)"
              >>= fun albums ->
                gen_pan_list db.song ((w - 3) / 3, view_port_height) "No song(s)"
                >>= fun songs ->
                  Lwt.return (I.hcat [artists; albums; songs])
        | Queue {status; plist; selected} ->
            let view_port_height =  h - I.(height title_bar) in
            gen_playlist_img selected plist status.song (w, view_port_height)
        end
        >>= fun view -> Lwt.return I.(title_bar <-> view)
