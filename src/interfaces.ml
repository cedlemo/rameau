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

let gen_state_img idata =
  let state_img = match idata.state with
    | Mpd.Status.Play -> I.(string A.(fg lightgreen) "Playing ▶️")
    | Mpd.Status.Pause -> I.(string A.(fg lightblack) "Paused ⏸")
    | Mpd.Status.Stop -> I.(string A.(fg yellow) "Stopped ⏹")
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
  let view = I.(string A.(fg white) "Queue") in
  let view_tab = I.(hcat [uchar attr (Uchar.of_int 0x23A1 (*⎡*)) 1 1; view; uchar attr (Uchar.of_int 0x23A4 (*⎤*)) 1 1]) in
  let tab_h_dotted_bar w = I.uchar attr (Uchar.of_int 0x2508) w 1 in
  let state_img = gen_state_img idata in
  let vol_img = gen_volume_img idata in
  let used_width = List.fold_left (fun acc i -> (I.width i) + acc) 4 [app_name; view_tab; state_img; vol_img] in
  let empty_width = w - used_width in
  I.(vcat [void w 1;
           hcat [void 1 1; app_name; void 1 1; view_tab; void empty_width 1; state_img; void 1 1; vol_img];
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
