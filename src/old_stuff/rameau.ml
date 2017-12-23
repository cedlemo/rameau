(*
 * Copyright 2017 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of Rameau.
 *
 * Rameau is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Rameau is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Rameau.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Notty
open Notty_lwt
open Notty.Infix
open Lwt
open Mpd
open Widgets

let host = "127.0.0.1"
let port = 6600

let create_client () =
  Mpd.LwtConnection.initialize host port
  >>= fun connection ->
  match connection with
  | None -> Lwt.return None
  | Some (c) -> Lwt.return (Some (Mpd.LwtClient.initialize c))


module LwtTerm = Notty_lwt.Term

let tab_outline attr t tabname =
  let tab_corners = { tl = I.uchar attr 0x256d 1 1;
                      tr = I.uchar attr 0x256e 1 1;
                      bl = I.uchar attr 0x2570 1 1;
                      br = I.uchar attr 0x256f 1 1
                    } in
  let tab_hbar w = I.uchar attr 0x2500 w 1 in
  let tab_vbar h = I.uchar attr 0x2502 1 h in
  let label = I.string A.(fg lightgreen ++ bg lightblack) tabname in
  let (lw, lh) = (I.width label, I.height label) in
  let (w, h) = LwtTerm.size t in
  grid [ [tab_corners.tl; label; tab_corners.tr];
         [tab_vbar 1; I.void lw 1; tab_corners.bl; tab_hbar (w - (lw + 3)); tab_corners.tr];
         [tab_vbar (h - 3); I.void (w - 2) 1; tab_vbar (h - 3)];
         [tab_corners.bl; tab_hbar (w - 2); tab_corners.br]
       ]

let top_right_conn_banner term client =
  let banner = Mpd.LwtClient.mpd_banner client in
  let label = I.string A.(fg lightgreen ++ bg lightblack) banner in
  let (lw, lh) = (I.width label, I.height label) in
  let (w, h) = LwtTerm.size term in
  (I.void (w - lw) 1 <|> label)

(* let list_box_line song =
  let label str = I.string A.(fg lightwhite) str in
  let id = label (string_of_int (Song.id song)) in
  let title = label (Song.title song) in
  let artist = label (Song.artist song) in
  (I.void 1 1 <|> id <|> I.void 1 1 <|> title <|> I.void 1 1 <|> artist)
*)
let playlist_to_strings playlist =
  List.map (fun song ->
      let item = [] in
      (string_of_int (Song.id song)) :: item in
let _ = (Song.title song) :: item in
let _ = (Song.artist song) :: item in
item
  ) playlist

let draw_list_box client (w, h as size) =
  MpdLwtQueue.playlist client
  >>= function
  | MpdLwtQueue.PlaylistError message -> let img = Widgets.error_panel message size in
    Lwt.return img
  | MpdLwtQueue.Playlist playlist -> Lwt.return playlist
    >>= fun p ->
    let img = Widgets.list_box (playlist_to_strings p) in
    Lwt.return img


let draw term client =
  draw_list_box client (LwtTerm.size term)
  >>= fun content ->
    let conn_banner = top_right_conn_banner term client in
    Lwt.return I.((tab_outline A.(fg lightred ) term "Current Playlist")
        </>
        conn_banner
        </>
        content
     )

let rec main term (x, y as pos) client =
  draw term client
  >>= fun img ->
  LwtTerm.image term img
    >>= fun () ->
      Lwt_stream.get (LwtTerm.events term)
      >>= fun event ->
      match event with
      | None -> LwtTerm.release term
        >>= fun () ->
        Lwt.return_unit
      | Some (`Resize _ | #Unescape.event as x) -> match x with
        | `Key (`Escape, []) | `Key (`Uchar 67, [`Ctrl]) -> LwtTerm.release term
          >>= fun () ->
          Lwt.return_unit
        | `Resize (cols, rows) -> main term (cols, rows) client
        | _ ->Lwt.return ()
          >>= fun () -> main term pos client

let launch () =
  create_client ()
  >>= function
  | None -> Lwt.return_unit
  | Some client -> client
    >>= fun c ->
    let t = LwtTerm.create () in
    let size = LwtTerm.size t in
    main t size c

let () =
    Lwt_main.run @@ launch ()
