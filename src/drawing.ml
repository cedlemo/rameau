(*
 * Copyright 2018 Cedric LE MOIGNE, cedlemo@gmx.com
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
open Lwt.Infix
open View

let help idata (w, h) =
  match idata with
  | Help _ ->  let title_bar = Widgets.gen_title_bar idata (w,h) in
    Widgets.gen_help_view (w, h)
    >>= fun view -> Lwt.return I.(title_bar <-> view)
  | _ -> let msg = "Drawing.help: Help view expected." in
    let view = Widgets.error msg in Lwt.return view

let music_db idata  (w, h) =
  match idata with
  | Music_db {status; db; shortcuts; render} ->
    let title_bar = Widgets.gen_title_bar idata (w,h) in
    let view_port_height =  h - I.(height title_bar) in
    Widgets.gen_pan_list db.artist ((w - 3) / 3, view_port_height) "No artist(s)"
    >>= fun artists ->
    Widgets.gen_pan_list db.album ((w - 3) / 3, view_port_height) "No album(s)"
    >>= fun albums ->
    Widgets.gen_pan_list db.song ((w - 3) / 3, view_port_height) "No song(s)"
    >>= fun songs ->
    let view = (I.hcat [artists; albums; songs]) in
    Lwt.return I.(title_bar <-> view)
  | _ -> let msg = "Drawing.music_db: Music database view expected." in
    let view = Widgets.error msg in Lwt.return view

let queue idata (w, h) =
  match idata with
  | Queue {status; plist; selected; shortcuts; render} ->
    let title_bar = Widgets.gen_title_bar idata (w,h) in
    let view_port_height =  h - I.(height title_bar) in
    Widgets.gen_playlist_img selected plist status.song (w, view_port_height)
    >>= fun view ->
    Lwt.return I.(title_bar <-> view)
  | _ -> let msg = "Drawing.queue: Queue view expected." in
    let view = Widgets.error msg in Lwt.return view
