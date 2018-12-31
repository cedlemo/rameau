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

open Lwt.Infix
open Notty
open Notty_lwt
module Terminal = Notty_lwt.Term

open View
open Widgets

let listen_mpd_event client =
  Mpd.Client_lwt.idle client >|= fun evt -> `Mpd_event evt

let event term = Lwt_stream.get (Terminal.events term) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let result_status_playlist_length = function
  | Error _ -> -1
  | Ok data ->
    match data with
    | Music_db _ | Help _ -> 0
    | Queue {status; plist; selected} -> match plist with
      | Ok p -> List.length p
      | _ -> 0

let render_img idata dim =
  match idata with
  | Error message ->
    let err_message = Printf.sprintf "[render internal data]: %s" message in
    Loggin.err err_message
    >>= fun () -> let img = Widgets.error err_message in Lwt.return img
  | Ok idata' -> View.render idata' dim

let rec loop term (ev_term, ev_mpd) dim client idata =
  let render_and_loop term events idata dim client =
    render_img idata dim
    >>= fun img ->
    Terminal.image term img
    >>= fun () ->
    loop term events dim client idata
  in
  let new_events () =
    (event term, listen_mpd_event client)
  in
  (ev_term <?> ev_mpd) >>= function
  | `Mpd_event event_name -> begin
      begin match idata with
        | Error _ -> View_manager.create client Shortcuts.queue Drawing.queue
        | Ok d -> View_manager.force_update d client
      end
      >>= fun idata' ->
      let events = (ev_term, listen_mpd_event client) in
      render_and_loop term events idata' dim client
    end
  | `Resize dim -> begin
      Lwt.cancel ev_mpd;
      View_manager.update idata client
      >>= fun idata' ->
      render_and_loop term (new_events ()) idata' dim client
    end
  | other_keys -> match idata with
    | Error _ -> loop term (event term, ev_mpd) dim client idata
    | Ok idata' ->
      let shortcuts = get_shortcuts idata' in
      shortcuts other_keys  client ev_mpd idata'
      >>= function
      | View.True ->  loop term (new_events ()) dim client idata
      | View.WithUpdate idata' ->
        render_and_loop term (new_events ()) (Ok idata') dim client
      | View.False ->
        Shortcuts.global other_keys client ev_mpd idata'
        >>= function
        | View.True -> loop term (new_events ()) dim client idata
        | View.WithUpdate idata' ->
          render_and_loop term (new_events ()) (Ok idata') dim client
        | View.False ->
          render_and_loop term (event term, ev_mpd) idata dim client

let create client =
  let term = Terminal.create () in
  let size = Terminal.size term in
  View_manager.create client Shortcuts.queue Drawing.queue
  >>= fun idata ->
  render_img idata size
  >>= fun img ->
  Terminal.image term img
  >>= fun () ->
  let events = event term, listen_mpd_event client in
  loop term events size client idata
