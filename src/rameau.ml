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

module Terminal = Notty_lwt.Term

open Types
open Types.Internal_data
open Interfaces


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

let rec loop term (e, t) dim client idata =
  let render_and_loop term events idata dim client =
    render idata dim
    >>= fun img ->
      Terminal.image term img
      >>= fun () ->
        loop term events dim client idata
  in
  let new_events () =
    (event term, listen_mpd_event client)
  in
  let wrap_command command =
    match idata with
    | Error _ -> loop term (event term, t) dim client idata
    | Ok d -> (
      Lwt.cancel t;
      command client d
      >>= fun () ->
        loop term (new_events ()) dim client idata
    )
  in
  let switch_view view =
    match idata with
    | Error _ -> loop term (event term, t) dim client idata
    | Ok d ->
        Lwt.cancel t;
        Mpd.Client_lwt.noidle client
        >>= fun _ ->
          Internal_data.create ~view client
          >>= fun idata' ->
            render_and_loop term (new_events ()) idata' dim client
  in
  (e <?> t) >>= function
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl])
  | `Key (`ASCII 'q', []) -> Mpd.Client_lwt.close client
  | `Mpd_event event_name -> begin
    begin match idata with
          | Error _ -> Internal_data.create client
          | Ok d -> Internal_data.force_update d client
    end
    >>= fun idata' ->
      let events = (e, listen_mpd_event client) in
      render_and_loop term events idata' dim client
  end
  | `Resize dim -> begin
    Lwt.cancel t;
    Internal_data.update idata client
      >>= fun idata' ->
        render_and_loop term (new_events ()) idata' dim client
  end
  | `Key (`ASCII 'j', []) -> begin
    match idata with
    | Error _ -> loop term (event term, t) dim client idata
    | Ok data ->
        Lwt.cancel t;
        let selected = get_selected data in
        let pl_len = get_n_elements data in
        let sel = if selected + 1 >= pl_len then 0 else selected + 1 in
        let d = set_selected (sel, 0, 0) data in(* TODO : deal with 3 selectors *)
        Internal_data.update (Ok d) client
        >>= fun idata' ->
          render_and_loop term (new_events ()) idata' dim client
  end
  | `Key (`ASCII 'k', []) -> begin
    match idata with
    | Error _ -> loop term (event term, t) dim client idata
    | Ok data ->
        Lwt.cancel t;
        let selected = get_selected data in
        let pl_len = get_n_elements data in
        let sel = if selected - 1 < 0 then pl_len - 1 else selected - 1 in
        let d = set_selected (sel, 0, 0) data in (* TODO : deal with 3 selectors *)
        Internal_data.update (Ok d) client
        >>= fun idata' ->
          render_and_loop term (new_events ()) idata' dim client
  end
  | `Key (`Enter, [])     -> wrap_command Commands.rameau_play
  | `Key (`ASCII 's', []) -> wrap_command Commands.rameau_stop
  | `Key (`ASCII 'p', []) -> wrap_command Commands.rameau_toggle_pause
  | `Key (`ASCII '+', []) -> wrap_command Commands.rameau_inc_vol
  | `Key (`ASCII '-', []) -> wrap_command Commands.rameau_decr_vol
  | `Key (`ASCII '1', []) -> switch_view Internal_data.Queue_view
  | `Key (`ASCII '2', []) -> switch_view Internal_data.Music_db_view
  | _ -> render_and_loop term (event term, t) idata dim client

let interface client =
  let term = Terminal.create () in
  let size = Terminal.size term in
  Internal_data.create client
  >>= fun internal_data ->
    render internal_data size
    >>= fun img ->
      Terminal.image term img
      >>= fun () ->
        let events = event term, listen_mpd_event client in
        loop term events size client internal_data

let run host port =
  let open Mpd in
  let main_thread =
    Loggin.setup ()
    >>= fun () ->
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
