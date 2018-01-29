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
                                           | Playlist p -> List.length p
                                           | _ -> 0

let rec loop term (e, t) dim client idata =
  (e <?> t) >>= function
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) | `Key (`ASCII 'q', []) ->
      Mpd.Client_lwt.close client
  | `Mpd_event event_name -> (
      Loggin.log "mpd idle event"
      >>= fun () ->
        begin
          match idata with
          | Error _ -> Internal_data.create client
          | Ok d -> Internal_data.force_update d client
        end
        >>= fun idata' ->
          render idata' dim
          >>= fun img ->
            Terminal.image term img
            >>= fun () ->
              let events = (e, listen_mpd_event client) in
              loop term events dim client idata')
  | `Resize dim -> (
      Internal_data.update idata client
      >>= fun idata' ->
        render idata' dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (event term, t) dim client idata')
  | `Key (`ASCII 'j', []) -> (
    Loggin.log "j"
    >>= fun () ->
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok data ->
          let selected = get_selected data in
          let pl_len = get_n_elements data in
          let sel = if selected + 1 >= pl_len then 0 else selected + 1 in
          let d = set_selected sel data in
          Internal_data.update (Ok d) client
          >>= fun idata' ->
            render idata' dim
            >>= fun img ->
              Terminal.image term img
              >>= fun () ->
                loop term (event term, t) dim client idata' )
  | `Key (`ASCII 'k', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok data ->
          let selected = get_selected data in
          let pl_len = get_n_elements data in
          let sel = if selected - 1 < 0 then pl_len - 1 else selected - 1 in
          let d = set_selected sel data in
          Internal_data.update (Ok d) client
          >>= fun idata' ->
            render idata' dim
            >>= fun img ->
              Terminal.image term img
              >>= fun () ->
                loop term (event term, t) dim client idata')
  | `Key (`Enter, []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok d -> (
            Commands.rameau_play client d
            >>= fun () ->
              loop term (event term, t) dim client idata
      )
  )
  | `Key (`ASCII 's', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok d -> (
            Commands.rameau_stop client d
            >>= fun () ->
              loop term (event term, t) dim client idata
      )
  )
  | `Key (`ASCII 'p', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok d -> (
            Commands.rameau_toggle_pause client d
            >>= fun () ->
              loop term (event term, t) dim client idata
      )
  )
  | `Key (`ASCII '+', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok d -> (
            Commands.rameau_inc_vol client d
            >>= fun () ->
              loop term (event term, t) dim client idata
      )
  )
  | `Key (`ASCII '-', []) -> (
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok s -> (
            Commands.rameau_decr_vol client s
            >>= fun () ->
              loop term (event term, t) dim client idata
      )
  )
  | `Key (`ASCII '1', []) -> begin
    Loggin.log "1"
    >>= fun () ->
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok d ->
          Mpd.Client_lwt.noidle client
          >>= fun () ->
            Internal_data.create ~view:Internal_data.Queue_view client
            >>= fun idata' ->
              render idata' dim
              >>= fun img ->
                Terminal.image term img
                >>= fun () ->
                  loop term (event term, t) dim client idata'
    end
  | `Key (`ASCII '2', []) -> begin
    Loggin.log "2"
    >>= fun () ->
      match idata with
      | Error _ -> loop term (event term, t) dim client idata
      | Ok d ->
          Mpd.Client_lwt.noidle client
          >>= fun () ->
            Internal_data.create ~view:Internal_data.Music_db_view client
            >>= fun idata' ->
              render idata' dim
              >>= fun img ->
                Loggin.log "post render"
                >>= fun () ->
                  Terminal.image term img
                  >>= fun () ->
                    Loggin.log "post image term"
                    >>= fun () ->
                      loop term (event term, t) dim client idata'
    end
  | _ -> Loggin.log "extra case in loop" >>= fun () ->
       render idata dim
         >>= fun img ->
           Terminal.image term img
           >>= fun () ->
             loop term (event term, t) dim client idata

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
