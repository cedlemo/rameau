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

open Mpd
open Lwt.Infix

module Internal_data = struct
  type view = Queue | Help | Music_db   (** Use to represent the type of interface to draw *)

  let string_of_view = function
    | Queue -> "Queue"
    | Help -> "Help"
    | Music_db -> "Music database"

  type t = {
    timestamp : float;        (** Used to limit the number of request to Mpd ie: one each sec *)
    state : Mpd.Status.state; (** Mpd state Play, Stop, Pause *)
    volume : int;             (** Mpd volume *)
    queue : Mpd.Queue_lwt.t;  (** The Mpd Queue to request. *)
    song : int;               (** The current song. *)
    view : view;              (** The current view Rameau is displaying. *)
    db: Mpd.Music_database_lwt.song_count list       (** A list of all the songs in Mpd. Used only in the Music_db view. *)
  }

  (** Used to get the internal data *)
  let fetch ?(view=Queue) ?(db=[]) client =
    Mpd.Client_lwt.status client
    >>= fun response ->
      match response with
      | Error message -> Lwt.return (Error message)
      | Ok d ->
          let timestamp = Unix.time () in
          let state = Mpd.Status.state d in
          let volume = Mpd.Status.volume d in
          let song = Mpd.Status.song d in
          Mpd.Queue_lwt.playlist client
          >>= fun queue ->
            Lwt.return (Ok {timestamp; state; volume; queue; song; view; db})

  let fetch_music_db ?(view=Queue) idata client =
    Loggin.log "fetch_music_db"
    >>= fun () ->
    Mpd.Music_database_lwt.count client [] ?group:(Some Mpd.Music_database_lwt.Artist) ()
    >>= function
      | Error _ -> Lwt.return idata (* TOFIX: *)
      | Ok song_count ->
        Loggin.log (Printf.sprintf "nb artiste %d" (List.length song_count))
        >>= fun () ->
        match idata with
        | Error message -> Loggin.log message >>= fun () -> Lwt.return idata
        | Ok d -> let timestamp = Unix.time () in
            Lwt.return (Ok { d with db = song_count; view; timestamp })

  let update idata client =
    let now = Unix.time () in
    match idata with
    | Error _ -> Lwt.return idata
    | Ok d -> Mpd.Client_lwt.noidle client
        >>= fun () ->
          match d.view with
          | Queue ->
            if ((now -. d.timestamp) > 1.0) then fetch ~view:d.view client
            else Lwt.return idata
          | Help -> Lwt.return idata
          | Music_db ->
            if ((now -. d.timestamp) > 2.0) || (d.db = []) then
              fetch_music_db ~view:d.view idata client
            else Lwt.return idata
end
