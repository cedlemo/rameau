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

open Mpd
open Lwt.Infix
open Notty
open Notty_lwt
module MDA = Mpd_data_access
module MOS = MDA.Mpd_or_stubs

type view = Queue_view | Help_view | Music_db_view

type status =
  { timestamp: float;        (** Used to limit the number of request to Mpd ie: one each sec *)
    state: Mpd.Status.state; (** Mpd state Play, Stop, Pause *)
    volume: int;             (** Mpd volume *)
    song: int;               (** The current song. *)
  }

type panel = { items: string list; selected: int}
type music_db_selector = { artist: panel; album: panel; song: panel }
type t =
  | Queue of { status: status;
               plist: (MDA.Song.t list, string) result;
               selected: int;
               shortcuts: shortcuts }
  | Music_db of { status: status;
                  db: music_db_selector ;
                  shortcuts: shortcuts }
  | Help of { status: status;
              shortcuts: shortcuts }
and
shortcuts = [ `End
            | `Key of Notty.Unescape.key
            | `Mouse of Notty.Unescape.mouse
            | `Mpd_event of (string, string) result
            | `Paste of Notty.Unescape.paste
            | `Resize of int * int ] -> Mpd.Client_lw.t -> 'a -> t -> Lwt.t bool

let view_name = function
  | Queue _ -> "Queue"
  | Music_db _ -> "Music Database"
  | Help _ -> "Help"

let get_status = function
  | Help {status} -> status
  | Queue {status; plist; selected} -> status
  | Music_db {status; db} -> status

let get_selected = function
  | Help {status} -> -1
  | Queue {status; plist; selected} -> selected
  | Music_db {status; db} -> db.artist.selected

let get_shortcuts = function
  | Help {status; shortcuts} -> shortcuts
  | Queue {status; plist; selected; shortcuts} -> shortcuts
  | Music_db {status; db; shortcuts} -> shortcuts

let get_n_elements = function
  | Help {status} -> -1
  | Music_db {status; db} ->
    let n_artist = List.length db.artist.items in
    let n_album = List.length db.album.items in
    let n_song = List.length db.song.items in
    max n_artist n_album
    |> max n_song
  | Queue {status; plist; selected} -> match plist with
    | Error _ -> -1
    | Ok p -> List.length p

let set_selected (i,i',i'') = function
  | Help help -> Help help
  | Queue {status; plist; selected; shortcuts} ->
    Queue {status; plist; selected = i; shortcuts}
  | Music_db {status; db; shortcuts} ->
    let artist_pan = {db.artist with selected = i} in
    let album_pan = {db.album with selected = i'} in
    let song_pan = {db.song with selected = i''} in
    Music_db {status;
              db = {artist = artist_pan;
                    album = album_pan;
                    song = song_pan};
             shortcuts}

let fetch_music_db client artist_sel album_sel song_sel =
  MOS.fetch_artists_in_music_db ~client ()
  >>= function
  | Error message -> Lwt.return_error message
  | Ok artists ->
    let artists_pan = { items = artists; selected = artist_sel } in
    let artist = List.nth artists artists_pan.selected in
    MOS.fetch_albums_in_music_db ~client artist
    >>= function
    | Error message -> Lwt.return_error message
    | Ok albums ->
      let albums_pan = { items = albums; selected = album_sel } in
      let album = List.nth albums albums_pan.selected in
      MOS.fetch_songs_in_music_db ~client artist album
      >>= function
      | Error message -> Lwt.return_error message
      | Ok  songs ->
        let songs_pan = { items = songs; selected = song_sel } in
        let music_db =  {artist = artists_pan;
                         album = albums_pan;
                         song = songs_pan} in
        Lwt.return_ok music_db

let create ?(view=Queue_view) client =
  MOS.fetch_status ~client ()
  >>= function
  | Error message -> Lwt.return_error message
  | Ok (timestamp, state, volume, song) ->
    let status = {timestamp; state; volume; song} in
    let shortcuts events client t idata =
      match events with
      | _-> Lwt.return false
    in
    match view with
    | Help_view -> Lwt.return_ok (Help { status; shortcuts })
    | Queue_view ->
      let shortcuts events client t idata =
        match events with
        | `Key (`Enter, [])     ->
          Lwt.cancel t;
          Commands.rameau_play client idata
          >>= fun _ -> Lwt.return true
        | `Key (`ASCII 's', []) ->
          Lwt.cancel t;
          Commands.rameau_stop client idata
          >>= fun _ -> Lwt.return true
        | _ -> Lwt.return false
      in
      MOS.fetch_queue_list ~client ()
      >>= fun plist -> Lwt.return_ok (Queue { status;
                                              plist;
                                              selected = 0;
                                              shortcuts
                                            })
    | Music_db_view ->
      fetch_music_db client 0 0 0
      >>= function
      | Error message -> Lwt.return_error message
      | Ok  db -> Lwt.return_ok (Music_db { status; db; shortcuts })

let force_update idata client =
  MOS.fetch_status ~client ()
  >>= function
  | Error message -> Lwt.return_error message
  | Ok (timestamp, state, volume, song) ->
    let status = {timestamp; state; volume; song} in
    match idata with
    | Help {status = _; shortcuts} -> Lwt.return_ok (Help {status; shortcuts})
    | Queue {status = _; plist; selected; shortcuts} ->
      MOS.fetch_queue_list ~client ()
      >>= fun plist ->
      Lwt.return_ok (Queue {status; plist; selected; shortcuts})
    | Music_db {status = _; db; shortcuts} ->
      fetch_music_db client db.artist.selected db.album.selected db.song.selected
      >>= function
      | Error message -> Lwt.return_error message
      | Ok db -> Lwt.return_ok (Music_db {status; db; shortcuts})

let update idata client =
  let now = Unix.time () in
  match idata with
  | Error message -> Lwt.return_error message
  | Ok internal_data ->
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
    match internal_data with
    | Help {status; shortcuts} -> Lwt.return_ok internal_data
    | Queue {status; plist; selected; shortcuts} ->
      let prev = status.timestamp in
      if ((now -. prev) > 1.0) then
        MOS.fetch_queue_list ~client ()
        >>= fun plist ->
        MOS.fetch_status ~client ()
        >>= function
        | Error message -> Lwt.return_error message
        | Ok (timestamp, state, volume, song) ->
          let status = {timestamp; state; volume; song} in
          Lwt.return_ok (Queue {status; plist; selected; shortcuts})
      else Lwt.return_ok internal_data
    | Music_db {status; db; shortcuts} ->
      let prev = status.timestamp in
      if ((now -. prev) > 1.0) then
        MOS.fetch_status ~client ()
        >>= function
        | Error message -> Lwt.return_error message
        | Ok (timestamp, state, volume, song) ->
          let s = {timestamp; state; volume; song} in
          begin if ((now -. prev) > 2.0) then
              fetch_music_db client db.artist.selected db.album.selected db.song.selected
            else Lwt.return_ok db
          end
          >>= function
          | Error message -> Lwt.return_error message
          | Ok db -> Lwt.return_ok (Music_db {status = s; db; shortcuts})
      else Lwt.return_ok internal_data
