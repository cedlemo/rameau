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
open View
module MDA = Mpd_data_access
module MOS = MDA.Mpd_or_stubs

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

let create ?(view=Queue_view) client shortcuts render =
  MOS.fetch_status ~client ()
  >>= function
  | Error message -> Lwt.return_error message
  | Ok (timestamp, state, volume, song) ->
    let status = {timestamp; state; volume; song} in
    match view with
    | Help_view -> Lwt.return_ok (Help { status; shortcuts; render})
    | Queue_view ->
      MOS.fetch_queue_list ~client ()
      >>= fun plist -> Lwt.return_ok (Queue { status;
                                              plist;
                                              selected = 0;
                                              shortcuts;
                                              render;
                                            })
    | Music_db_view ->
      fetch_music_db client 0 0 0
      >>= function
      | Error message -> Lwt.return_error message
      | Ok  db -> Lwt.return_ok (Music_db { status;
                                            db;
                                            shortcuts;
                                            render;
                                          })

let force_update idata client =
  MOS.fetch_status ~client ()
  >>= function
  | Error message -> Lwt.return_error message
  | Ok (timestamp, state, volume, song) ->
    let status = {timestamp; state; volume; song} in
    match idata with
    | Help {status = _; shortcuts; render} ->
      Lwt.return_ok (Help {status; shortcuts; render})
    | Queue {status = _; plist; selected; shortcuts; render} ->
      MOS.fetch_queue_list ~client ()
      >>= fun plist ->
      Lwt.return_ok (Queue {status; plist; selected; shortcuts; render})
    | Music_db {status = _; db; shortcuts; render} ->
      fetch_music_db client db.artist.selected db.album.selected db.song.selected
      >>= function
      | Error message -> Lwt.return_error message
      | Ok db -> Lwt.return_ok (Music_db {status; db; shortcuts; render})

let update idata client =
  let now = Unix.time () in
  match idata with
  | Error message -> Lwt.return_error message
  | Ok internal_data ->
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
    match internal_data with
    | Help {status; shortcuts; render} -> Lwt.return_ok internal_data
    | Queue {status; plist; selected; shortcuts; render} ->
      let prev = status.timestamp in
      if ((now -. prev) > 1.0) then
        MOS.fetch_queue_list ~client ()
        >>= fun plist ->
        MOS.fetch_status ~client ()
        >>= function
        | Error message -> Lwt.return_error message
        | Ok (timestamp, state, volume, song) ->
          let status = {timestamp; state; volume; song} in
          Lwt.return_ok (Queue {status; plist; selected; shortcuts; render})
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
          | Ok db -> Lwt.return_ok (Music_db {status = s; db; shortcuts; render})
      else Lwt.return_ok internal_data
