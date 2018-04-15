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
  type view = Queue_view | Help_view | Music_db_view   (** Use to represent the type of interface to draw *)

  let string_of_view = function
    | Queue_view -> "Queue"
    | Help_view -> "Help"
    | Music_db_view -> "Music database"

  type status =
    { timestamp: float;        (** Used to limit the number of request to Mpd ie: one each sec *)
      state: Mpd.Status.state; (** Mpd state Play, Stop, Pause *)
      volume: int;             (** Mpd volume *)
      song: int;               (** The current song. *)
    }

  type panel = { items: string list; selected: int}
  type music_db_selector = { artist: panel; album: panel; song: panel }
  type t =
    | Queue of {status: status; plist: Mpd.Queue_lwt.t; selected: int }
    | Music_db of {status: status; db: music_db_selector }
    | Help of {status: status}

  let view_name = function
    | Queue _ -> "Queue"
    | Music_db _ -> "Music Playlist"
    | Help _ -> "Help"

  (** Get the status data from the internal data. *)
  let get_status = function
    | Help {status} -> status
    | Queue {status; plist; selected} -> status
    | Music_db {status; db} -> status

  (** Get the selected song from the internal data. *)
  let get_selected = function
    | Help {status} -> -1
    | Queue {status; plist; selected} -> selected
    | Music_db {status; db} -> db.artist.selected

  (** Get the list length. *)
  let get_n_elements = function
    | Help {status} -> -1
    | Music_db {status; db} ->
        let n_artist = List.length db.artist.items in
        let n_album = List.length db.album.items in
        let n_song = List.length db.song.items in
        max n_artist n_album
        |> max n_song
    | Queue {status; plist; selected} -> match plist with
        | PlaylistError _ -> -1
        | Playlist p -> List.length p

  (** Set selected. *)
  let set_selected (i,i',i'') = function
    | Help {status} -> Help {status}
    | Queue {status; plist; selected} -> Queue {status; plist; selected = i'}
    | Music_db {status; db} ->
        let artist_pan = {db.artist with selected = i} in
        let album_pan = {db.album with selected = i'} in
        let song_pan = {db.song with selected = i''} in
        Music_db {status;
                  db = {artist = artist_pan;
                        album = album_pan;
                        song = song_pan} }

  (** Used to get the internal status *)
  let fetch_status client =
    Mpd.Client_lwt.status client
    >>= function
      | Error message -> Lwt.return_error message
      | Ok d ->
          let timestamp = Unix.time () in
          let state = Mpd.Status.state d in
          let volume = Mpd.Status.volume d in
          let song = Mpd.Status.song d in
          Lwt.return_ok {timestamp; state; volume; song}

  let fetch_queue_list client =
    Mpd.Queue_lwt.playlist client

  let fetch_artists_in_music_db client =
    Mpd.Music_database_lwt.list client Mpd.Music_database_lwt.Artist []
  (* Queries to implement
   * list album artist "artist name"
   * list title album "album name" artist "artist name"
   * *)
  let fetch_albums_in_music_db client artist =
    Mpd.Music_database_lwt.(list client Album [(Artist, artist)])

  let fetch_songs_in_music_db client artist album =
    Mpd.Music_database_lwt.(list client Title [(Artist, artist); (Album, album)])

  let fetch_music_db client artist_sel album_sel song_sel =
    fetch_artists_in_music_db client
    >>= function
    | Error message -> Lwt.return_error message
    | Ok artists ->
        let artists_pan = { items = artists; selected = artist_sel } in
        let artist = List.nth artists artists_pan.selected in
        fetch_albums_in_music_db client artist
        >>= function
          | Error message -> Lwt.return_error message
          | Ok albums ->
              let albums_pan = { items = albums; selected = album_sel } in
              let album = List.nth albums albums_pan.selected in
              fetch_songs_in_music_db client artist album
              >>= function
                | Error message -> Lwt.return_error message
                | Ok  songs ->
                    let songs_pan = { items = songs; selected = song_sel } in
                    let music_db =  {artist = artists_pan;
                                     album = albums_pan;
                                     song = songs_pan} in
                    Lwt.return_ok music_db

  (** Create / fill internal data. *)
  let create ?(view=Queue_view) client =
    fetch_status client
    >>= function
    | Error message -> Lwt.return_error message
    | Ok status ->
        match view with
        | Help_view -> Lwt.return_ok (Help { status })
        | Queue_view -> fetch_queue_list client
              >>= fun plist -> Lwt.return_ok (Queue { status; plist; selected = 0 })
        | Music_db_view ->
            fetch_music_db client 0 0 0
            >>= function
            | Error message -> Lwt.return_error message
            | Ok  db -> Lwt.return_ok (Music_db { status; db })

  (** Force to update an internal data. *)
  let force_update idata client =
    fetch_status client
    >>= function
    | Error message -> Lwt.return_error message
    | Ok status ->
        match idata with
        | Help _ -> Lwt.return_ok (Help {status})
        | Queue {status = _; plist; selected} ->
            fetch_queue_list client
              >>= fun plist ->
                Lwt.return_ok (Queue {status; plist; selected})
        | Music_db {status = _; db;} ->
            fetch_music_db client db.artist.selected db.album.selected db.song.selected
            >>= function
              | Error message -> Lwt.return_error message
              | Ok db -> Lwt.return_ok (Music_db {status; db})

  (** Update internal data but with checks for elapsed time in order to limit
   *  the number of request send to Mpd. (not every times a key is pressed. *)
  let update idata client =
    let now = Unix.time () in
    match idata with
    | Error message -> Lwt.return_error message
    | Ok internal_data ->
        Mpd.Client_lwt.noidle client
        >>= fun _ ->
          match internal_data with
          | Help {status} -> Lwt.return_ok internal_data
          | Queue {status; plist; selected} ->
              let prev = status.timestamp in
              if ((now -. prev) > 1.0) then
                fetch_queue_list client
                >>= fun plist ->
                  fetch_status client
                  >>= function
                    | Error message -> Lwt.return_error message
                    | Ok status -> Lwt.return_ok (Queue {status; plist; selected})
              else Lwt.return_ok internal_data
          | Music_db {status; db} ->
              let prev = status.timestamp in
              if ((now -. prev) > 1.0) then
                fetch_status client
                >>= function
                  | Error message -> Lwt.return_error message
                  | Ok s ->
                      begin if ((now -. prev) > 2.0) then
                          fetch_music_db client db.artist.selected db.album.selected db.song.selected
                        else Lwt.return_ok db
                      end
                      >>= function
                        | Error message -> Lwt.return_error message
                        | Ok db -> Lwt.return_ok (Music_db {status = s; db})
              else Lwt.return_ok internal_data
end
