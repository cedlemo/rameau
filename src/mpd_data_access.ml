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

module Song = struct
  (** module dedicated to use / encapsulate the Mpd.Song type. *)
  type t = {
    title : string;
    artist : string;
    album : string;
    time : int;
    track : string;
  }

  let from_mpd_song s =
    {
      title = Mpd.Song.title s;
      artist = Mpd.Song.artist s;
      album = Mpd.Song.album s;
      time = Mpd.Song.time s;
      track = Mpd.Song.track s;
    }
end

(** Module with functions permit to get mpd data when client is given as parameter
 *  or stubs when no client is present *)
module Mpd_or_stubs : sig
  val fetch_status:
    ?client:(Mpd.Client_lwt.t) -> unit ->  (float * Mpd.Status.state * int * int, string) result Lwt.t
  val fetch_queue_list:
    ?client:(Mpd.Client_lwt.t) -> unit -> (Song.t list, string) result Lwt.t
  val fetch_artists_in_music_db:
    ?client:(Mpd.Client_lwt.t) -> unit -> (string list, string) result Lwt.t
  val fetch_albums_in_music_db:
    ?client:(Mpd.Client_lwt.t) -> string -> (string list, string) result Lwt.t
  val fetch_songs_in_music_db:
    ?client:(Mpd.Client_lwt.t) -> string -> string -> (string list, string) result Lwt.t
end = struct

  let build_random_stub prefix =
    let () = Random.self_init () in
    let n = Random.int 15 in
    let rec build_stub i acc =
      if i > n then acc
      else
        let s = String.concat " " [prefix; string_of_int i] in
        build_stub (i + 1) (s :: acc)
    in
    if false then Lwt.return_error "nope" (* trick for the stub to return a Result Lwt.t. *)
    else Lwt.return_ok (build_stub 0 [])

  let _build_random_song title_n artist_n album_n time_n track : Song.t=
    {
      title = "title" ^ (string_of_int title_n);
      artist = "artist" ^ (string_of_int artist_n);
      album = "album" ^ (string_of_int album_n);
      time = 120 + time_n;
      track = track;
    }

  let fetch_queue_list ?client () =
    match client with
    | Some client -> begin
        Mpd.Queue_lwt.playlist client
        >>= function
        | Mpd.Queue_lwt.PlaylistError message -> Lwt.return_error message
        | Mpd.Queue_lwt.Playlist p ->
          let songs =
            List.map (fun s : Song.t -> { title = Mpd.Song.title s;
                                          artist = Mpd.Song.artist s;
                                          album = Mpd.Song.album s;
                                          time = int_of_float (Mpd.Song.duration s); (* TOFIX *)
                                          track = Mpd.Song.track s; }) p
          in
          Lwt.return_ok songs
      end
    | None -> Lwt.return_ok [({ title = "title1"; artist = "artist1";
                                album = "album1"; time = 120; track = "1"; } : Song.t);
                             { title = "title2"; artist = "artist2";
                               album = "album2"; time = 180; track = "2"; }]

  let fetch_artists_in_music_db ?client () =
    match client with
    | Some client  -> Mpd.Music_database_lwt.list client Mpd.Music_database_lwt.Artist []
    | None -> build_random_stub "Artist"

  let fetch_albums_in_music_db ?client artist =
    match client with
    | Some client -> Mpd.Music_database_lwt.(list client Album [(Artist, artist)])
    | None -> build_random_stub "Album"

  let fetch_songs_in_music_db ?client artist album =
    match client with
    | Some client ->
      Mpd.Music_database_lwt.(list client Title [(Artist, artist); (Album, album)])
    | None -> build_random_stub "Song"

  let fetch_status ?client () =
    match client with
    | Some client -> begin
        Mpd.Client_lwt.status client
        >>= function
        | Error message -> Lwt.return_error message
        | Ok d ->
          let timestamp = Unix.time () in
          let state = Mpd.Status.state d in
          let volume = Mpd.Status.volume d in
          let song = Mpd.Status.song d in
          Lwt.return_ok (timestamp, state, volume, song)
      end
    | None  -> Lwt.return_ok (3.0, Mpd.Status.Play, 97, 2)
end
