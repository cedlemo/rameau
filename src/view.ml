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

type desc = Queue_view | Help_view | Music_db_view

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
            | `Resize of int * int ] -> Mpd.Client_lwt.t ->
            [ `End
            | `Key of Notty.Unescape.key
            | `Mouse of Notty.Unescape.mouse
            | `Mpd_event of (string, string) result
            | `Paste of Notty.Unescape.paste
            | `Resize of int * int ] Lwt.t -> t -> bool Lwt.t

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


