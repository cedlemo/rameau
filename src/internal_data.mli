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
open Notty_lwt
module MDA = Mpd_data_access

type view = Queue_view | Help_view | Music_db_view
(** Used to represent the type of interface to draw *)

type status =
  { timestamp: float;        (** Used to limit the number of request to Mpd ie: one each sec *)
    state: Mpd.Status.state; (** Mpd state Play, Stop, Pause *)
    volume: int;             (** Mpd volume *)
    song: int;               (** The current song. *)
  }

type panel = { items: string list; selected: int}
type music_db_selector = { artist: panel; album: panel; song: panel }

type shortcuts = [ `Key of Unescape.key
                 | `Mouse of Unescape.mouse
                 | `Paste of Unescape.paste
                 | `Resize of int * int ] Lwt_stream.t -> unit
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

val get_status: t -> status
(** Get the status data from the internal data. *)
val get_selected: t -> int
(** Get the index of the current selected item *)
val get_n_elements: t -> int
(** Get the number of items in the current view *)
val set_selected: (int * int * int) -> t -> t
(** Set the index of the current selected item *)
val create:
  ?view:view -> Mpd.Client_lwt.t -> (t, string) Result.result Lwt.t
(** Create / fill internal data. *)
val force_update:
t -> Mpd.Client_lwt.t -> (t, string) Result.result Lwt.t
(** Force to update an internal data. *)
val update:
(t, string) result -> Mpd.Client_lwt.t -> (t, string) Result.result Lwt.t
(** Update internal data but with checks for elapsed time in order to limit
 *  the number of request send to Mpd. (not every times a key is pressed. *)

