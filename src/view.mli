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

type desc = Queue_view | Help_view | Music_db_view
(** Used to represent the type of interface to draw *)

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
               shortcuts: shortcuts;
               render: render
             }
  | Music_db of { status: status;
                  db: music_db_selector ;
                  shortcuts: shortcuts ;
                  render: render;
                }
  | Help of { status: status;
              shortcuts: shortcuts;
              render: render;
            }
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
            | `Resize of int * int ] Lwt.t -> t -> event_handled Lwt.t
and event_handled = False | True | WithUpdate of t
(* Type that describe a rameau event has been handled:
 * false if the function did not respond to the event
 * True if the function responded to the event
 * WithUpdate if the function responded to the event and updated the internal
 * data *)
and render = t -> int * int -> Notty.image Lwt.t
(* type for the function that generate the notty image of a view. *)

val get_status: t -> status
(** Get the status data from the internal data. *)
val get_shortcuts: t -> shortcuts
(** Get the shortcuts handler from the internal data *)
val get_render: t -> render
(** Get the render function associated with the current internal data *)
val render: t -> int * int -> Notty.image Lwt.t
(** Directly call the render function in order to generate the Notty image.*)
val get_selected: t -> int
(** Get the index of the current selected item *)
val get_n_elements: t -> int
(** Get the number of items in the current view *)
val set_selected: (int * int * int) -> t -> t
(** Set the index of the current selected item *)
