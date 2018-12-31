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

(** Module that manage the creation, update of Views. *)
val create:
  ?view:View.desc -> Mpd.Client_lwt.t -> View.shortcuts -> View.render -> (View.t, string) Result.result Lwt.t
(** Create / fill internal data. *)
val force_update:
View.t -> Mpd.Client_lwt.t -> (View.t, string) Result.result Lwt.t
(** Force to update an internal data. *)
val update:
(View.t, string) result -> Mpd.Client_lwt.t -> (View.t, string) Result.result Lwt.t
(** Update internal data but with checks for elapsed time in order to limit
 *  the number of request send to Mpd. (not every times a key is pressed. *)

