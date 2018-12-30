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

(* Module that regroups the functions related to rameau functionalities.*)
val rameau_play: Mpd.Client_lwt.t -> View.t -> unit Lwt.t

val rameau_stop: Mpd.Client_lwt.t -> View.t -> unit Lwt.t

val rameau_toggle_pause: Mpd.Client_lwt.t -> View.t -> unit Lwt.t

val rameau_inc_vol: Mpd.Client_lwt.t -> View.t -> unit Lwt.t

val rameau_decr_vol: Mpd.Client_lwt.t -> View.t -> unit Lwt.t

val rameau_quit: Mpd.Client_lwt.t -> unit Lwt.t
