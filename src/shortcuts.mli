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

(** Module that regroups all the key event handlers, the functions associated
    with the views in order to bind actions with keys. *)

val none: View.shortcuts
(** key events handler that does nothing *)
val queue: View.shortcuts
(** key events handler associated to the Queue view *)
val global: View.shortcuts
(** key events handler associated with the global interface.
    Those bindings are available il all views*)
