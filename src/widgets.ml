(*
 * Copyright 2017 Cedric LE MOIGNE, cedlemo@gmx.com
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

(** Assemble a list of list of Notty.image in a grid image *)
let grid xxs = xxs |> List.map I.hcat |> I.vcat

(** Organize ascii code for decorations of the same kind *)
type ascii_corners = { tl: Notty.image; (** top left *)
                       tr: Notty.image; (** top right *)
                       bl: Notty.image; (** bottom left *)
                       br: Notty.image  (** bottom right *)
                     }
(** Create a label widget that expand in the given size with the text centered
 in it
 TODO : check that label size is not bigger than the given size *)
let expanded_label str attr (w, h as size) =
  let label = I.string attr str in
  let (lw, lh) = (I.width label, I.height label) in
  grid [ [I.void w ((h - lh) / 2)];
         [I.void ((w - lw) / 2) 1; label; I.void ((w - lw) / 2) 1];
         [I.void w ((h - lh) / 2)]
       ]
(** Create an expanded_label with red blinking text on black *)
let error_panel str (w, h) =
  expanded_label str A.(fg red ++ bg lightblack ++ st blink) (w, h)

(** Create a box with horizontal line built with line_builder with padding
  (top, right, bottom, and left)*)
let list_box data_list line_builder padding =
  let img = data_list |> List.map line_builder |> I.vcat in
  let (tp, rp, bp, lp) = padding in
  I.pad ~l:lp ~r:rp ~t:tp ~b:bp img
