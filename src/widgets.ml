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
open Notty.Infix
open Lwt

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
  let vdiff = h - lh in let hdiff = w - lw in
  match vdiff with
  | 0 -> I.void (hdiff / 2) 1 <|> label <|> I.void (hdiff / 2) 1
  | 1 ->grid [ [I.void w 1];
               [I.void (hdiff / 2) 1; label; I.void (hdiff / 2) 1];
             ]
  | _ -> grid [ [I.void w (vdiff / 2)];
                [I.void (hdiff / 2) 1; label; I.void (hdiff / 2) 1];
                [I.void w (vdiff / 2)]
              ]

(** Create an expanded_label with red blinking text on black *)
let error_panel str (w, h) =
  expanded_label str A.(fg red ++ bg lightblack ++ st blink) (w, h)

(** Create a box with horizontal line built with line_builder with padding
  (top, right, bottom, and left)*)
let simple_list_box data_list line_builder padding =
  let img = data_list |> List.map line_builder |> I.vcat in
  let (tp, rp, bp, lp) = padding in
  I.pad ~l:lp ~r:rp ~t:tp ~b:bp img

let list_box playlist_labels =
  let rec compute_max_cols p (l1, l2, l3) =
    match p with
    | [] -> (l1, l2, l3)
    | h :: q -> let (s1, s2, s3) = h in
      let l'1 = let ls1 = String.length s1 in if ls1 > l1 then ls1 else l1 in
      let l'2 = let ls2 = String.length s2 in if ls2 > l2 then ls2 else l2 in
      let l'3 = let ls3 = String.length s3 in if ls3 > l3 then ls3 else l3 in
      compute_max_cols q (l'1, l'2, l'3) in
  let (c1, c2, c3) = compute_max_cols playlist_labels (0, 0, 0) in
  playlist_labels |> List.map (fun (id_str, title_str, artist_str) ->
    let attr = A.(fg lightwhite) in
    let id = expanded_label id_str attr (c1, 1) in
    let title = expanded_label title_str attr (c2, 1) in
    let artist = expanded_label artist_str attr (c3, 1) in
    id <|> title <|> artist ) |> I.vcat

