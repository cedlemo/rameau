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
open View

let none events client t idata =
  match events with
  | _-> Lwt.return View.False

let queue events client ev_mpd idata =
  let move_selection keep_in_bounds =
    Lwt.cancel ev_mpd;
    let selected = get_selected idata in
    let pl_len = get_n_elements idata in
    let sel = keep_in_bounds selected pl_len in
    let d = set_selected (sel, 0, 0) idata in
    View_manager.update (Ok d) client
    >>= function
    | Error _ -> Lwt.return False
    | Ok idata' -> Lwt.return (View.WithUpdate idata')
  in
  match events with
  | `Key (`Enter, [])     ->
    Lwt.cancel ev_mpd; Commands.rameau_play client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII 's', []) ->
    Lwt.cancel ev_mpd; Commands.rameau_stop client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII 'p', []) ->
    Lwt.cancel ev_mpd; Commands.rameau_toggle_pause client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII '+', []) ->
    Lwt.cancel ev_mpd; Commands.rameau_inc_vol client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII '-', []) ->
    Lwt.cancel ev_mpd; Commands.rameau_decr_vol client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII 'j', []) ->
    move_selection (fun s l -> if s + 1 >= l then 0 else s + 1)
  | `Key (`ASCII 'k', []) ->
    move_selection (fun s l -> if s - 1 < 0 then l - 1 else s - 1)
  | _ -> Lwt.return View.False

let global events client t idata =
  let switch view shortcuts render =
    Lwt.cancel t;
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
    View_manager.create ~view client shortcuts render
    >>= function
    | Error _ -> Lwt.return View.False
    | Ok idata' -> Lwt.return (View.WithUpdate idata')
  in
  match events with
  | `Key (`ASCII '0', []) -> switch View.Help_view none Drawing.help
  | `Key (`ASCII '1', []) -> switch View.Queue_view queue Drawing.queue
  | `Key (`ASCII '2', []) -> switch View.Music_db_view none Drawing.music_db
  | `End
  | `Key (`Escape, [])
  | `Key (`ASCII 'C', [`Ctrl])
  | `Key (`ASCII 'q', []) ->
    Commands.rameau_quit client
    >>= fun () -> Lwt.return View.True
  | _ -> Lwt.return View.False
