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

open View
open Lwt.Infix

let rameau_play client idata =
  Mpd.Client_lwt.noidle client
  >>= fun _ ->
  Mpd.Playback_lwt.play client (get_selected idata)
  >>= fun _ -> Lwt.return_unit

let rameau_stop client idata =
  let status = get_status idata in
  if status.state = Mpd.Status.Play then begin
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
    Mpd.Playback_lwt.stop client
    >>= fun _ -> Lwt.return_unit
  end
  else Lwt.return_unit

let rameau_toggle_pause client idata =
  Mpd.Client_lwt.noidle client
  >>= fun _ -> (
    let status = get_status idata in
    if status.state = Mpd.Status.Pause then
      Mpd.Playback_lwt.pause client false
    else
      Mpd.Playback_lwt.pause client true
  )
  >>= fun _ -> Lwt.return_unit

let rameau_inc_vol client idata =
  let status = get_status idata in
  if status.volume < 100 then
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
    Mpd.Playback_options_lwt.setvol client (status.volume + 1)
    >>= fun _ -> Lwt.return_unit
  else Lwt.return_unit

let rameau_decr_vol client idata =
  let status = get_status idata in
  if status.volume > 0 then
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
    Mpd.Playback_options_lwt.setvol client (status.volume - 1)
    >>= fun _ -> Lwt.return_unit
  else Lwt.return_unit

let rameau_quit client =
  Mpd.Client_lwt.close client
