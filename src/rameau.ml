(*
 * Copyright 2018 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of rameau.
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
 * along with rameau.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Lwt.Infix

let run host port =
  let open Mpd in
  let main_thread =
    Loggin.setup ()
    >>= fun () ->
      Mpd.Connection_lwt.initialize host port
      >>= fun connection ->
        Mpd.Client_lwt.initialize connection
        >>= fun client ->
          Interface.create client
  in
  Lwt_main.run (
    Lwt.catch
      (fun () -> main_thread)
      (function
        | Mpd.Connection_lwt.Lwt_unix_exn message ->
            Lwt_io.write_line Lwt_io.stderr message
        | _ -> Lwt_io.write_line Lwt_io.stderr "Exception not handled. Exit ..."
      )
  )

let () =
  (run "127.0.0.1" 6600)
