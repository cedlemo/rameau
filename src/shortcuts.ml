open Lwt.Infix
open View

let none events client t idata =
  match events with
  | _-> Lwt.return false

let queue events client t idata =
  match events with
  | `Key (`Enter, [])     ->
    Lwt.cancel t; Commands.rameau_play client idata
    >>= fun _ -> Lwt.return true
  | `Key (`ASCII 's', []) ->
    Lwt.cancel t; Commands.rameau_stop client idata
    >>= fun _ -> Lwt.return true
  | `Key (`ASCII 'p', []) ->
    Lwt.cancel t; Commands.rameau_toggle_pause client idata
    >>= fun _ -> Lwt.return true
  | `Key (`ASCII '+', []) ->
    Lwt.cancel t; Commands.rameau_inc_vol client idata
    >>= fun _ -> Lwt.return true
  | `Key (`ASCII '-', []) ->
    Lwt.cancel t; Commands.rameau_decr_vol client idata
    >>= fun _ -> Lwt.return true
  | _ -> Lwt.return false

