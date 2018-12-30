open Lwt.Infix
open View

type event_handled = False | True | WithUpdate of View.t
(* Type that describe a rameau event has been handled:
 * false if the function did not respond to the event
 * True if the function responded to the event
 * WithUpdate if the function responded to the event and updated the internal
 * data *)

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


