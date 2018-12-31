open Lwt.Infix
open View

let none events client t idata =
  match events with
  | _-> Lwt.return View.False

let queue events client t idata =
  match events with
  | `Key (`Enter, [])     ->
    Lwt.cancel t; Commands.rameau_play client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII 's', []) ->
    Lwt.cancel t; Commands.rameau_stop client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII 'p', []) ->
    Lwt.cancel t; Commands.rameau_toggle_pause client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII '+', []) ->
    Lwt.cancel t; Commands.rameau_inc_vol client idata
    >>= fun _ -> Lwt.return View.True
  | `Key (`ASCII '-', []) ->
    Lwt.cancel t; Commands.rameau_decr_vol client idata
    >>= fun _ -> Lwt.return View.True
  | _ -> Lwt.return View.False

let global events client t idata =
  let switch view shortcuts =
    Lwt.cancel t;
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
    View_manager.create ~view client shortcuts
    >>= function
    | Error _ -> Lwt.return View.False
    | Ok idata' -> Lwt.return (View.WithUpdate idata')
  in
  match events with
  | `Key (`ASCII '0', []) -> switch View.Help_view none
  | `Key (`ASCII '1', []) -> switch View.Queue_view queue
  | `Key (`ASCII '2', []) -> switch View.Music_db_view none
  | `End
  | `Key (`Escape, [])
  | `Key (`ASCII 'C', [`Ctrl])
  | `Key (`ASCII 'q', []) ->
    Commands.rameau_quit client
    >>= fun () -> Lwt.return View.True
  | _ -> Lwt.return View.False
