open Shortcuts
open Lwt.Infix

(** Module created to avoid cyclic dependency because it uses View_manager
 *  Previous state was View_manager open Shortcuts that open Commands that
 *  open View_manager for switching view. Create this module to solve this.
 *  Next problem will be to deal with moving selection that require to use
 *  View_manager.update
 *
 *  Another solution to think of:
 *  dont import the shortcuts module in View_manager
 *  create shortcuts type in a separated file and create function for the
 *  shortcut handlers in another file.
 *  *)

let rameau_switch_view view client t idata =
  (*Lwt.return_ok idata *)
  Lwt.cancel t;
  Mpd.Client_lwt.noidle client
  >>= fun _ ->
  View_manager.create ~view client

let global events client t idata =
  let switch view =
    Lwt.cancel t;
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
    View_manager.create ~view client
    >>= function
    | Error _ -> Lwt.return False
    | Ok idata' -> Lwt.return (WithUpdate idata')
  in
  match events with
  | `Key (`ASCII '0', []) -> switch View.Help_view
  | `Key (`ASCII '1', []) -> switch View.Queue_view
  | `Key (`ASCII '2', []) -> switch View.Music_db_view
  | `End
  | `Key (`Escape, [])
  | `Key (`ASCII 'C', [`Ctrl])
  | `Key (`ASCII 'q', []) ->
    Commands.rameau_quit client
    >>= fun () -> Lwt.return True
  | _ -> Lwt.return False
