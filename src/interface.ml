open Lwt.Infix
open Notty
open Notty_lwt
module Terminal = Notty_lwt.Term

open Internal_data
open Widgets

let listen_mpd_event client =
  Mpd.Client_lwt.idle client >|= fun evt -> `Mpd_event evt

let event term = Lwt_stream.get (Terminal.events term) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let result_status_playlist_length = function
  | Error _ -> -1
  | Ok data ->
      match data with
      | Music_db _ | Help _ -> 0
      | Queue {status; plist; selected} -> match plist with
                                           | Ok p -> List.length p
                                           | _ -> 0

let rec loop term (ev_term, ev_mpd) dim client idata =
  let render_and_loop term events idata dim client =
    render idata dim
    >>= fun img ->
      Terminal.image term img
      >>= fun () ->
        loop term events dim client idata
  in
  let new_events () =
    (event term, listen_mpd_event client)
  in
  let wrap_command command =
    match idata with
    | Error _ -> loop term (event term, ev_mpd) dim client idata
    | Ok d -> (
      Lwt.cancel ev_mpd;
      command client d
      >>= fun () ->
        loop term (new_events ()) dim client idata
    )
  in
  let switch_view view =
    match idata with
    | Error _ -> loop term (event term, ev_mpd) dim client idata
    | Ok d ->
        Lwt.cancel ev_mpd;
        Mpd.Client_lwt.noidle client
        >>= fun _ ->
          Internal_data.create ~view client
          >>= fun idata' ->
            render_and_loop term (new_events ()) idata' dim client
  in
  let move_selection keep_in_bounds =
    match idata with
    | Error _ -> loop term (event term, ev_mpd) dim client idata
    | Ok data ->
        Lwt.cancel ev_mpd;
        let selected = get_selected data in
        let pl_len = get_n_elements data in
        let sel = keep_in_bounds selected pl_len in
        let d = set_selected (sel, 0, 0) data in (* TODO : deal with 3 selectors *)
        Internal_data.update (Ok d) client
        >>= fun idata' ->
          render_and_loop term (new_events ()) idata' dim client
  in
  (ev_term <?> ev_mpd) >>= function
  | `Mpd_event event_name -> begin
    begin match idata with
          | Error _ -> Internal_data.create client
          | Ok d -> Internal_data.force_update d client
    end
    >>= fun idata' ->
      let events = (ev_term, listen_mpd_event client) in
      render_and_loop term events idata' dim client
  end
  | `Resize dim -> begin
    Lwt.cancel ev_mpd;
    Internal_data.update idata client
      >>= fun idata' ->
        render_and_loop term (new_events ()) idata' dim client
  end
  | `Key (`ASCII 'j', []) ->
    move_selection (fun s l -> if s + 1 >= l then 0 else s + 1)
  | `Key (`ASCII 'k', []) ->
    move_selection (fun s l -> if s - 1 < 0 then l - 1 else s - 1)
(*  | `Key (`Enter, [])     -> wrap_command Commands.rameau_play
  | `Key (`ASCII 's', []) -> wrap_command Commands.rameau_stop
  | `Key (`ASCII 'p', []) -> wrap_command Commands.rameau_toggle_pause
  | `Key (`ASCII '+', []) -> wrap_command Commands.rameau_inc_vol
  | `Key (`ASCII '-', []) -> wrap_command Commands.rameau_decr_vol *)
  | `Key (`ASCII '0', []) -> switch_view Internal_data.Help_view
  | `Key (`ASCII '1', []) -> switch_view Internal_data.Queue_view
  | `Key (`ASCII '2', []) -> switch_view Internal_data.Music_db_view
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl])
  | `Key (`ASCII 'q', []) -> Commands.rameau_quit client
  | other_keys -> match idata with
    | Error _ -> loop term (event term, ev_mpd) dim client idata
    | Ok idata' ->
    let shortcuts = get_shortcuts idata' in
    shortcuts other_keys  client ev_mpd idata'
    >>= function
    | true ->  loop term (new_events ()) dim client idata
    | false -> render_and_loop term (event term, ev_mpd) idata dim client

let create client =
  let term = Terminal.create () in
  let size = Terminal.size term in
  Internal_data.create client
  >>= fun internal_data ->
    render internal_data size
    >>= fun img ->
      Terminal.image term img
      >>= fun () ->
        let events = event term, listen_mpd_event client in
        loop term events size client internal_data
