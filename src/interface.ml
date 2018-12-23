open Lwt.Infix
open Notty
open Notty_lwt
open Types
open Types.Internal_data

module Terminal = Notty_lwt.Term
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

let rec loop term (e, t) dim client idata =
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
    | Error _ -> loop term (event term, t) dim client idata
    | Ok d -> (
      Lwt.cancel t;
      command client d
      >>= fun () ->
        loop term (new_events ()) dim client idata
    )
  in
  let switch_view view =
    match idata with
    | Error _ -> loop term (event term, t) dim client idata
    | Ok d ->
        Lwt.cancel t;
        Mpd.Client_lwt.noidle client
        >>= fun _ ->
          Internal_data.create ~view client
          >>= fun idata' ->
            render_and_loop term (new_events ()) idata' dim client
  in
  let move_selection keep_in_bounds =
    match idata with
    | Error _ -> loop term (event term, t) dim client idata
    | Ok data ->
        Lwt.cancel t;
        let selected = get_selected data in
        let pl_len = get_n_elements data in
        let sel = keep_in_bounds selected pl_len in
        let d = set_selected (sel, 0, 0) data in (* TODO : deal with 3 selectors *)
        Internal_data.update (Ok d) client
        >>= fun idata' ->
          render_and_loop term (new_events ()) idata' dim client
  in
  (e <?> t) >>= function
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl])
  | `Key (`ASCII 'q', []) -> Mpd.Client_lwt.close client
  | `Mpd_event event_name -> begin
    begin match idata with
          | Error _ -> Internal_data.create client
          | Ok d -> Internal_data.force_update d client
    end
    >>= fun idata' ->
      let events = (e, listen_mpd_event client) in
      render_and_loop term events idata' dim client
  end
  | `Resize dim -> begin
    Lwt.cancel t;
    Internal_data.update idata client
      >>= fun idata' ->
        render_and_loop term (new_events ()) idata' dim client
  end
  | `Key (`ASCII 'j', []) ->
    move_selection (fun s l -> if s + 1 >= l then 0 else s + 1)
  | `Key (`ASCII 'k', []) ->
    move_selection (fun s l -> if s - 1 < 0 then l - 1 else s - 1)
  | `Key (`Enter, [])     -> wrap_command Commands.rameau_play
  | `Key (`ASCII 's', []) -> wrap_command Commands.rameau_stop
  | `Key (`ASCII 'p', []) -> wrap_command Commands.rameau_toggle_pause
  | `Key (`ASCII '+', []) -> wrap_command Commands.rameau_inc_vol
  | `Key (`ASCII '-', []) -> wrap_command Commands.rameau_decr_vol
  | `Key (`ASCII '0', []) -> switch_view Internal_data.Help_view
  | `Key (`ASCII '1', []) -> switch_view Internal_data.Queue_view
  | `Key (`ASCII '2', []) -> switch_view Internal_data.Music_db_view
  | _ -> render_and_loop term (event term, t) idata dim client

let interface client =
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
