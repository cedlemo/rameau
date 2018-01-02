(*
 * Copyright 2017 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of OCaml-libmpdclient.
 *
 * OCaml-libmpdclient is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * OCaml-libmpdclient is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-libmpdclient.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Lwt.Infix
open Notty
open Notty_lwt

module Terminal = Notty_lwt.Term

type status = {
  timestamp : float;
  state : Mpd.Status.state;
  volume : int;
  queue : Mpd.Queue_lwt.t;
  song : int;
}

let fetch_status client =
  Mpd.Client_lwt.status client
  >>= fun response ->
    match response with
    | Error message -> Lwt.return (Error message)
    | Ok s ->
        let timestamp = Unix.time () in
        let state = Mpd.Status.state s in
        let volume = Mpd.Status.volume s in
        let song = Mpd.Status.song s in
        Mpd.Queue_lwt.playlist client
        >>= fun queue ->
          Lwt.return (Ok {timestamp; state; volume; queue; song})

let update_status status client =
  match status with
  | Error _ -> Lwt.return status
  | Ok s -> Mpd.Client_lwt.noidle client
      >>= fun () ->
        let now = Unix.time () in
        if ((now -. s.timestamp) > 1.0) then fetch_status client
        else Lwt.return status

let gen_state_img status =
  let state_img = match status.state with
    | Mpd.Status.Play -> I.(string A.(fg green) "play")
    | Mpd.Status.Pause -> I.(string A.(fg lightblack) "Pause")
    | Mpd.Status.Stop -> I.(string A.(fg black ++ bg lightblack) "Stop")
    | Mpd.Status.ErrState -> I.(string A.(fg red) "State Error")
  in
  I.(string A.(fg white) "[state ] : " <|> state_img)

let gen_volume_img status =
  I.(strf ~attr:A.(fg white)   "[volume] : %d" status.volume)

let gen_playlist_img selected status (w, h) =
  match status.queue with
  | PlaylistError message -> Lwt.return I.(strf ~attr:A.(fg red) "Error: %s" message)
  | Playlist songs ->
    let gen_song_img i song =
      let title = Mpd.Song.title song in
      let artist = Mpd.Song.artist song in
      if selected = i then
        I.(strf ~attr:A.(fg red ++ bg black) "+ %s : %s" title artist)
      else if status.song = i then
          I.(strf ~attr:A.(fg lightred ++ bg lightblack) "+ %s : %s" title artist)
        else
          I.(strf ~attr:A.(fg lightblack) "- %s : %s" title artist)
    in
    let song_imgs = List.mapi gen_song_img songs in
    let lines = List.map (fun i ->
      let left_margin = 4 in
      let i_w = I.width i in
      let remain = let r = w - (i_w + left_margin) in (max r 0) in
      I.hpad left_margin remain i)
      song_imgs in
    Lwt.return I.(vcat lines)

let render status selected (w, h) =
    match status with
    | Error message -> Lwt.return I.(strf ~attr:A.(fg red) "[there is a pb %s]" message)
    | Ok status -> let state_img = gen_state_img status in
      let volume_img = gen_volume_img status in
      gen_playlist_img selected status (w, h)
      >>= fun songs_img ->
      Lwt.return I.(state_img <-> volume_img <-> songs_img)

let listen_mpd_event client =
  Mpd.Client_lwt.idle client >|= fun evt -> `Mpd_event evt

let event term = Lwt_stream.get (Terminal.events term) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let result_status_playlist_length = function
  | Error _ -> -1
  | Ok status -> match status.queue with | Playlist p -> List.length p | _ -> 0

let rameau_play client status selected =
  ignore(Mpd.Playback_lwt.play client selected);
  Lwt.return_unit

let rameau_stop client status =
  if status.state = Mpd.Status.Play then (
    ignore(Mpd.Playback_lwt.stop client));
  Lwt.return_unit

let rec loop term (e, t) dim client status selected =
  (e <?> t) >>= function
  | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) ->
      Mpd.Client_lwt.close client
  | `Mpd_event event_name ->
      fetch_status client
      >>= fun status' ->
        render status' selected dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (e, listen_mpd_event client) dim client status' selected
  | `Resize dim ->
      update_status status client
      >>= fun status' ->
        render status' selected dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (event term, t) dim client status' selected
  | `Key (`ASCII 'j', []) ->
      let pl_len = result_status_playlist_length status in
      let sel = if selected + 1 >= pl_len then 0 else selected + 1
      in update_status status client
      >>= fun status' ->
        render status' selected dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (event term, t) dim client status' sel
  | `Key (`ASCII 'k', []) ->
      let pl_len = result_status_playlist_length status in
      let sel = if selected - 1 < 0 then pl_len - 1 else selected - 1
      in update_status status client
      >>= fun status' ->
        render status' selected dim
        >>= fun img ->
          Terminal.image term img
          >>= fun () ->
            loop term (event term, t) dim client status'  sel
  | `Key (`Enter, []) -> (
      match status with
      | Error _ -> loop term (event term, t) dim client status selected
      | Ok s -> (
          Mpd.Client_lwt.noidle client
          >>= fun () ->
            rameau_play client s selected
            >>= fun () ->
              loop term (event term, t) dim client status selected
      )
  )
  | `Key (`ASCII 's', []) -> (
      match status with
      | Error _ -> loop term (event term, t) dim client status selected
      | Ok s -> (
          Mpd.Client_lwt.noidle client
          >>= fun () ->
            rameau_stop client s
            >>= fun () ->
              loop term (event term, t) dim client status selected
      )
  )
  | _ -> render status selected dim
         >>= fun img ->
           Terminal.image term img
           >>= fun () ->
             loop term (event term, t) dim client status selected

let interface client =
  let term = Terminal.create () in
  let size = Terminal.size term in
  fetch_status client
  >>= fun result_status ->
    render result_status 0 size
    >>= fun img ->
      Terminal.image term img
      >>= fun () ->
        loop term (event term, listen_mpd_event client) size client result_status 0

let run host port =
  let open Mpd in
  let main_thread =
    Mpd.Connection_lwt.initialize host port
    >>= fun connection ->
      Mpd.Client_lwt.initialize connection
      >>= fun client ->
        interface client
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
