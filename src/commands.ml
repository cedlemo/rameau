open Rameau_types
open Lwt.Infix

let rameau_play client status selected =
  Mpd.Client_lwt.noidle client
  >>= fun _ ->
    Mpd.Playback_lwt.play client selected
    >>= fun _ -> Lwt.return_unit

let rameau_stop client status =
  if status.state = Mpd.Status.Play then (
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
      Mpd.Playback_lwt.stop client
      >>= fun _ -> Lwt.return_unit
  )
  else Lwt.return_unit

let rameau_toggle_pause client status =
  Mpd.Client_lwt.noidle client
  >>= fun _ -> (
    if status.state = Mpd.Status.Pause then
      Mpd.Playback_lwt.pause client false
    else
      Mpd.Playback_lwt.pause client true
    )
    >>= fun _ -> Lwt.return_unit

let rameau_inc_vol client status =
  if status.volume < 100 then
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
      Mpd.Playback_options_lwt.setvol client (status.volume + 1)
      >>= fun _ -> Lwt.return_unit
  else Lwt.return_unit

let rameau_decr_vol client status =
  if status.volume > 0 then
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
      Mpd.Playback_options_lwt.setvol client (status.volume - 1)
      >>= fun _ -> Lwt.return_unit
  else Lwt.return_unit

