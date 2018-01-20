open Types.Internal_data
open Lwt.Infix

let rameau_play client idata selected =
  Mpd.Client_lwt.noidle client
  >>= fun _ ->
    Mpd.Playback_lwt.play client selected
    >>= fun _ -> Lwt.return_unit

let rameau_stop client idata =
  if idata.state = Mpd.Status.Play then (
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
      Mpd.Playback_lwt.stop client
      >>= fun _ -> Lwt.return_unit
  )
  else Lwt.return_unit

let rameau_toggle_pause client idata =
  Mpd.Client_lwt.noidle client
  >>= fun _ -> (
    if idata.state = Mpd.Status.Pause then
      Mpd.Playback_lwt.pause client false
    else
      Mpd.Playback_lwt.pause client true
    )
    >>= fun _ -> Lwt.return_unit

let rameau_inc_vol client idata =
  if idata.volume < 100 then
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
      Mpd.Playback_options_lwt.setvol client (idata.volume + 1)
      >>= fun _ -> Lwt.return_unit
  else Lwt.return_unit

let rameau_decr_vol client idata =
  if idata.volume > 0 then
    Mpd.Client_lwt.noidle client
    >>= fun _ ->
      Mpd.Playback_options_lwt.setvol client (idata.volume - 1)
      >>= fun _ -> Lwt.return_unit
  else Lwt.return_unit

