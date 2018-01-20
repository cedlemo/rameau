open Mpd
open Lwt.Infix

module Internal_data = struct
  type view = Queue | Help    (** Use to represent the type of interface to draw *)
  type t = {
    timestamp : float;        (** Used to limit the number of request to Mpd ie: one each sec *)
    state : Mpd.Status.state; (** Mpd state Play, Stop, Pause *)
    volume : int;             (** Mpd volume *)
    queue : Mpd.Queue_lwt.t;  (** The Mpd Queue to request. *)
    song : int;               (** The current song. *)
    view : view;              (** The current view Rameau is displaying. *)
  }

  (** Used to get the internal data *)
  let fetch ?view:(view=Queue) client =
    Mpd.Client_lwt.status client
    >>= fun response ->
      match response with
      | Error message -> Lwt.return (Error message)
      | Ok d ->
          let timestamp = Unix.time () in
          let state = Mpd.Status.state d in
          let volume = Mpd.Status.volume d in
          let song = Mpd.Status.song d in
          Mpd.Queue_lwt.playlist client
          >>= fun queue ->
            Lwt.return (Ok {timestamp; state; volume; queue; song; view})

  let update idata client =
    match idata with
    | Error _ -> Lwt.return idata
    | Ok d -> Mpd.Client_lwt.noidle client
        >>= fun () ->
          let now = Unix.time () in
          if ((now -. d.timestamp) > 1.0) then fetch ~view:d.view client
          else Lwt.return idata
end

