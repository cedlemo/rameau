type status = {
  timestamp : float;
  state : Mpd.Status.state;
  volume : int;
  queue : Mpd.Queue_lwt.t;
  song : int;
}


