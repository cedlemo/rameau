open Lwt.Infix

module Song = struct
  (** module dedicated to use encapsulate the Mpd.Song type. *)
  type t = {
    title : string;
    artist : string;
    album : string;
    time : float;
    track : int;
  }

  let from_mpd_song (s : Mpd.Song.t) : t=
    {
      title = s.title;
      artist = s.artist;
      album = s.album;
      time = s.time;
      track = s.track;
    }
    (*
     let title = Mpd.Song.title song in
  let artist = Mpd.Song.artist song in
  let album = Mpd.Song.album song in
  let time = Mpd.Song.duration song in
  let track = Mpd.Song.track song in
 *)
end

(* Initialize random number generator *)
let () = Random.self_init ()

let build_random_stub prefix =
  let n = Random.int 15 in
  let rec build_stub i acc =
    if i > n then acc
    else
      let s = String.concat " " [prefix; string_of_int i] in
      build_stub (i + 1) (s :: acc)
  in
  if false then Lwt.return_error "nope" (* trick for the stub to return a Result Lwt.t. *)
  else Lwt.return_ok (build_stub 0 [])

let build_random_song title_n artist_n album_n time_n track () =
    {
      title = "title" ^ (string_of_int title_n);
      artist = "artist" ^ (string_of_int artist_n);
      album = "album" ^ (string_of_int album_n);
      time = 2.0 +. time_n;
      track = track;
    }

(*
  TODO
  let build_random_song_list () =
  let artist_n = Random.int 5 in
  let title_n = Random.int 10 in
  let album_n = Random.int 3 in
  let time_n () = (Random.float 50.0) /. 100. in
  let rec build i *)
(** Used to get the internal status *)
let fetch_status client =
  Mpd.Client_lwt.status client
  >>= function
    | Error message -> Lwt.return_error message
    | Ok d ->
        let timestamp = Unix.time () in
        let state = Mpd.Status.state d in
        let volume = Mpd.Status.volume d in
        let song = Mpd.Status.song d in
        Lwt.return_ok (timestamp, state, volume, song)

let fetch_queue_list client =
  (* Mpd.Queue_lwt.playlist client *)
  build_random_stub "Playlist: song"

let fetch_artists_in_music_db client =
  (* Mpd.Music_database_lwt.list client Mpd.Music_database_lwt.Artist [] *)
(* Queries to implement
 * list album artist "artist name"
 * list title album "album name" artist "artist name"
 * *)
  build_random_stub "Artist"

let fetch_albums_in_music_db client artist =
  (* Mpd.Music_database_lwt.(list client Album [(Artist, artist)]) *)
  build_random_stub "Album"


let fetch_songs_in_music_db client artist album =
  (* Mpd.Music_database_lwt.(list client Title [(Artist, artist); (Album, album)]) *)
  (* This is some stubs *)
  build_random_stub "Song"
