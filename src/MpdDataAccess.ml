open Lwt.Infix

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
  Mpd.Queue_lwt.playlist client

let fetch_artists_in_music_db client =
  Mpd.Music_database_lwt.list client Mpd.Music_database_lwt.Artist []
(* Queries to implement
 * list album artist "artist name"
 * list title album "album name" artist "artist name"
 * *)
let fetch_albums_in_music_db client artist =
  Mpd.Music_database_lwt.(list client Album [(Artist, artist)])

let () = Random.self_init ()

let fetch_songs_in_music_db client artist album =
  (* Mpd.Music_database_lwt.(list client Title [(Artist, artist); (Album, album)]) *)
  let n = Random.int 15 in
  let rec build_stub i acc =
    if i > n then acc
    else
      let s = "Song " ^ (string_of_int i) in
      build_stub (i + 1) (s :: acc)
  in
  if n = -1 then Lwt.return_error "nope"
  else let bouchon = build_stub 0 [] in
  Lwt.return_ok bouchon
