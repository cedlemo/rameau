let file_exists f = try ignore(Unix.stat f); true with _ -> false

(* Warning: can throws Not_found error *)
let get_config_dirs () =
  let home = Sys.getenv "HOME" in
  let config = Printf.sprintf "%s/.config" home in
  (home, config, Printf.sprintf "%s/rameau" config)

let generate_config_dir () =
  try
    let (_, config, path) = get_config_dirs () in
    let flags = 0o755 in
    let () = if not (file_exists config) then Unix.mkdir config flags in
    let () = if not (file_exists path) then Unix.mkdir path flags in
    Lwt.return_nil
  with
  | Not_found -> Lwt.fail_with "Unable to get the HOME env variable"
  | Unix.Unix_error (e, _, _) ->
    let message = Unix.error_message e in Lwt.fail_with message


