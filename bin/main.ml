open Cmd
open Command

(** table of commands*)
let tbl = Cfactory.tbl

let name, args =
  match Array.to_list Sys.argv with
  | [] | _ :: [] -> ("help", [])
  | _ :: n :: t -> (n, t)

let () =
  match Hashtbl.find_opt tbl name with
  | Some c -> Command.execute c args
  | None ->
      "Unrecognized command: " ^ name
      ^ ". Try calling [help] to see a list of all valid commands."
      |> print_endline
