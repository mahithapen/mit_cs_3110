module Bl = Blob.StringBlob
module Tr = Tree.MakeListTree (Bl)
module Com = Commit.MakeRecordCommit (Tr)
open Exception

(** [p1 // p2] concatenates the file paths [p1] and [p2] together. *)
let ( // ) = Mitnames.( // )

(** [is_repo_initialized ()] is true if it hash been initialized, false if not. *)
let is_repo_initialized () : bool =
  Sys.file_exists Mitnames.mit_root && Sys.is_directory Mitnames.mit_root

(** [is_repo_empty ()] is true if repo is empty *)
let is_repo_empty () : bool =
  match Com.last_commit () with
  | None -> true
  | Some _ -> false

let status () : unit =
  if is_repo_empty () then "No commits made" |> print_endline
  else
    let current_c = Com.of_dir "." in
    let last_c = Com.last_commit () |> Option.get in
    let diffs = Tr.all_differences (Com.tree last_c) (Com.tree current_c) in
    if diffs = [] then print_endline "No changes detected."
    else diffs |> String.concat "\n" |> print_endline

let log () : unit =
  if is_repo_empty () then "No commits made" |> print_endline
  else
    let c = Com.last_commit () in
    let rec print_commit_info commit =
      print_endline (Com.summary commit);
      if Com.has_parent commit then
        print_commit_info (commit |> Com.parent |> Option.get)
      else print_endline "END OF LOG"
    in
    print_commit_info (Option.get c)

let change () : unit =
  let c = Com.last_commit () in
  if c <> None then
    if Com.has_parent (Option.get c) then
      let x =
        Tr.all_differences
          (Com.tree (Option.get (Com.parent (Option.get c))))
          (Com.tree (Option.get c))
      in
      let x_filtered = List.filter (fun s -> s <> "No changes made") x in
      print_endline (String.concat "\n" x_filtered)

let init () : unit =
  if is_repo_initialized () then print_endline "Mit repo already initialized"
  else (
    Unix.mkdir Mitnames.mit_root 0o755;
    Unix.mkdir Mitnames.branch 0o755;
    print_endline ".mit folder generated";
    Writer.write_to_file Mitnames.head "")

let clean () : unit = Writer.rm Mitnames.mit_root

let commit auth mess : unit =
  let h = Com.make auth mess in
  if is_repo_empty () then Writer.write_to_file Mitnames.head Mitnames.main
  else ();
  let b = Mitnames.current_branch () |> Option.get in
  Writer.write_to_file (Mitnames.branch // b) h

let branch branch_name : unit =
  if is_repo_empty () then
    "Cannot create branch in empty repo." |> print_endline
  else
    match branch_name with
    | "" -> print_endline (Branch.get_branch ())
    | s ->
        Branch.create_new_branch s;
        print_endline (Branch.get_branch ())

let checkout branch_name b : unit =
  if is_repo_empty () then
    "Cannot checkout to branch in empty repo." |> print_endline
  else if b then branch branch_name
  else ();
  Branch.checkout branch_name
