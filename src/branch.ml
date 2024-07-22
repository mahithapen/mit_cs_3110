(** Branches that are created here with ability to store the commits*)

open Exception
module Bl = Blob.StringBlob
module Tr = Tree.MakeListTree (Bl)
module Com = Commit.MakeRecordCommit (Tr)

(** Helper function to concatenate file paths. *)
let ( // ) = Mitnames.( // )

(** [all_branches ()] is a list of the names of all branches. *)
let all_branches () =
  match Reader.read_dir Mitnames.branch with
  | Some branchlst -> branchlst
  | None -> failwith "Branches not found"

let get_branch () : string =
  let curr_branch = Mitnames.current_branch () |> Option.get in
  let branches_lst = all_branches () in
  let rec mark_branch branches acc =
    match branches with
    | [] -> acc
    | branch :: t ->
        if branch = curr_branch then mark_branch t ("* " ^ branch ^ "\n" ^ acc)
        else mark_branch t (branch ^ "\n" ^ acc)
  in
  mark_branch branches_lst ""

(** [branch_exists b] is true if a branch of name [b] exists or not *)
let branch_exists b : bool = List.mem b (all_branches ())

(** [branch_empty b] is true if [b] is empty, false if it references a commit.
    Requires: [b] must be an existing branch *)
let branch_empty b : bool =
  let p = Mitnames.branch // b in
  match Reader.read_first_line p with
  | "" -> true
  | _ -> false

let create_new_branch branch_name =
  if branch_exists branch_name || branch_name = "" then ()
  else
    let out_channel = open_out (Mitnames.branch ^ "/" ^ branch_name) in
    output_string out_channel "";
    close_out out_channel

(** [link_branch b] links branch of name [b], to most recent commit. Requires:
    [b] must be empty. *)
let link_branch b : unit =
  let p = Mitnames.branch // b in
  let h = Com.last_commit () |> Option.get |> Com.hash in
  Writer.write_to_file p h

(** [update_dir ()] updates local file structure and contents to match last
    commit on the current branch. Requires: repo has at least one commit. *)
let update_dir () =
  let tr = Com.last_commit () |> Option.get |> Com.tree in
  Tr.update_dir "." tr

(** [switch_branch b] makes switche to branch [b]. Requires: [b] is a branch
    that exists. *)
let switch_branch branch_name = Writer.write_to_file Mitnames.head branch_name

let checkout b =
  if not (branch_exists b) then raise (BranchDoesNotExist b) else ();
  if branch_empty b then link_branch b else ();
  switch_branch b;
  update_dir ()
