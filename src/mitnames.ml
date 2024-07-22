(** Mit folder structure and naming conventions. *)

let ( // ) s1 s2 = Filename.concat s1 s2

(** [mit_root] is the relative root of mit folder. *)
let mit_root = ".mit"

(** [commit_info] is the name of commit info file. *)
let commit_info = "commit_info.txt"

(** [project_files] is the name of directory for project files. *)
let project_files = "project_files"

(** [branch] is the relative directory to access branches. *)
let branch = mit_root // "branches"

(** [head] is the path to access the head file. *)
let head = mit_root // "HEAD"

(** [main] is the name of the first, primary branch. *)
let main = "main"

(** [mitignore ()] is a list of folder and file names for mit to ignore. *)
let mitignore () = [ ".mit"; ".DS_Store" ]

(** [in_mitignore s] is true is [s] is supposed to be ignored, and false
    otherwise. *)
let in_mitignore s = List.mem s (mitignore ())

(** [current branch()] is [Some s] if the currently active branch has name [s].
    Otherwise returns [None]. *)
let current_branch () : string option =
  match Reader.read_first_line head with
  | "" -> None
  | s -> Some s

(** [hash_of_branch b] is [Some s] the most recent commit on branch [b] has hash
    [s]. Otherwise returns [None]. *)
let hash_of_branch b : string option =
  match Reader.read_first_line (branch // b) with
  | "" -> None
  | s -> Some s
