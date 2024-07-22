(** Branch operations. For creating and switching branches. *)

val get_branch : unit -> string
(** [get_branch ()] is the name of the branch at the head. *)

val create_new_branch : string -> unit
(** [create_new_branch name] creates a new branch called [name]. If [name]
    already exists, it does nothing. *)

val checkout : string -> unit
(** [checkout name] switches current branch to [name] if it exists. Updates
    current directory to reflect change. If [name] is empty, populates with most
    recent commit. Raises: [BranchDoesNotExist] if [name] does not exist. *)
