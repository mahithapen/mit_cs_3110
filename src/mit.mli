(** Mit operations. All commands. *)

val init : unit -> unit
(** [init ()] initializes a mit folder in the current directory, if one does not
    already exist. *)

val clean : unit -> unit
(** [clean ()] removes the mit folder. *)

val status : unit -> unit
(** [status ()] prints the changes in the current directory compared to the most
    recent commit.*)

val change : unit -> unit
(** [change ()] prints the changes between the previous two commits. *)

val log : unit -> unit
(** [log ()] prints the log of commit histories starting at the current commit. *)

val commit : string -> string -> unit
(** [commit a m] makes a commit of the current directory with the current time,
    author [a], and message [m]. *)

val branch : string -> unit
(** [branch name] lists all current branches. If [name] is not "", then creates
    a branch of [name] if it doesn't already exist. *)

val checkout : string -> bool -> unit
(** [checkout name b] switches the head to branch [name]. If [b] is true, then
    creates branch [name] if it does not already exist. *)
