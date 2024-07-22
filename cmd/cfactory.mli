(** Produces and links all commands and parameters in mit. *)

val tbl : (string, Command.Command.t) Hashtbl.t
(** [tbl] maps command names to Command data structures. *)
