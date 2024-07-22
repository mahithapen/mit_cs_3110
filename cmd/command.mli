(** Command operations. *)

open Params

exception InvalidArguments

module type CommandType = sig
  type t
  (** An alias for the type of commands.*)

  type params = Params.t
  (** An alias for the type of parameters. *)

  val name : t -> string
  (** [name c] is the keyword to call [c]. *)

  val make : string -> string -> params list -> (params list -> unit) -> t
  (** [make name desc plst cmd] is a command called by [name], described by
      [desc], with parameters [plst], and command [cmd]. Requires: [cmd] must
      accept the exact same parameter list as [plst]. *)

  val execute : t -> string list -> unit
  (** [execute c plst] executes [c] on [plst]. Scans for required arguments
      left-to-right. Optional argument can appear at any place and number of
      times, but the rightmost one is the one that is counted. Requires: [plst]
      must be the same as the one used to make [c]. Raises: Exceptions if
      missing or there are extra arguments. *)

  val help : t -> string
  (** [help c] is a single line descripion of syntax of [c]. For example, a
      command called branch with an optional string parameter b will return
      [branch "[-b <string>]"]. *)

  val summary : t -> string
  (** [summary c] is an english description of the action of [c]. *)

  val description : t -> string
  (** [description c] is a parameter-by-parameter description of [c]. *)
end

module Command : CommandType
(** Primary command module. *)
