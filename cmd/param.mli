(** Individual parameter operations. *)

exception AnonymousParamsHaveNoFlags
exception MissingArguments of string
exception FlagDoesNotMatch
exception FlagNeedsFlag

module type ParamType = sig
  (** Module for operations on parameter types. *)

  type t
  (** An alias for the parameter type. *)

  type v
  (** An alias for the type of the value. *)

  val make : string option -> string -> t
  (** [make sw_opt desc] is a parameter with described by [desc]. If
      [sw_opt = None], then it is required. If [sw_opt = Some switch], then it
      is optional and is denoted with by [switch]. *)

  val val_opt : t -> v option
  (** [val_opt p] is [None] if no value is assigned to [p], and [Some v] if [v]
      was assigned to [p]. *)

  val optional : t -> bool
  (** [optional p] is true if [p] is optional, and false otherwise. *)

  val consume : t -> string list -> string list
  (** [consume c args] consumes value and switch of [c] from the beginning of
      [t], if possible, and returns the tail. If [c] is optional, and the switch
      is not identified, then the original list is returned. Raises:
      MissingArgument if required arguments are missing. *)

  val help : t -> string
  (** [help p] is a single line descripion of syntax of [p]. For example, a
      required string parameter will option ["<string>"], an optional string
      parameter outputs ["[--file <string>]"], and a boolean flag outputs
      ["[--skip-video]"]. *)

  val description : t -> string
  (** [description p] is an english description of the useage of [p] within the
      larger context of the command. *)
end

module StringParam : ParamType with type v = string
(** A single string parameter. *)

module Flag : ParamType with type v = bool
(** A boolean flag.*)
