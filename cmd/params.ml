(** Combined parameter operations.*)

open Param
(** Accept values that are passed as parameters here*)

module SP = StringParam
module FL = Flag

(** parameter types. *)
type ptype =
  | TString of SP.t
  | TFlag of FL.t

(** parameter value types. *)
type pvalue =
  | VString of SP.v
  | VFlag of FL.v
  | VNone

module Params = struct
  type t = ptype
  (** An alias for the type of a combined parameter. *)

  type v = pvalue
  (** An alias for the value of a combined parameter. *)

  (** [make_string_param fl_opt s] make a string parameter just as
      [SP.make fl_opt s]. Value defaults to [""]. *)
  let make_string_param fl_opt s =
    let p = SP.make fl_opt s in
    TString p

  (** [make_flag_param fl s] makes a flag parameter just as
      [FL.make (Some fl) s]. Defaults to [false]. *)
  let make_flag_param fl s =
    let p = FL.make (Some fl) s in
    TFlag p

  (** [val_opt p] follows the same specification as [val_opt] in [ParamType]. *)
  let val_opt p =
    match p with
    | TString sp -> (
        match SP.val_opt sp with
        | Some s -> VString s
        | None -> VNone)
    | TFlag fl -> (
        match FL.val_opt fl with
        | Some b -> VFlag b
        | None -> VNone)

  (** [optional p] follows the same specification as [optional] in [ParamType]. *)
  let optional p =
    match p with
    | TString sp -> SP.optional sp
    | TFlag fl -> FL.optional fl

  (** [consume p args] follows the same specification as [consume] in
      [ParamType]. *)
  let consume p args =
    match p with
    | TString sp -> SP.consume sp args
    | TFlag fl -> FL.consume fl args

  (** [help p] follows the same specification as [help] in [ParamType]. *)
  let help p =
    match p with
    | TString sp -> SP.help sp
    | TFlag fl -> FL.help fl

  (** [description p] follows the same specification as [description] in
      [ParamType].*)
  let description p =
    match p with
    | TString sp -> SP.description sp
    | TFlag fl -> FL.description fl

  (** [extract_string p] is string of [p]. Defaults to [""]. Requires: must be
      of type VString *)
  let extract_string p =
    match val_opt p with
    | VString s -> s
    | VFlag _ -> failwith "Does not return a string"
    | VNone -> ""

  (** [extract_string p] is boolean of [p]. Defaults to [False]. Requires: must
      be of type VString *)
  let extract_bool p =
    match val_opt p with
    | VString _ -> failwith "Does not return a string"
    | VFlag b -> b
    | VNone -> failwith "Impossible to reach with flag"
end

(** [make_help_param ()] is a fresh help parameter, which is a flag. *)
let make_help_param () =
  Params.make_flag_param "help"
    "view a detailed list of the command's arguments"
