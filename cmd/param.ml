exception AnonymousParamsHaveNoFlags
exception MissingArguments of string
exception FlagDoesNotMatch
exception FlagNeedsFlag

module type ParamType = sig
  type t
  type v

  val make : string option -> string -> t
  val val_opt : t -> v option
  val optional : t -> bool
  val consume : t -> string list -> string list
  val help : t -> string
  val description : t -> string
end

module type Value = sig
  type t

  val name : string
  val value_of_string : string -> t
end

module MakeSingleParam =
functor
  (V : Value)
  ->
  (
    struct
      (*Accept values that are passed as parameters here*)
      type v = V.t

      type t = {
        flag : string option;
        description : string;
        mutable value : v option;
      }

      let make s_opt s = { flag = s_opt; description = s; value = None }
      let val_opt p = p.value

      let optional p =
        match p.flag with
        | None -> false
        | Some _ -> true

      (** [matches_flag p s] is true if the flag of [p] matches [s]. Requires:
          [p] is optional.*)
      let matches_flag p s =
        match p.flag with
        | None -> raise AnonymousParamsHaveNoFlags
        | Some fl -> s = "-" ^ fl || s = "--" ^ fl

      (** [consume_value p s] stores the value from a string *)
      let consume_value p s = p.value <- Some (V.value_of_string s)

      (** [consume p args] is the tail of [args] after [p] tries to match
          against the beginning of [args] and tries to consume.*)
      let consume p args =
        match (p.flag, args) with
        | Some fl, fh :: ah :: t when matches_flag p fh ->
            consume_value p ah;
            t
        | Some fl, _ -> args
        | None, h :: t ->
            consume_value p h;
            t
        | None, _ -> raise (MissingArguments "expected 1 argument")

      let help p =
        let prepend, append =
          match p.flag with
          | None -> ("", "")
          | Some fl -> ("[--" ^ fl ^ " ", "]")
        in
        prepend ^ "<" ^ V.name ^ ">" ^ append

      let description p = p.description
    end :
      ParamType with type v = V.t)

module StringValue : Value with type t = string = struct
  type t = string

  let name = "string"
  let value_of_string s = s
end

module StringParam : ParamType with type v = string =
  MakeSingleParam (StringValue)

module Flag : ParamType with type v = bool = struct
  type v = bool

  type t = {
    flag : string;
    description : string;
    mutable value : v option;
  }

  let make s_opt s =
    match s_opt with
    | None -> raise FlagNeedsFlag
    | Some fl -> { flag = fl; description = s; value = Some false }

  let val_opt p = p.value
  let optional p = true

  (** [matches_flag p s] is true if the flag of [p] matches against [s], and
      false otherwise. *)
  let matches_flag p s =
    match p.flag with
    | fl -> s = "-" ^ fl || s = "--" ^ fl

  (** [set_true p] sets the value of [p] to true.*)
  let set_true p = p.value <- Some true

  (** [consume p args] is the tail of [args] after [p] tries to match and
      consume the beginning of [args]. *)
  let consume p args =
    match args with
    | fh :: t when matches_flag p fh ->
        set_true p;
        t
    | _ ->
        p.value <- Some false;
        args

  let help p = "[--" ^ p.flag ^ "]"
  let description p = p.description
end
