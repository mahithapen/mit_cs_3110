open Params
module P = Params

exception InvalidArguments
exception ExtraneousArguments of string

module type CommandType = sig
  type t
  type params = Params.t

  val name : t -> string
  val make : string -> string -> params list -> (params list -> unit) -> t
  val execute : t -> string list -> unit
  val help : t -> string
  val summary : t -> string
  val description : t -> string
end

module Command : CommandType with type params = ptype = struct
  (*Match Commands from the CFactory module to here *)
  type params = ptype
  type v = pvalue

  type t = {
    name : string;
    ordered : params list;
    required : params list;
    optional : params list;
    help : params;
    command : params list -> unit;
    description : string;
  }

  let name (c : t) = c.name

  let make n desc plst cmd =
    let h = make_help_param () in
    let o, r = List.partition P.optional plst in
    {
      name = n;
      ordered = plst;
      required = r;
      optional = o;
      help = h;
      command = cmd;
      description = desc;
    }

  let help p =
    List.fold_left
      (fun acc p -> acc ^ " " ^ P.help p)
      (name p) (p.required @ p.optional)

  let summary c = c.description

  let description p =
    List.fold_left
      (fun acc p -> acc ^ "\n\t" ^ P.help p ^ " : " ^ P.description p)
      (p.name ^ " : " ^ p.description)
      (p.required @ p.optional)

  (** parses arg list into all arguments. Follows specification of [execute]. *)
  let parse c args : unit =
    let g args popt = P.consume popt args in
    let f args preq = List.fold_left g args c.optional |> P.consume preq in
    let tail = List.fold_left f (List.fold_left g args c.optional) c.required in
    match tail with
    | [] -> ()
    | t -> raise (ExtraneousArguments (String.concat "; " t))

  (** scans args for a single argument *)
  let rec parse_single p args : unit =
    match args with
    | [] -> ()
    | h :: t ->
        ignore (P.consume p args);
        parse_single p t

  let execute c args : unit =
    parse_single c.help args;
    if Params.extract_bool c.help then description c |> print_endline
    else (
      parse c args;
      c.command c.ordered)
end
