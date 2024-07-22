open Exception

let is_dir d = Sys.is_directory d

(** string of the content of given input [channel] without annotations as lines
    For example, [[line1_content, line2_content, ...]]. If file was not found or
    is empty, returns [[]]. Requires: File exists and is non-empty *)
let rec get_file_contents_list channel : string list =
  try
    let file_contents = input_line channel in
    file_contents :: get_file_contents_list channel
  with End_of_file ->
    close_in channel;
    []

let join (delim : string) (lst : string list) =
  let append_line (acc : string) (l : string) =
    match acc with
    | "" -> l
    | x as acc -> acc ^ delim ^ l
  in
  List.fold_left append_line "" lst

let string_list_to_contents (lst : string list) = join "\n" lst

let prepend (prefix : int -> string) (lst : string list) =
  let p i s = prefix (i + 1) ^ s in
  List.mapi p lst

(** string of content of given input [channel] Requires: File exists and is
    non-empty *)
let get_file_contents channel =
  let prefix i = "Line " ^ string_of_int i ^ ": " in
  let lst = get_file_contents_list channel in
  let lst = prepend prefix lst in
  lst @ [ "END OF FILE" ] |> join "\n"
(* "END OF FILE" *)

let read_file (file_name : string) : string =
  try
    let channel = open_in file_name in
    get_file_contents channel
  with
  | Sys_error _ -> "ERROR: File '" ^ file_name ^ "' not found."
  | End_of_file -> "ERROR: Empty File"

let read_file_to_list (file_name : string) : string list =
  try
    let channel = open_in file_name in
    get_file_contents_list channel
  with Sys_error _ | End_of_file -> []

let read_first_line (file_name : string) : string =
  match read_file_to_list file_name with
  | s :: _ -> s
  | _ -> ""

let read_dir d =
  if not (is_dir d) then None
  else
    let is_dir bn = Filename.concat d bn |> Sys.is_directory in
    let l = Sys.readdir d |> Array.to_list in
    let d, f = List.partition is_dir l in
    Some (List.sort String.compare d @ List.sort String.compare f)
