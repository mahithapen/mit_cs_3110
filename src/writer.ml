(** Writer Module Desc *)

open Reader
open Exception

let rec get_file_body file_name channel curr_line =
  try
    let file_contents = input_line channel in
    (* "Line " ^ string_of_int curr_line ^ ": " ^ *)
    let extra_line = if curr_line = 1 then "" else "\n" in
    extra_line ^ file_contents ^ get_file_body file_name channel (curr_line + 1)
  with End_of_file ->
    close_in channel;
    ""

let read_file_body (file_name : string) : string =
  try
    let channel = open_in file_name in
    get_file_body file_name channel 1
  with
  | Sys_error _ -> "ERROR: File '" ^ file_name ^ "' not found."
  | End_of_file -> "ERROR: Empty File"

(** append_to_file appends data to existing file contents *)
let append_to_file (file_name : string) (data : string) =
  try
    let curr_contents = read_file_body file_name in
    let output_channel = open_out file_name in
    Printf.fprintf output_channel "%s" (curr_contents ^ "\n" ^ data);
    close_out output_channel
  with Sys_error ex -> raise (DirectoryNotFound ex)

(** write_to_file overwrites all stuff in the file with new data *)
let write_to_file (file_path : string) (data : string) =
  try
    let output_channel = open_out file_path in
    Printf.fprintf output_channel "%s" (data ^ "\n");
    close_out output_channel
  with Sys_error ex -> raise (DirectoryNotFound ex)

let mkdir dir = if Sys.file_exists dir then () else Unix.mkdir dir 0o755

let rec rm dir =
  if Sys.file_exists dir then
    if Sys.is_directory dir then (
      let contents = Array.to_list (Sys.readdir dir) in
      List.iter
        (fun file ->
          let file_path = Filename.concat dir file in
          if Sys.is_directory file_path then rm file_path
          else Sys.remove file_path)
        contents;
      Unix.rmdir dir)
    else Unix.unlink dir
  else ()
