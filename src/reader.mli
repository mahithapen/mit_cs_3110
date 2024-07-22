(** Reading operations. *)

val is_dir : string -> bool
(** [is_dir d] verifies is [d] is an existsing directory or not. *)

val join : string -> string list -> string
(** [join delim lst] is the string by joining [lst] with [delim] as a delimiter. *)

val string_list_to_contents : string list -> string
(** [string_list_to_contents lst] puts the lines of a file back into a single
    string. *)

val prepend : (int -> string) -> string list -> string list
(** [prepend f lst] is the list of [lst] prepending some string via [f] that
    takes the index of the element. *)

val read_file : string -> string
(** string of the content of [file_name] with line number annotations as single
    string. If file was not found returns "ERROR: File '[filename]' not found."
    If file was empty returns "ERROR: Empty File" *)

val read_file_to_list : string -> string list
(** string of the content of [file_name] without annotations as lines For
    example, [[line1_content, line2_content, ...]]. If file was not found or is
    empty, returns [[]]. *)

val read_first_line : string -> string
(** [read_first_line path] is the first line of that file at [path]. Raises:
    [FileInvalidFormat] if file is empty.*)

val read_dir : string -> string list option
(** [read_dir d] is list of files and subdirectories in directory [d].
    Postcondition: returns list directory first, then files in alphabetical
    order If [d] is not a directory, then returns None. *)
