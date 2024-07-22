(** Writing operations. *)

val write_to_file : string -> string -> unit
(** [write_to_file p s] writes [s] to a file at path [p]. *)

val append_to_file : string -> string -> unit
(** append_to_file file_path data appends data to the file provided in file_path *)

val read_file_body : string -> string
(** string of the content of [file_name] If file was not found returns "ERROR:
    File '[file_name]' not found." If file was empty returns "ERROR: Empty File" *)

val mkdir : string -> unit
(** [mkdir dir] makes a directory at [dir]. *)

val rm : string -> unit
(** [rm dir] removes a directory or file. *)
