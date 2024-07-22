(** Blob stores content of file *)

open Reader
open Hash
open Exception

module type BlobType = sig
  (** Blob stores content of file *)

  type t

  val empty : t
  val of_file : string -> t
  val read_from_commit : string -> string -> t
  val write_to_commit : string -> t -> unit
  val update_dir : string -> t -> unit
  val filename : t -> string
  val content : t -> string
  val hash : t -> string
  val same : t -> t -> bool
  val diff : t -> t -> string list
end

module StringBlob : BlobType = struct
  (*Create a blob with string types *)
  type t = string * string list
  (** AF: [(name, [s1; ...; sn])] represent blob with name [name] and n lines of
      strings s1, s2, ..., sn. RI: None *)

  let empty = ("", [])
  let ( // ) = Mitnames.( // )

  (** base name of file at [path], otherwise [""] Requires: file at file path
      exists *)
  let of_file (path : string) =
    if Sys.file_exists path then
      (Filename.basename path, Reader.read_file_to_list path)
    else empty

  let read_from_commit (path : string) (h : string) =
    match Reader.read_file_to_list (path // h) with
    | "blob" :: name :: t -> (name, t)
    | _ -> raise (FileInvalidFormat "Expected Blob with file name")

  let filename (b : t) =
    let f, _ = b in
    f

  let content (b : t) =
    let _, c = b in
    Reader.join "\n" c

  let hash (b : t) =
    filename b |> Hash.of_string |> Hash.rehash (content b) |> Hash.to_hex

  let write_to_commit (folder : string) (b : t) =
    let name = hash b in
    let blob_info = "blob\n" ^ filename b ^ "\n" ^ content b in
    Writer.write_to_file (folder // name) blob_info

  let update_dir p b = Writer.write_to_file p (content b)

  let same (b1 : t) (b2 : t) : bool =
    let name1, content1 = b1 in
    let name2, content2 = b2 in
    let compare_filenames () = name1 = name2 in
    let compare_contents () =
      let is_same_length = List.length content1 = List.length content2 in
      if not is_same_length then false
      else List.for_all2 (fun line1 line2 -> line1 = line2) content1 content2
    in
    compare_filenames () && compare_contents ()

  let diff cfile nfile : 'a list =
    let split_lines s = String.split_on_char '\n' s in
    let c_content = split_lines (content cfile) in
    let n_content = split_lines (content nfile) in
    let rec compare_lines acc c_lines n_lines i =
      match (c_lines, n_lines) with
      | [], [] -> List.rev acc
      | c :: cs, n :: ns when c = n -> compare_lines acc cs ns (i + 1)
      | c :: cs, n :: ns ->
          let mod_line =
            Printf.sprintf "Modified at line %d: '%s' -> '%s'" i c n
          in
          compare_lines (mod_line :: acc) cs ns (i + 1)
      | c :: cs, [] ->
          let del_line = Printf.sprintf "Deleted at line %d: '%s'" i c in
          compare_lines (del_line :: acc) cs [] (i + 1)
      | [], n :: ns ->
          let add_line = Printf.sprintf "Added at line %d: '%s'" i n in
          compare_lines (add_line :: acc) [] ns (i + 1)
    in

    if List.length (compare_lines [] c_content n_content 1) > 0 then
      compare_lines [] c_content n_content 1
    else [ "No changes made" ]
end
