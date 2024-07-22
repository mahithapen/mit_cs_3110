open Blob
open Exception
module StringSet = Set.Make (String)

module type TreeType = sig
  type l
  type t

  val empty : t
  val of_dir : string -> t
  val read_from_commit : string -> string -> t
  val write_to_commit : string -> t -> unit
  val update_dir : string -> t -> unit
  val foldername : t -> string
  val write_tree : t -> string
  val list_of_blobs : t -> (l * string * string) list
  val list_of_files : t -> string list
  val hash : t -> string
  val issame : t -> t -> bool
  val diff : t -> t -> string list
  val all_differences : t -> t -> string list
end

module type MakeTree = functor (B : BlobType) -> TreeType with type l = B.t

module MakeListTree : MakeTree =
functor
  (B : BlobType)
  ->
  struct
    (** [s1 // s2] concatenates [s1] and [s2] as paths. *)
    let ( // ) = Mitnames.( // )

    type l = B.t

    type t = string * tree_or_blob list

    and tree_or_blob =
      | Tree of t
      | Blob of l

    let empty = ("", [])

    let foldername tr =
      let f, _ = tr in
      f

    (** [name_of_tob thing] is the file/foldername of [thing]. *)
    let name_of_tob (thing : tree_or_blob) : string =
      match thing with
      | Tree tr -> foldername tr
      | Blob bl -> B.filename bl

    let rec of_dir path =
      let parse_tree_or_blob bname =
        let path = Filename.concat path bname in
        if Sys.is_directory path then Tree (of_dir path)
        else Blob (B.of_file path)
      in
      match Reader.read_dir path with
      | None -> raise (DirectoryNotFound path)
      | Some lst ->
          let lst =
            List.filter (fun thing -> not (Mitnames.in_mitignore thing)) lst
          in
          let bname = path |> Filename.basename in
          (bname, List.map parse_tree_or_blob lst)

    let rec read_from_commit (path : string) (h : string) : t =
      let p = path // h in
      match Reader.read_file_to_list p with
      | "tree" :: name :: t ->
          let lst =
            List.fold_right (fun h acc -> read_tree_or_blob path h :: acc) t []
          in
          (name, lst)
      | _ -> raise (FileInvalidFormat "Expected Tree and folder name")

    (** [read_tree_of_blob p h] is the tree or blob with hash [h] in the commit
        folder at [p]. *)
    and read_tree_or_blob (path : string) (h : string) : tree_or_blob =
      let p = path // h in
      match Reader.read_first_line p with
      | "tree" -> Tree (read_from_commit path h)
      | "blob" -> Blob (B.read_from_commit path h)
      | _ -> raise (FileInvalidFormat "Expected blob or tree.")

    let rec hash (tr : t) : string =
      let h = foldername tr |> Hash.of_string in
      let _, lst = tr in
      List.fold_left (fun acc thing -> Hash.rehash (hash_tob thing) acc) h lst
      |> Hash.to_hex

    (** [hash_blob thing] is hash of [thing]. *)
    and hash_tob (thing : tree_or_blob) : string =
      match thing with
      | Blob b -> B.hash b
      | Tree tr -> hash tr

    let rec write_to_commit (folder : string) (tr : t) : unit =
      let _, lst = tr in
      let filename = hash tr in
      let tree_info =
        String.concat "\n"
          ("tree" :: foldername tr :: List.map (fun thing -> hash_tob thing) lst)
      in
      Writer.write_to_file (folder // filename) tree_info;
      List.fold_left
        (fun () thing -> write_commit_tree_or_blob folder thing)
        () lst

    (** [write_commit_tree_or_blob p thing] is the writes [thing] to the commit
        folder at [p]. *)
    and write_commit_tree_or_blob (folder : string) (thing : tree_or_blob) :
        unit =
      match thing with
      | Tree tr -> write_to_commit folder tr
      | Blob bl -> B.write_to_commit folder bl

    let rec update_dir (dir : string) (tr : t) =
      let _, tob_future_lst = tr in
      let future_lst = List.map (fun x -> name_of_tob x) tob_future_lst in
      let current_lst = Reader.read_dir dir |> Option.get in
      let removes =
        List.filter
          (fun x -> not (List.mem x (future_lst @ Mitnames.mitignore ())))
          current_lst
      in
      List.iter (fun x -> Writer.rm (dir // x)) removes;
      List.iter (fun x -> update_dir_tob dir x) tob_future_lst

    (** [update_dir_tob p thing] updates the directory at [dir] according to
        [thing]. *)
    and update_dir_tob (dir : string) (thing : tree_or_blob) =
      let path = dir // name_of_tob thing in
      match thing with
      | Tree tr ->
          Writer.mkdir path;
          update_dir path tr
      | Blob bl -> B.update_dir path bl

    (** [write_tree_aux tr d] write tree [tr] keeping track of depth [d].*)
    let rec write_tree_aux tr depth =
      let name, lst = tr in
      let f (acc : string) (b : tree_or_blob) =
        let s =
          let tab =
            if depth > 0 then String.make (2 * depth) ' ' ^ "* " else ""
          in
          match b with
          | Tree tr -> tab ^ foldername tr ^ "\n" ^ write_tree_aux tr (depth + 1)
          | Blob b -> tab ^ B.filename b
        in
        let newline = if acc = "" then "" else "\n" in
        acc ^ newline ^ s
      in
      List.fold_left f "" lst

    let rec write_tree tr = write_tree_aux tr 0

    let rec list_of_blobs : t -> (l * string * string) list =
     fun (dir, blobs_and_trees) ->
      let blob_list =
        List.fold_left
          (fun acc elem ->
            match elem with
            | Blob blob -> (blob, B.content blob, B.filename blob) :: acc
            | Tree subtree -> list_of_blobs subtree @ acc)
          [] blobs_and_trees
      in
      List.rev blob_list

    (** [list_of_files_helper prefix] is a helper function to list files. *)
    let rec list_of_files_helper prefix = function
      | _, [] -> [] (* Empty tree, no paths to generate *)
      | dir, blobs_and_trees ->
          let dir_path = if prefix = "" then dir else prefix ^ "/" ^ dir in
          let blob_paths =
            List.map
              (function
                | Blob blob -> [ "test/" ^ dir_path ^ "/" ^ B.filename blob ]
                | Tree subtree -> list_of_files_helper dir_path subtree)
              blobs_and_trees
          in
          List.flatten blob_paths

    let list_of_files (tr : t) =
      let final_list = list_of_files_helper "" tr in

      final_list

    (** [split_lines str] splits the lines of str. *)
    let split_lines str =
      let rec aux acc i =
        if i >= String.length str then List.rev acc
        else
          try
            let next_i = String.index_from str i '\n' in
            let line = String.sub str i (next_i - i) in
            aux (line :: acc) (next_i + 1)
          with Not_found ->
            List.rev (String.sub str i (String.length str - i) :: acc)
      in
      aux [] 0

    let rec issame (tr1 : t) (tr2 : t) : bool =
      let name1, lst1 = tr1 in
      let name2, lst2 = tr2 in

      (* Compare names of the trees *)
      let compare_names () = name1 = name2 in

      (* Compare the tree_or_blob lists *)
      let rec compare_list l1 l2 =
        match (l1, l2) with
        | [], [] -> true
        | Tree t1 :: tl1, Tree t2 :: tl2 -> issame t1 t2 && compare_list tl1 tl2
        | Blob b1 :: tl1, Blob b2 :: tl2 -> B.same b1 b2 && compare_list tl1 tl2
        | _, _ -> false
      in

      (* Combine name and list comparisons *)
      compare_names () && compare_list lst1 lst2

    let diff tr1 tr2 : 'a list =
      let split_lines str = split_lines str in
      let set_of_list lst = List.fold_right StringSet.add lst StringSet.empty in

      let first = write_tree tr1 |> split_lines |> set_of_list in
      let second = write_tree tr2 |> split_lines |> set_of_list in

      let added = StringSet.diff second first in
      let deleted = StringSet.diff first second in

      let format_line prefix line =
        let item_type =
          if String.contains line '*' then "file " else "directory "
        in
        prefix ^ item_type ^ line
      in

      let format_set prefix set =
        StringSet.fold (fun line acc -> format_line prefix line :: acc) set []
        |> List.rev
      in

      let added_list = format_set "Added " added in
      let deleted_list = format_set "Deleted " deleted in

      if List.length (added_list @ deleted_list) > 0 then
        added_list @ deleted_list
      else [ "" ]

    module Hashtable = Hashtbl.Make (struct
      type t = string

      let equal = String.equal
      let hash = Hashtbl.hash
    end)

    let all_differences (tr1 : t) (tr2 : t) : string list =
      let build_hashtable (tr : t) =
        let ht = Hashtable.create 10 in
        let rec build_aux lst prefix =
          List.iter
            (function
              | Tree (name, sub) -> build_aux sub (prefix ^ name ^ "/")
              | Blob b -> Hashtable.add ht (prefix ^ B.filename b) b)
            lst
        in
        let _, lst = tr in
        build_aux lst "";
        ht
      in

      let ht1 = build_hashtable tr1 in
      let ht2 = build_hashtable tr2 in

      let modifications, deletions =
        Hashtable.fold
          (fun name blob (mods, dels) ->
            match Hashtable.find_opt ht2 name with
            | Some blob2 -> (B.diff blob blob2 @ mods, dels)
            | None -> (mods, ("Deleted " ^ name) :: dels))
          ht1 ([], [])
      in

      let additions =
        Hashtable.fold
          (fun name _ acc ->
            if Hashtable.mem ht1 name then acc else ("Added " ^ name) :: acc)
          ht2 []
      in

      modifications @ deletions @ additions
  end
