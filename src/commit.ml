open Tree
open Blob
open Exception

module type CommitType = sig
  type l
  type t

  val make : string -> string -> string
  val of_dir : string -> t
  val parent : t -> t option
  val has_parent : t -> bool
  val tree : t -> l
  val author : t -> string
  val date : t -> string
  val message : t -> string
  val summary : t -> string
  val hash : t -> string
  val last_commit : unit -> t option
end

module type MakeCommit = functor (T : TreeType) -> CommitType with type l = T.t

module MakeRecordCommit : MakeCommit =
functor
  (T : TreeType)
  ->
  struct
    (** Helper to concatenate file paths. *)
    let ( // ) = Mitnames.( // )

    type l = T.t

    type t = {
      mutable hash : string option;
      mutable author : string;
      mutable date : string;
      mutable message : string;
      mutable parent_hash : string option;
      mutable parent : t option;
      mutable tree_hash : string option;
      mutable tree : l option;
    }

    (** [empty ()] creates a new empty commit *)
    let empty () =
      {
        hash = None;
        author = "";
        date = "";
        message = "";
        parent_hash = None;
        parent = None;
        tree_hash = None;
        tree = None;
      }

    (** [set_author c a] sets the author of [c] to [a]. *)
    let set_author (c : t) (a : string) = c.author <- a

    (** [set_date c d] sets the date of [c] to [d]. *)
    let set_date (c : t) (d : string) = c.date <- d

    (** [set_message c m] sets the message of [c] to [m]. *)
    let set_message (c : t) (m : string) = c.message <- m

    let author (c : t) : string = c.author
    let date (c : t) : string = c.date
    let message (c : t) : string = c.message
    let has_parent (c : t) : bool = Option.is_some c.parent_hash

    (** [tree_hash c] is the hash of the tree of [c]. Requires: [c] has a tree*)
    let tree_hash (c : t) : string = c.tree_hash |> Option.get

    let rec hash (c : t) : string =
      (match c.hash with
      | None -> set_hash c
      | Some h -> ());
      match c.hash with
      | None -> raise (UnreachableStatement "must be hashed already")
      | Some h -> h

    (** [set_hash c] sets hash of commit. Should be called one-time per commit. *)
    and set_hash c =
      let h =
        author c |> Hash.of_string
        |> Hash.rehash (date c)
        |> Hash.rehash (message c)
        |> Hash.rehash (tree_hash c)
        |> Hash.rehash (Option.value ~default:"" c.parent_hash)
        |> Hash.to_hex
      in
      c.hash <- Some h

    let rec tree (c : t) : l =
      (match (c.tree_hash, c.tree) with
      | None, _ ->
          c.tree_hash <- Some (T.hash T.empty);
          c.tree <- Some T.empty
      | _, None ->
          c.tree <-
            (let path = Mitnames.mit_root // hash c // Mitnames.project_files in
             Some (T.read_from_commit path (tree_hash c)))
      | _ -> ());
      Option.get c.tree

    (** [of_commit_info p] is a commit read from the info file at path [p]. *)
    let of_commit_info (path : string) : t =
      match Reader.read_file_to_list path with
      | auth :: dat :: mess :: par :: tree :: _ ->
          let par_opt =
            match par with
            | "" -> None
            | s -> Some s
          in
          {
            (empty ()) with
            author = auth;
            date = dat;
            message = mess;
            parent_hash = par_opt;
            tree_hash = Some tree;
          }
      | _ -> raise (FileInvalidFormat "Commit info not formatted correctly")

    (** [last_commit_hash ()] is the hash code of the most recent commit *)
    let last_commit_hash () : string option =
      match Reader.read_first_line Mitnames.head with
      | "" -> None
      | b ->
          let s = Mitnames.hash_of_branch b in
          s

    let summary c =
      String.concat "\n"
        [
          "commit " ^ hash c;
          "";
          "Author: " ^ author c;
          "Date: " ^ date c;
          "";
          "\t " ^ message c;
          "";
        ]

    (** [write_commit info c f] writes the commit info file to the mit folder.
        Requires: mit repo has been initialized. *)
    let write_commit_info c folder_path : unit =
      let c_info_path = folder_path // Mitnames.commit_info in
      Writer.mkdir folder_path;
      let commit_info =
        Format.sprintf "%s\n%s\n%s\n%s\n%s" (author c) (date c) (message c)
          (Option.value ~default:"" c.parent_hash)
          (tree_hash c)
      in
      Writer.write_to_file c_info_path commit_info

    (** [generate_files c] writes the project files of [c] to the mit folder.
        Requires: mit repo has been initialized.*)
    let generate_files c : unit =
      let folder_path = Mitnames.mit_root // hash c in
      write_commit_info c folder_path;
      let pj_folder = folder_path // Mitnames.project_files in
      Writer.mkdir pj_folder;
      T.write_to_commit pj_folder (tree c)

    let make auth mess =
      let c = empty () in
      let tr = T.of_dir "." in
      c.author <- auth;
      c.message <- mess;
      c.parent_hash <- last_commit_hash ();
      c.tree <- Some tr;
      c.tree_hash <- Some (T.hash tr);
      c.date <- Time.current_time ();
      generate_files c;
      hash c

    (** [of_commit h] is the commit retrieve from mit via the commit hash [h]. *)
    let of_commit (commit_hash : string) : t =
      let path = Mitnames.mit_root // commit_hash // Mitnames.commit_info in
      of_commit_info path

    let last_commit () : t option =
      match Reader.read_first_line Mitnames.head with
      | "" -> None
      | b -> (
          match Mitnames.hash_of_branch b with
          | None -> None
          | Some h -> Some (of_commit h))

    let parent (c : t) : t option =
      (match (c.parent_hash, c.parent) with
      | Some h, None -> c.parent <- Some (of_commit h)
      | _, _ -> ());
      c.parent

    let of_dir (path : string) =
      let tr = T.of_dir path in
      { (empty ()) with tree_hash = Some (T.hash tr); tree = Some tr }
  end
