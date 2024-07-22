(** Tree operations. *)

(** TreeType that stores blobs *)
module type TreeType = sig
  type l
  (** An alias for the blob type. *)

  type t
  (** An alias for the tree type. *)

  val empty : t
  (** [empty] is an empty tree. *)

  val of_dir : string -> t
  (** [of_dir d] is the tree rooted at directory [d]. *)

  val read_from_commit : string -> string -> t
  (** [read_tree path tree_hash] is the tree given the tree's hash and project
      directory. *)

  val write_to_commit : string -> t -> unit
  (** [write_commit_tree f tr] writes the tree [tr] to the commit folder [f]. *)

  val update_dir : string -> t -> unit
  (** [update dir dir tr] updates the directory at [dir] with [tr] obj. *)

  val foldername : t -> string
  (** [foldername tr] is the foldername of the root of [tr]. *)

  val write_tree : t -> string
  (** [write_tree tr] is a string representation of the folder structure rooted
      at [tr]. *)

  val list_of_blobs : t -> (l * string * string) list
  (** [list_of_blobs tr] is a list of all (blob, blob_content, blob_filename)
      throughout the entire tree. *)

  val list_of_files : t -> string list
  (** [list_of_files tr] is a list of all file paths rooted at [tr]. *)

  val hash : t -> string
  (** [hash tr] is the hash of [tr]. *)

  val issame : t -> t -> bool
  (** [issame tr1 tr2] is true if [tr1] and [tr2] has the same structure, and
      false otherwise. *)

  val diff : t -> t -> string list
  (** [diff tr1 tr2] is a list of differences (modifications, additions,
      deletions) between [tr1] and [tr2]. *)

  val all_differences : t -> t -> string list
  (** [diff tr1 tr2] is a list of all differences (modifications, additions,
      deletions) between [tr1] and [tr2]. *)
end

(** TreeType that stores blobs of [B.t] *)
module type MakeTree = functor (B : Blob.BlobType) -> TreeType with type l = B.t

module MakeListTree : MakeTree
