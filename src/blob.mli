(** Blob operations. *)

module type BlobType = sig
  (** Module storing blob operations. *)

  type t
  (** An alias for the type of blobs. *)

  val empty : t
  (** [empty] is an empty blob. *)

  val of_file : string -> t
  (** [of_file p] is blob of the file at path [p]. *)

  val read_from_commit : string -> string -> t
  (** [read_from_commit p h] is the blob read from the commit file at path [p]
      with hash [h]. Requires: directory and blob file exists. *)

  val write_to_commit : string -> t -> unit
  (** [write_to_commit f b] writes a commit blob file of [b] to the commit
      folder at [f]. *)

  val update_dir : string -> t -> unit
  (** [update_dir p b] writes a blob at the directory at [p]*)

  val filename : t -> string
  (** [filename b] is the file name of the blob. *)

  val content : t -> string
  (** [content b] is the string of the blob's content. *)

  val hash : t -> string
  (** [hash b] is the hash of [b]. *)

  val same : t -> t -> bool
  (** [same b1 b2] is true if [b1] and [b2] are the same content-wise, and false
      otherwise. *)

  val diff : t -> t -> string list
  (** [diff b1 b2] is a list of differences (modifications, additions,
      deletions) per line between blobs [b1] and [b2]. *)
end

module StringBlob : BlobType
(** A direct-string blob representation. *)
