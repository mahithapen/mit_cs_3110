(** Commit operations. Creation, modification, and retrieval of commits. *)

open Tree

module type CommitType = sig
  type l
  (** An alias for the tree type. *)

  type t
  (** An alias for the commit type. *)

  val make : string -> string -> string
  (** [make auth mess ] creates a new commit from current branch/time with
      author [auth] and message [mess]. Returns the hash of the commit. *)

  val of_dir : string -> t
  (** [of_dir path] makes a commit rooted at [path] without a parent. To be used
      for testing purposes only. *)

  val parent : t -> t option
  (** if [c] has a parent [parent c] is [Some parent], otherwise [c] is the
      initial commit. *)

  val has_parent : t -> bool
  (** [has_parent c] is true if [c] has a parent, false if [c] is initial
      commit. *)

  val tree : t -> l
  (** [tree c] is the root tree of [c] *)

  val author : t -> string
  (** [author c] is the author of [c] *)

  val date : t -> string
  (** [date c] is the date [c] was made. *)

  val message : t -> string
  (** [message c] is the commit message of [c]. *)

  val summary : t -> string
  (** [summary c] is the summary of [c]. *)

  val hash : t -> string
  (** [hash c] is the hash code of [c] *)

  val last_commit : unit -> t option
  (** [last_commit ()] is last commit on the current branch *)
end

(** Type of a functor to create commit operations using a specific tree type. *)
module type MakeCommit = functor (T : TreeType) -> CommitType with type l = T.t

module MakeRecordCommit : MakeCommit
(** Primary commit module. *)
