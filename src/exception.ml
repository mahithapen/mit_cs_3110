(** Mit exceptions. *)

exception FileNotFound of string
exception FileInvalidFormat of string
exception DirectoryNotFound of string
exception UnreachableStatement of string
exception NoCommit of string
exception BranchDoesNotExist of string
