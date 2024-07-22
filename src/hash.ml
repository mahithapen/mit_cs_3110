type t =
  | HString of string
  | HDigest of Digest.t

(** [to_digest h] converts hash [h] to something digestible. *)
let to_digest (h : t) : Digest.t =
  match h with
  | HString s -> Digest.string s
  | HDigest h -> h

let compare h1 h2 = Digest.compare (to_digest h1) (to_digest h2)
let equal h1 h2 = Digest.equal (to_digest h1) (to_digest h2)
let from_hex s : t = HDigest (Digest.from_hex s)
let to_hex h : string = h |> to_digest |> Digest.to_hex
let of_string s : t = HString s

let rehash s h =
  let p = to_hex h in
  HString (p ^ s ^ p)
