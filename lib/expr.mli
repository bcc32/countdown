open Core

type t [@@deriving sexp_of]

val literal : int -> t
val value   : t   -> int

val all_combinations : t -> t -> t list

include Comparable.S with type t := t
