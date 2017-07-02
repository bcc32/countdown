open Core

type t [@@deriving sexp_of]

val of_int : int -> t
val to_int : t -> int

val all_combinations : t -> t -> t list

include Comparable.S with type t := t
