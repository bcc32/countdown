open Core

(** The type of Countdown mathematical expressions.

    Allowed operations are [+], [-], [*], and [/], and only non-negative
    integers are allowed as intermediate results. *)
type t [@@deriving sexp_of]

(** [literal n] returns an expression which is just the integer literal [n]. *)
val literal : int -> t

val value : t -> int

(** [all_combinations a b] returns a list of allowed operations on [a] and [b],
    excluding any duplications of the same resulting value. *)
val all_combinations : t -> t -> t list

(** Expressions compare by value, so two expressions whose results are the same
    but whose structures are different compare equal. *)
include Comparable.S with type t := t

include Invariant.S with type t := t
