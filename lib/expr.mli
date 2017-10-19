(** An expression represents an arithmetic calculation on integers, i.e., steps
    in the calculations in the {e Numbers} round.

    The following operations are permissible: [+], [-], [*], [/]. Only
    non-negative integers are allowed as intermediate results. *)

open Core

(** An expression *)
type t [@@deriving sexp_of]

(** @param n a positive integer

    @return an expression consisting of just the number [n] *)
val literal : int -> t

(** @return the result of evaluating the expression *)
val value : t -> int

(** Find all meaningful combinations of two expressions.

    @param a an expression

    @param b an expression

    @return a list of permissible expressions formed by the application of a
    single binary operator on [a] and [b]. Does {b not} return multiple
    expressions that evaluate to the same value, e.g., [2 + 2] and [2 * 2].
*)
val all_combinations : t -> t -> t list

include Comparable.S with type t := t

(** Expressions compare by value, so two expressions whose results are the same
    but whose structures are different compare equal. *)
val equal : t -> t -> bool

include Invariant.S with type t := t
