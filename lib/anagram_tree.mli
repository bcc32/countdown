(** An anagram tree represents a set of strings whose elements contain *at most*
    a certain number of each character. This data structure is useful for
    determining what words can be constructed from a given set of letters.

    See https://en.wikipedia.org/wiki/Anatree for a similar design. *)

open Core

type t [@@deriving sexp]

include Invariant.S with type t := t

(** Create an anagram tree over the given alphabet *)
val create : char list -> t

val add : t -> string -> t

(** Sequence of all strings in the tree that can be constructed using the
    characters in the input string *)
val to_sequence : t -> string -> string Sequence.t
