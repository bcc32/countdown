(** A one-sided trie represents a set of strings whose elements contain *at
    most* a certain number of each character. This data structure is useful for
    determining what words can be constructed from a given set of letters. *)

open Core

type t

(** Create a one-sided trie over the given alphabet *)
val create : char list -> t

val add : t -> string -> t

(** Sequence of all strings in the one-sided trie that can be constructed using
    the characters in the input string *)
val to_sequence : t -> string -> string Sequence.t

(** [fold], [iter], etc. produce elements in the same order as [to_sequence] *)

val fold : t -> string -> init:'acc -> f:('acc -> string -> 'acc) -> 'acc

val iter : t -> string -> f:(string -> unit) -> unit
