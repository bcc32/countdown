(** An anagram tree represents a set of strings whose elements contain *at most*
    a certain number of each character. This data structure is useful for
    determining what words can be constructed from a given set of letters.

    @see <https://en.wikipedia.org/wiki/Anatree> for a similar design. *)

open! Core

type t [@@deriving sexp]

include Invariant.S with type t := t

(** Create an anagram tree.

    @param alphabet a list of valid characters for words in this tree *)
val create : char list -> t

(** Add a word to the tree.

    @param t an anagram tree

    @param word a word consisting of characters from the alphabet

    @return a new tree including [word] *)
val add : t -> string -> t

(** Find (sub-)anagrams of the input word.

    @param t an anagram tree

    @param letters the available letters from which to construct words

    @return a sequence of all strings in the tree that can be constructed using
    only the characters [letters]. *)
val to_sequence : t -> string -> string Sequence.t
