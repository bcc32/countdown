open Core

(* First field is value. *)
type t =
  | Base   of int
  | Plus   of int * t * t
  | Minus  of int * t * t
  | Times  of int * t * t
  | Divide of int * t * t

let rec sexp_of_t =
  function
  | Base n -> [%sexp (n : int)]
  | Plus   (_, a, b) -> [%sexp [(a : t); "+"; (b : t)]]
  | Minus  (_, a, b) -> [%sexp [(a : t); "-"; (b : t)]]
  | Times  (_, a, b) -> [%sexp [(a : t); "*"; (b : t)]]
  | Divide (_, a, b) -> [%sexp [(a : t); "/"; (b : t)]]
;;

let to_int = function
  | Base    v
  | Plus   (v, _, _)
  | Minus  (v, _, _)
  | Times  (v, _, _)
  | Divide (v, _, _) -> v
;;

(* Compare two [t]'s by value *)
let compare = Comparable.lift Int.compare ~f:to_int

let of_int int = Base int

let plus   a b = Plus   (to_int a + to_int b, a, b)
let minus  a b = Minus  (to_int a - to_int b, a, b)
let times  a b = Times  (to_int a * to_int b, a, b)
let divide a b = Divide (to_int a / to_int b, a, b)

let abs_diff a b = if to_int a > to_int b then minus a b else minus b a

let can_divide a b =
  let a = to_int a in
  let b = to_int b in
  b <> 0 && a % b = 0

let all_combinations a b =
  [ plus a b
  ; abs_diff a b
  ; times a b ]
  @ (if can_divide a b then [divide a b] else [])
  @ (if can_divide b a then [divide b a] else [])
  |> List.dedup ~compare
;;

include Comparable.Make(struct
    type nonrec t = t
    let sexp_of_t = sexp_of_t
    let t_of_sexp = [%of_sexp: opaque]
    let compare = compare
  end)
