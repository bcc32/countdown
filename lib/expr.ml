open! Core

(* First field is value. *)
type t =
  | Base   of int
  | Plus   of int * t * t
  | Minus  of int * t * t
  | Times  of int * t * t
  | Divide of int * t * t

(* TODO add pretty to_string *)

let rec sexp_of_t =
  function
  | Base n -> [%sexp (n : int)]
  | Plus   (_, a, b) -> [%sexp ["+"; (a : t); (b : t)]]
  | Minus  (_, a, b) -> [%sexp ["-"; (a : t); (b : t)]]
  | Times  (_, a, b) -> [%sexp ["*"; (a : t); (b : t)]]
  | Divide (_, a, b) -> [%sexp ["/"; (a : t); (b : t)]]
;;

let value = function
  | Base    v
  | Plus   (v, _, _)
  | Minus  (v, _, _)
  | Times  (v, _, _)
  | Divide (v, _, _) -> v
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    assert (value t >= 0);
    match t with
    | Base _ | Plus _ | Times _ -> ()
    | Minus  (_, a, b) -> assert (value a >= value b)
    | Divide (_, a, b) -> assert (value a % value b = 0))
;;

(* Compare two [t]'s by value *)
let compare = Comparable.lift Int.compare ~f:value

let literal int = Base int

let plus   a b = Plus   (value a + value b, a, b)
let minus  a b = Minus  (value a - value b, a, b)
let times  a b = Times  (value a * value b, a, b)
let divide a b = Divide (value a / value b, a, b)

let abs_diff a b = if value a > value b then minus a b else minus b a

let can_divide a b =
  let a = value a in
  let b = value b in
  b <> 0 && a % b = 0
;;

let all_combinations a b =
  [ plus a b
  ; abs_diff a b
  ; times a b ]
  @ (if can_divide a b then [ divide a b ] else [])
  @ (if can_divide b a then [ divide b a ] else [])
  |> List.dedup_and_sort ~compare
;;

include Comparable.Make (struct
    type nonrec t = t
    let sexp_of_t = sexp_of_t
    let t_of_sexp = opaque_of_sexp
    let compare = compare
  end)
