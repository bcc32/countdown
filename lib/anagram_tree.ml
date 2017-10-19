open Core

type t =
  { chars    : char list
  ; children : t Int.Map.t sexp_opaque
  ; values   : String.Set.t }
[@@deriving sexp]

let create chars =
  { chars
  ; children = Int.Map.empty
  ; values   = String.Set.empty }
;;

let rec invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    assert (Option.is_none (List.find_a_dup t.chars ~compare:Char.compare));
    Map.iter t.children ~f:(fun u ->
      invariant u;
      assert (phys_equal u.chars (List.tl_exn t.chars)));
    (* TODO encode this in the type *)
    assert (Map.is_empty t.children || Set.is_empty t.values))

let rec add t string =
  match t.chars with
  | [] -> { t with values = Set.add t.values string }
  | hd :: tl ->
    let n = String.count string ~f:(Char.equal hd) in
    { t with
      children =
        Map.update t.children n ~f:(function
          | None -> add (create tl) string
          | Some t -> add t string) }
;;

let rec to_sequence t string =
  match t.chars with
  | [] -> Set.to_sequence t.values
  | hd :: _ ->
    let n = String.count string ~f:(Char.equal hd) in
    Map.fold_range_inclusive t.children
      ~min:0
      ~max:n
      ~init:Sequence.empty
      ~f:(fun ~key:_ ~data:t acc ->
        Sequence.append acc (to_sequence t string))
;;
