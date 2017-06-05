open Core

type t =
  { chars : char list
  ; children : t Int.Map.t
  ; values : String.Set.t
  }

let create chars =
  { chars
  ; children = Int.Map.empty
  ; values = String.Set.empty
  }
;;

let rec add t string =
  match t.chars with
  | [] -> { t with values = Set.add t.values string }
  | hd :: tl ->
    let n = String.count string ~f:(Char.equal hd) in
    { t with
      children =
        Map.update t.children n ~f:(
          function
          | None -> add (create tl) string
          | Some t -> add t string
        )
    }
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
        Sequence.append acc (to_sequence t string)
      )
;;

let fold t string ~init ~f = to_sequence t string |> Sequence.fold ~init ~f

let iter t string ~f = to_sequence t string |> Sequence.iter ~f
