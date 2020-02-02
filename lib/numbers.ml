open! Core

(* Calculate all possible results from the given numbers. *)
let project ns =
  let len = List.length ns in
  let max_mask = 1 lsl len in
  let candidates = Array.init max_mask ~f:(fun _ -> Int.Table.create ()) in
  List.iteri ns ~f:(fun i n ->
    let mask = 1 lsl i in
    Hashtbl.set candidates.(mask) ~key:n ~data:(Expr.literal n));
  (* We iterate through all possible masks in increasing order. This is a
     topologically sorted order since any mask [a] which is a subset of mask [b]
     has the property [a < b]. *)
  for target_mask = 0 to max_mask - 1 do
    (* [mask_a] and [mask_b] are disjoint submasks of [target_mask] *)
    for mask_a = 0 to target_mask do
      if mask_a land lnot target_mask = 0
      then (
        let mask_b = target_mask lxor mask_a in
        Hashtbl.iter candidates.(mask_a) ~f:(fun expr_a ->
          Hashtbl.iter candidates.(mask_b) ~f:(fun expr_b ->
            let new_exprs = Expr.all_combinations expr_a expr_b in
            List.iter new_exprs ~f:(fun expr ->
              Hashtbl.set candidates.(target_mask) ~key:(Expr.value expr) ~data:expr))))
    done
  done;
  candidates
;;

let find ns n =
  let tables = project ns in
  (* maximize numbers used *)
  Array.rev_inplace tables;
  match Array.find_map tables ~f:(fun table -> Hashtbl.find table n) with
  | Some expr -> printf !"%{sexp: Expr.t}\n" expr
  | None -> printf "No solution\n"
;;

let main_loop () =
  let rec loop () =
    printf "numbers> %!";
    match In_channel.(input_line stdin) with
    | None -> ()
    | Some numbers ->
      let numbers = String.split numbers ~on:' ' |> List.map ~f:Int.of_string in
      printf "target> %!";
      (match In_channel.(input_line stdin) with
       | None -> ()
       | Some target ->
         let target = Int.of_string target in
         find numbers target;
         loop ())
  in
  loop ()
;;

let command =
  let open Command.Let_syntax in
  Command.basic ~summary:"attempt to construct the target number" (return main_loop)
;;
