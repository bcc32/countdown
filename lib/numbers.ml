open Core

(* Calculate all possible results from the given numbers. *)
let project ns =
  let len = List.length ns in
  let max_mask = 1 lsl len in
  let candidates = Array.init max_mask (fun _ -> Int.Table.create ()) in
  List.iteri ns ~f:(fun i n ->
    let mask = 1 lsl i in
    Hashtbl.set candidates.(mask) ~key:n ~data:(Expr.literal n));
  for mask = 0 to max_mask - 1 do
    Hashtbl.iter candidates.(mask) ~f:(fun expr ->
      List.iteri ns ~f:(fun i n ->
        if mask land (1 lsl i) = 0 then begin
          let new_mask = mask lor (1 lsl i) in
          let new_exprs = Expr.all_combinations expr (Expr.literal n) in
          List.iter new_exprs ~f:(fun expr ->
            Hashtbl.set candidates.(new_mask)
              ~key:(Expr.value expr)
              ~data:expr)
        end))
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
      match In_channel.(input_line stdin) with
      | None -> ()
      | Some target ->
        let target = Int.of_string target in
        find numbers target;
        loop ()
  in
  loop ()
;;

let command =
  Command.basic' ~summary:"attempt to construct the target number" begin
    let open Command.Let_syntax in
    return main_loop
  end
;;
