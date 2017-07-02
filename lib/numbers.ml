open Core
open Core_extended

(* Calculate all possible results from the given numbers. *)
let project ns =
  let len = List.length ns in
  let max_mask = 1 lsl len in
  let candidates = Array.init max_mask (fun _ -> Int.Table.create ()) in
  List.iteri ns ~f:(fun i n ->
    let mask = 1 lsl i in
    Hashtbl.set candidates.(mask) ~key:n ~data:(Expr.of_int n));
  for mask = 0 to max_mask - 1 do
    Hashtbl.iter candidates.(mask) ~f:(fun expr ->
      List.iteri ns ~f:(fun i n ->
        if mask land (1 lsl i) = 0 then begin
          let new_mask = mask lor (1 lsl i) in
          let new_exprs = Expr.all_combinations expr (Expr.of_int n) in
          List.iter new_exprs ~f:(fun expr ->
            Hashtbl.set candidates.(new_mask)
              ~key:(Expr.to_int expr)
              ~data:expr)
        end))
  done;
  candidates
;;

let find ns n =
  let tables = project ns in
  (* maximize numbers used *)
  Array.rev_inplace tables;
  Array.find_map tables ~f:(fun table -> Hashtbl.find table n)
  |> function
  | Some expr -> printf !"%{sexp: Expr.t}\n" expr
  | None -> printf "No solution\n"
;;

let main_loop () =
  let rec loop () =
    match Readline.input_line () ~prompt:"numbers> " with
    | None -> ()
    | Some numbers ->
      let numbers = String.split numbers ~on:' ' |> List.map ~f:Int.of_string in
      match Readline.input_line () ~prompt:"target> " with
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
