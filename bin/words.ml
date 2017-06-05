open Core
open Countdown_lib

let letters =
  Char.all
  |> List.filter ~f:Char.is_lowercase
;;

let trie_param =
  let open Command.Let_syntax in
  let%map_open filename = anon ("dictionary-file" %: string) in
  In_channel.with_file filename ~f:(fun chan ->
    Debug.eprint "Constructing index from dictionary file...";
    let trie =
    In_channel.(fold_lines chan)
      ~init:(One_sided_trie.create letters)
      ~f:(fun trie word ->
        let word = String.lowercase word in
        One_sided_trie.add trie word
      )
    in
    Debug.eprint "Checking invariant...";
    One_sided_trie.invariant trie;
    Debug.eprint "Done!";
    trie
  )
;;

let main_loop trie =
  In_channel.(iter_lines stdin) ~f:(fun word ->
    let word = String.lowercase word in
    let by_length_desc = Comparable.lift Int.descending ~f:String.length in
    let cmp = Comparable.lexicographic [ by_length_desc; String.compare ] in
    let results =
      One_sided_trie.to_sequence trie word
      |> Sequence.to_array
    in
    Array.sort results ~cmp;
    printf !"%{sexp: string array}\n%!" results
  );
;;

let command =
  let open Command.Let_syntax in
  Command.basic' ~summary:"find words in input letters" begin
    let%map_open trie = trie_param in
    fun () -> main_loop trie
  end
;;

let () = Command.run command
