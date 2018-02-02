open! Core

let letters = List.filter [%all: Char.t] ~f:Char.is_lowercase

let default_dictionary_file =
  Filename.(
    of_parts
      [ dirname Sys.executable_name
      ; parent_dir_name
      ; "share"
      ; "countdown"                     (* FIXME avoid hard-coding this *)
      ; "dictionary" ])
;;

let tree_param =
  let open Command.Let_syntax in
  let%map_open filename =
    "dictionary-file" %: string
    |> maybe_with_default default_dictionary_file
    |> anon
  in
  In_channel.with_file filename ~f:(fun chan ->
    In_channel.(fold_lines chan)
      ~init:(Anagram_tree.create letters)
      ~f:(fun tree word ->
        let word = String.lowercase word in
        Anagram_tree.add tree word))
;;

let main_loop tree =
  let rec loop () =
    printf "> %!";
    match In_channel.(input_line stdin) with
    | None -> ()
    | Some word ->
      let word = String.lowercase word in
      let by_length_desc = Comparable.lift Int.descending ~f:String.length in
      let cmp = Comparable.lexicographic [ by_length_desc; String.compare ] in
      let results =
        Anagram_tree.to_sequence tree word
        |> Sequence.to_array
      in
      Array.sort results ~cmp;
      printf !"%{sexp: string array}\n%!" results;
      loop ()
  in
  loop ()
;;

let command =
  let open Command.Let_syntax in
  Command.basic ~summary:"find words in input letters" (
    let%map_open tree = tree_param in
    fun () -> main_loop tree)
;;
