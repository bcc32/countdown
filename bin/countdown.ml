open Core
open Countdown_lib

let command =
  Command.group ~summary:"Countdown solver"
    [ "numbers", Numbers.command
    ; "words", Words.command
    ]
;;

let () = Command.run command
