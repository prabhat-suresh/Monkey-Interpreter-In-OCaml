open Base
open Stdio
open MonkeyInterpreter

let user = Option.value ~default:"User" (Sys.getenv "USER")

let () =
  printf "Hello %s! This is the Monkey programming language!\n" user;
  print_endline "Feel free to type in commands";
  Repl.start stdin stdout
