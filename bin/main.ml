open Stdio
open Astprint
open Label

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let (Prog (c, l)) = Parser.prog Lexer.token lexbuf in
    let _ = print_endline "" in
    printProg (Prog (c, l)) 0
  with
  | Lexer.Error msg -> Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
;;
