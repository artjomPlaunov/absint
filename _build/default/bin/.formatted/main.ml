open Base
open Absint
open Binterval
open Astprint

let () =
  let lexbuf = Lexing.from_channel Stdio.stdin in
  try
    let p = Parser.prog Lexer.token lexbuf in
    let mem = Hashtbl.create (module String) in
    let glbl = Absint.absintIter p mem in
    let _ =
      Hashtbl.iteri glbl ~f:(fun ~key:lbl ~data:mem ->
        let _ = Stdio.printf "label%d: \n" lbl in
        Hashtbl.iteri mem ~f:(fun ~key:var ~data:d ->
          let _ = Stdio.printf "%s: " var in
          let s = Binterval.toString d in
          Stdio.printf "%s\n" s))
    in
    print_prog p 0
  with
  | Lexer.Error msg -> Stdio.printf "%s%!" msg
  | Parser.Error ->
    Stdio.printf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
;;