open Base
open Absint
open Interval
open Astprint

let () =
  let lexbuf = Lexing.from_channel Stdio.stdin in
  try
    let program = Parser.prog Lexer.token lexbuf in
    let memory = Hashtbl.create (module String) in
    let global = Absint.absint_iter program memory in
    let _ =
      Hashtbl.iteri global ~f:(fun ~key:label ~data:memory ->
        let _ = Stdio.printf "label%d: \n" label in
        Hashtbl.iteri memory ~f:(fun ~key:variable ~data:interval ->
          Stdio.printf "%s: %s\n" variable (Interval.to_string interval)))
    in
    print_prog program 0
  with
  | Lexer.Error msg -> Stdio.printf "%s%!" msg
  | Parser.Error ->
    Stdio.printf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
;;
