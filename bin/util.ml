open Syntax
open Base

let get_global_mem global label =
  match Base.Hashtbl.find global label with
  | None -> Base.Hashtbl.create (module String)
  | Some memory -> Base.Hashtbl.copy memory
;;

let print_global_mem global =
  Hashtbl.iteri global ~f:(fun ~key:label ~data:memory ->
    let _ = Stdio.printf "label%d: \n" label in
    Hashtbl.iteri memory ~f:(fun ~key:variable ~data:interval ->
      let _ = Stdio.printf "%s: " variable in
      Interval.to_string interval |> Stdio.printf "%s\n"))
;;

let find_label command =
  match command with
  | Seq (l, _, _) | Assume (l, _) | While (l, _, _) | Choice (l, _, _) | Assign (l, _, _)
    -> l
;;

(* this should really be called find "outside" label*)
let rec find_next_label command label next_label =
  match command with
  | Seq (lbl, cmd1, cmd2) ->
    if lbl = label
    then Some (find_label cmd1)
    else (
      match find_next_label cmd1 label (find_label cmd2) with
      | None -> find_next_label cmd2 label next_label
      | label_opt -> label_opt)
  | Assume (lbl, _) -> if lbl = label then Some next_label else None
  | While (lbl, _, cmd) ->
    if lbl = label
    then Some next_label
    else (
      match find_next_label cmd label lbl with
      (* find_next_label cmd label lbl seems weird, the lbl part...*)
      | None -> None
      | label_opt -> label_opt)
  | Choice (lbl, cmd1, cmd2) ->
    if lbl = label
    then Some next_label
    else (
      match find_next_label cmd1 label next_label with
      | Some lbl -> Some lbl
      | None -> find_next_label cmd2 label next_label)
  | Assign (lbl, _, _) -> if lbl = label then Some next_label else None
;;

let rec find_command command label =
  match command with
  | Seq (lbl, cmd1, cmd2) ->
    if lbl = label
    then Some (Seq (lbl, cmd1, cmd2))
    else (
      match find_command cmd1 label with
      | Some cmd -> Some cmd
      | None ->
        (match find_command cmd2 label with
         | Some cmd -> Some cmd
         | None -> None))
  | Assume (lbl, _cmd) -> if lbl = label then Some command else None
  | While (lbl, _cond, cmd) ->
    if lbl = label
    then Some command
    else (
      match find_command cmd label with
      | Some cmd -> Some cmd
      | None -> None)
  | Choice (lbl, cmd1, cmd2) ->
    if lbl = label
    then Some command
    else (
      match find_command cmd1 label with
      | Some cmd -> Some cmd
      | None ->
        (match find_command cmd2 label with
         | Some cmd -> Some cmd
         | None -> None))
  | Assign (lbl, _var, _expr) -> if lbl = label then Some command else None
;;
