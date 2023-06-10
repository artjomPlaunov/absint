open Syntax

module Util : sig
  val findLabel : cmd -> int
  val findNextLabel : cmd -> int -> int -> int option
  val findCmd : cmd -> int -> cmd option
end = struct
  let findLabel command =
    match command with
    | Seq (l, _, _) | Assume (l, _) | While (l, _, _) | Choice (l, _, _) | Assign (l, _, _)
      -> l
  ;;

  let rec findNextLabel command label next_label =
    match command with
    | Seq (lbl, cmd1, cmd2) ->
      if lbl = label
      then Some (findLabel cmd1)
      else (
        match findNextLabel cmd1 label (findLabel cmd2) with
        | None -> findNextLabel cmd2 label next_label
        | label_opt -> label_opt)
    | Assume (lbl, _) -> if lbl = label then Some next_label else None
    | While (lbl, _, cmd) ->
      if lbl = label
      then Some next_label
      else (
        match findNextLabel cmd label next_label with
        | None -> None
        | label_opt -> label_opt)
    | Choice (lbl, cmd1, cmd2) ->
      if lbl = label
      then Some next_label
      else (
        match findNextLabel cmd1 label next_label with
        | Some lbl -> Some lbl
        | None -> findNextLabel cmd2 label next_label)
    | Assign (lbl, _, _) -> if lbl = label then Some next_label else None
  ;;

  let rec findCmd command label =
    match command with
    | Seq (lbl, cmd1, cmd2) ->
      if lbl = label
      then Some (Seq (lbl, cmd1, cmd2))
      else (
        match findCmd cmd1 label with
        | Some cmd -> Some cmd
        | None ->
          (match findCmd cmd2 label with
           | Some cmd -> Some cmd
           | None -> None))
    | Assume (lbl, _cmd) -> if lbl = label then Some command else None
    | While (lbl, _cond, cmd) ->
      if lbl = label
      then Some command
      else (
        match findCmd cmd label with
        | Some cmd -> Some cmd
        | None -> None)
    | Choice (lbl, cmd1, cmd2) ->
      if lbl = label
      then Some command
      else (
        match findCmd cmd1 label with
        | Some cmd -> Some cmd
        | None ->
          (match findCmd cmd2 label with
           | Some cmd -> Some cmd
           | None -> None))
    | Assign (lbl, _var, _expr) -> if lbl = label then Some command else None
  ;;
end