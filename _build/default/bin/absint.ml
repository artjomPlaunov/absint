open Syntax
open Bigint

(*
let rec absintExpr e (mem : (BigInt.t * BigInt.t) option StringMap.t option) =
  match e with
  | Const c -> Some (BigInt.Int c, BigInt.Int c)
  | Var x ->
    (match StringMap.find x (Option.get mem) with
     | Some interval -> Some interval
     | _ -> None)
  | Binop (binop, e1, e2) -> intervalBinop binop (absintExpr e1 mem) (absintExpr e2 mem)
;;
(*
(*
let absintCond to_negate (Cmp (cmpop, e1, e2)) mem =*)

let rec absintStep
  (_label:label)
  (command: cmd)
  (nextLabel:label)
  (mem : (BigInt.t * BigInt.t) option StringMap.t option)
  =
  match command with
  | Seq (_, c1, c2) -> absintStep (firstLabel c1) c1 (firstLabel c2) mem
  | Assign (_, x, e) ->
    let interval = absintExpr e mem in
    (match interval with
     | None -> failwith "expression undefined in absintStep"
     | Some i ->
       let mem' = StringMap.remove x (Option.get mem) in
       let mem'' = StringMap.add x (Some i) mem' in
       [ nextLabel, Some mem'' ])
  | _ -> []
;;

(*
  | While (_, cond, c) ->
      [
        (firstLabel c, absintCond cond mem),
        (nextLabel, absintCond (cond mem)
      ]*)

let join (i1 : (BigInt.t * BigInt.t) option) (i2 : (BigInt.t * BigInt.t) option) =
  match i1, i2 with
  | None, i -> i
  | i, None -> i
  | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.min lo1 lo2, BigInt.max hi1 hi2)
;;

let updateGlobal = failwith ""

let rec absintIterLoop
  (stack : label Base.Stack.t)
  (global : mem IntMap.t)
  (constProg : cmd)
  (lExit : label)
  =
  if Base.Stack.is_empty stack
  then global
  else (
    let label = Base.Stack.pop_exn stack in
    match findCmd constProg label with
    | None -> failwith "findCmd err in absintIterLoop"
    | Some command ->
      (match findNextLabel constProg label lExit with
       | None -> failwith "findNextLabel err in absintIterLoop"
       | Some nextLabel ->
         let (mem : 'a StringMap.t option) =
           if IntMap.mem label global
           then IntMap.find label global
           else Some StringMap.empty
         in
         let mems = absintStep label command nextLabel mem in
         let global', stack' = List.fold_left updateGlobal (global, stack) mems in
         absintIterLoop stack' global' constProg lExit))
;;

let absintIter (Prog (cmd, l)) (initMem : mem) =
  let initLabel = firstLabel cmd in
  let worklist = Base.Stack.of_list [ initLabel ] in
  let glbl = IntMap.empty in
  let glbl' = IntMap.add initLabel initMem glbl in
  absintIterLoop worklist glbl' cmd l
;;
*)*)