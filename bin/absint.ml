open Base
open Bigint
open Interval
open Memory
open Syntax
open Util

module Absint : sig
  val absint_expression : expr -> Memory.t -> Interval.t
  val absint_command : cmd -> (label, Memory.t) Hashtbl.t -> label -> label list
  val absint_iter : prog -> Memory.t -> (label, (var, Interval.t) Hashtbl.t) Hashtbl.t

  val absint_iter_loop
    :  label Stack.t
    -> (label, Memory.t) Hashtbl.t
    -> cmd
    -> label
    -> (label, Memory.t) Hashtbl.t
end = struct
  let rec absint_expression expression memory =
    match expression with
    | Const c -> Some (BigInt.Int c, BigInt.Int c)
    | Var x ->
      (match Hashtbl.find memory x with
       | Some x -> x
       | None -> failwith "todo, none or [-inf, inf]")
    | Binop (op, left_expr, right_expr) ->
      (match op with
       | Add ->
         Interval.( + )
           (absint_expression left_expr memory)
           (absint_expression right_expr memory)
       | Sub ->
         Interval.( - )
           (absint_expression right_expr memory)
           (absint_expression right_expr memory))
  ;;

  let absint_condition (Cmp (cmp_op, left_expr, right_expr)) memory =
    match left_expr with
    | Var x ->
      let right_interval = absint_expression right_expr memory in
      (match cmp_op with
       | Less ->
         (* if right interval is [4,11] modified will be [-inf, 10] -- to generalize [a,b] -> [-inf, b-1]*)
         let modified_right_interval =
           match right_interval with
           | Some (_, hi) ->
             Interval.( - )
               (Some (BigInt.NegInf, hi))
               (Some (BigInt.Int Z.one, BigInt.Int Z.one))
           | None -> None
         in
         let left_interval = Option.value (Hashtbl.find memory x) ~default:None in
         let joined_interval = Interval.join left_interval modified_right_interval in
         joined_interval
       | Equal ->
         (* 
          we can overwrite x with the right_interval if the worklist processes items inorder -- i.e. 
          it never processes something that comes before an assume after processing an assume.contents

          right now we're just super over approximating:
        *)
         let left_interval = Option.value (Hashtbl.find memory x) ~default:None in
         let joined_interval = Interval.join left_interval right_interval in
         joined_interval)
    | _ -> failwith "we only ac(opium)cept variables on left and expressions on right"
  ;;

  let rec absint_command command global next_label =
    match command with
    | Seq (_, cmd1, cmd2) -> absint_command cmd1 global (Util.find_label cmd2)
    | While (while_label, cond, cmd) ->
      let current_memory =
        Hashtbl.find global while_label
        |> Option.value ~default:(Hashtbl.create (module String))
        |> Hashtbl.copy
      in
      let cond_interval = absint_condition cond current_memory in
      let label = Util.find_label cmd in
      let var =
        match cond with
        | Cmp (_, Var x, _) -> x
        | _ -> failwith "No variable in Condition expression in absint_command."
      in
      let var_interval = Option.value (Hashtbl.find current_memory var) ~default:None in
      let meet_interval1 = Interval.meet cond_interval var_interval in
      let meet_interval2 = Interval.meet (Interval.not cond_interval) var_interval in
      List.fold
        [ label, meet_interval1; next_label, meet_interval2 ]
        ~init:[]
        ~f:(fun acc (lbl, meet) ->
          let current_memory_prime = Hashtbl.copy current_memory in
          let _ = Hashtbl.set current_memory_prime ~key:var ~data:meet in
          let next_memory =
            Hashtbl.find global lbl
            |> Option.value ~default:(Hashtbl.create (module String))
          in
          if Memory.( <> ) current_memory_prime next_memory
          then (
            let joined_memory = Memory.join current_memory_prime next_memory in
            let _ = Hashtbl.set global ~key:lbl ~data:joined_memory in
            lbl :: acc)
          else if Hashtbl.is_empty current_memory_prime && Hashtbl.is_empty next_memory
          then lbl :: acc
          else acc)
    | Choice (lbl, cmd1, cmd2) ->
      let current_memory =
        Hashtbl.find global lbl
        |> Option.value ~default:(Hashtbl.create (module String))
        |> Hashtbl.copy
      in
      let label1 = Util.find_label cmd1 in
      let label2 = Util.find_label cmd2 in
      List.fold [ label1; label2 ] ~init:[] ~f:(fun acc label ->
        let next_memory =
          Hashtbl.find global label
          |> Option.value ~default:(Hashtbl.create (module String))
        in
        if Memory.( <> ) current_memory next_memory
        then (
          let joined_memory = Memory.join current_memory next_memory in
          let _ = Hashtbl.set global ~key:label ~data:joined_memory in
          label :: acc)
        else if Hashtbl.is_empty current_memory && Hashtbl.is_empty next_memory
        then label :: acc
        else acc)
    | Assume (lbl, cond) ->
      let v =
        match cond with
        | Cmp (_, Var x, _) -> x
        | _ ->
          failwith
            "No variable in Condition expression. public static void assume \
             absint_command failure."
      in
      let current_memory =
        Hashtbl.find global lbl
        |> Option.value ~default:(Hashtbl.create (module String))
        |> Hashtbl.copy
      in
      let cond_eval = absint_condition cond current_memory in
      let _ = Hashtbl.set current_memory ~key:v ~data:cond_eval in
      let next_memory =
        Hashtbl.find global next_label
        |> Option.value ~default:(Hashtbl.create (module String))
      in
      if Memory.( <> ) current_memory next_memory
      then (
        let joined_memory = Memory.join current_memory next_memory in
        Hashtbl.set global ~key:next_label ~data:joined_memory;
        [ next_label ])
      else []
    | Assign (lbl, var, exp) ->
      let current_memory =
        Hashtbl.find global lbl
        |> Option.value ~default:(Hashtbl.create (module String))
        |> Hashtbl.copy
      in
      let old_interval = Option.value (Hashtbl.find current_memory var) ~default:None in
      let new_interval = absint_expression exp current_memory in
      let join_interval = Interval.join old_interval new_interval in
      let _ = Hashtbl.set current_memory ~key:var ~data:join_interval in
      let next_memory =
        Hashtbl.find global next_label
        |> Option.value ~default:(Hashtbl.create (module String))
      in
      if Memory.( <> ) current_memory next_memory
      then (
        let joined_memory = Memory.join current_memory next_memory in
        Hashtbl.set global ~key:next_label ~data:joined_memory;
        [ next_label ])
      else []
  ;;

  let rec absint_iter_loop worklist global command exit_label =
    if Stack.is_empty worklist
    then global
    else (
      let label = Stack.pop_exn worklist in
      match Util.find_command command label with
      | None -> absint_iter_loop worklist global command exit_label
      | Some command ->
        (match Util.find_next_label command label exit_label with
         | None -> failwith "find_next_label err in absint_iter_loop"
         | Some nextLabel ->
           let labels = absint_command command global nextLabel in
           let _ = List.map labels ~f:(fun x -> Stack.push worklist x) in
           absint_iter_loop worklist global command exit_label))
  ;;

  let absint_iter (Prog (init_command, label)) initial_memory =
    let init_label = Util.find_label init_command in
    let worklist = Base.Stack.of_list [ init_label ] in
    let global = Hashtbl.create (module Int) in
    let _ = Hashtbl.set global ~key:init_label ~data:initial_memory in
    absint_iter_loop worklist global init_command label
  ;;
end
