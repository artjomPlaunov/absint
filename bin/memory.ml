open Base
open Interval
open Syntax

module Memory : sig
  type t = (var, Interval.t) Hashtbl.t

  val ( == ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val join : t -> t -> t
end = struct
  type t = (var, Interval.t) Hashtbl.t

  let is_subset t1 t2 =
    Hashtbl.fold
      ~init:true
      ~f:(fun ~key:var ~data:interval1 bool_acc ->
        let interval2 = Option.value (Hashtbl.find t2 var) ~default:None in
        Interval.is_subset interval1 interval2 && bool_acc)
      t1
  ;;

  let ( == ) t1 t2 = is_subset t1 t2 && is_subset t2 t1
  let ( <> ) t1 t2 = not (t1 == t2)

  let join t1 t2 =
    Hashtbl.fold
      ~init:t1
      ~f:(fun ~key:variable2 ~data:interval2 t1_acc ->
        match Hashtbl.find t1 variable2 with
        | None -> t1_acc
        | Some interval1 ->
          let joined_interval = Interval.join interval1 interval2 in
          let _ = Hashtbl.set t1_acc ~key:variable2 ~data:joined_interval in
          t1_acc)
      t2
  ;;
end
