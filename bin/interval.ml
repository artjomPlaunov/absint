open Bigint

module Interval : sig
  type t = (BigInt.t * BigInt.t) option

  val binop : Syntax.binop -> BigInt.t -> BigInt.t -> BigInt.t
  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t
  val not : t -> t
  val join : t -> t -> t
  val meet : t -> t -> t
  val is_subset : t -> t -> bool
  val to_string : t -> string
end = struct
  type t = (BigInt.t * BigInt.t) option

  let binop op t1 t2 =
    match op with
    | Syntax.Add -> BigInt.( + ) t1 t2
    | Syntax.Sub -> BigInt.( - ) t1 t2
  ;;

  let ( - ) t1 t2 =
    match t1, t2 with
    | None, _ | _, None -> failwith "err"
    | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.( - ) lo1 hi2, BigInt.( - ) hi1 lo2)
  ;;

  let ( + ) t1 t2 =
    match t1, t2 with
    | None, _ | _, None -> failwith "err"
    | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.( + ) lo1 lo2, BigInt.( + ) hi1 hi2)
  ;;

  let not t =
    BigInt.(
      match t with
      | None -> Some (NegInf, PosInf)
      | Some (Int _lo, Int _hi) -> Some (NegInf, PosInf)
      | Some (NegInf, Int hi) -> Some (Int hi + Int Z.one, PosInf)
      | Some (Int lo, PosInf) -> Some (NegInf, Int lo - Int Z.one)
      | Some (NegInf, PosInf) -> None
      | _ ->
        failwith "not valid interval -- i.e intervals of the form [x, -inf] | [inf, x]")
  ;;

  let join t1 t2 =
    match t1, t2 with
    | None, None -> None
    | None, t | t, None -> t
    | Some (lo1, hi1), Some (lo2, hi2) -> Some (BigInt.min lo1 lo2, BigInt.max hi1 hi2)
  ;;

  let meet t1 t2 =
    match t1, t2 with
    | None, _ | _, None -> None
    | Some (lo1, hi1), Some (lo2, hi2) ->
      if hi1 < lo2 || hi2 < lo1
      then None
      else Some (BigInt.max lo1 lo2, BigInt.min hi1 hi2)
  ;;

  let is_subset t t2 =
    match t, t2 with
    | None, _ -> true
    | _, None -> false
    | Some (lo1, hi1), Some (lo2, hi2) ->
      BigInt.(min lo1 lo2 == lo2 && max hi1 hi2 == hi2)
  ;;

  let to_string t =
    match t with
    | None -> ""
    | Some (lo, hi) -> "[" ^ BigInt.to_string lo ^ ", " ^ BigInt.to_string hi ^ "]"
  ;;
end
