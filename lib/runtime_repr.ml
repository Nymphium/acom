open Syntax

module Uuid : sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
  val to_string : t -> string
end = struct
  module M = Uuid

  type t = M.t

  let create = Uuid_unix.create
  let equal = M.equal
  let to_string = M.to_string
end

type env = (variable * runtime_value) list

and runtime_value =
  | RNull
  | RUnit
  | RNum of number
  | RBuiltin of builtin
  | RPromise of Uuid.t
  | Closure of env * variable list * stmts

exception NotConvertible

let rtv_of_value env = function
  | Null -> RNull
  | Unit -> RUnit
  | Num n -> RNum n
  | Fun (xs, stmts) -> Closure (env, xs, stmts)
  | Builtin b -> RBuiltin b
  | _ -> raise NotConvertible
;;

let value_of_rtv = function
  | RNum n -> Num n
  | RNull -> Null
  | RUnit -> Unit
  | RBuiltin b -> Builtin b
  | Closure (_, xs, stmts) -> Fun (xs, stmts)
  | RPromise _ -> failwith "cannot covert to value"
;;

type thread_status =
  | Pending of (unit -> thread_status)
  | Done of runtime_value
