open Syntax
open Base

module Uuid : sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
  val to_string : t -> string
  val comparator : (t, Uuid.comparator_witness) Comparator.t
end = struct
  module M = Uuid

  type t = M.t

  let create = Uuid_unix.create
  let equal = M.equal
  let to_string = M.to_string
  let comparator = M.comparator
end

type env = (variable * runtime_value) list

and runtime_value =
  | RNull
  | RUnit
  | RNum of number
  | RBuiltin of builtin
  | RPromise of Uuid.t
  | Closure of env * variable list * stmts
  | NativeFun of (runtime_value list -> runtime_value)

exception NotConvertible
exception NotCoercible
exception VariableNotFound of variable

let number_of_value = function
  | Num n -> n
  | _ -> raise NotCoercible
;;

let lookup x env =
  try List.Assoc.find_exn env x ~equal:equal_variable with
  | Not_found_s _ -> raise @@ VariableNotFound x
;;

let rtv_of_value env = function
  | Var x -> lookup x env
  | Null -> RNull
  | Unit -> RUnit
  | Num n -> RNum n
  | Fun (xs, stmts) -> Closure (env, xs, stmts)
  | Builtin b -> RBuiltin b
;;

let value_of_rtv = function
  | RNum n -> Num n
  | RNull -> Null
  | RUnit -> Unit
  | RBuiltin b -> Builtin b
  | Closure (_, xs, stmts) -> Fun (xs, stmts)
  | RPromise _ -> failwith "cannot covert to value"
  | _ -> failwith "naitive function cannot be converted to value"
;;

type thread_status =
  | Pending of (runtime_value list, runtime_value) Cont.t
  | Done of runtime_value
