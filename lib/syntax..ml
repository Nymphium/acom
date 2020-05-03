(* non-empty list *)
type 'a nlist =
  | Head of 'a
  | Tail of 'a * 'a nlist

type binop =
  | Add
  | Minus
  | Mul

type builtin =
  | SetTimeout
  | ConsoleLog

type number = int
type variable = string

(**
 * n     ∈ Numebrs
 * x     ∈ Variables
 * v    := n
 *       | function(x* ) { stmt+ }
 *)
type value =
  | Num of number
  | Fun of variable list * stmt nlist
  | Builtin of builtin

(**
 * exp  := v
 *       | exp op exp
 *       | exp(exp* )
 *       | PRomise.create(exp)
 *)
and exp =
  | Value of value
  | Op of binop * exp * exp
  | Call of exp * exp list
  | Promise of promise
and promise =
  | Constructor of exp
  | All of exp nlist
  | Wait of exp

(**
 * stmt := exp
 *       | const x = exp
 *       | return exp
 *)
and stmt =
  |  Expression of exp
  (* Def(x, e) === const x = e; *)
  | Def of variable * exp
  | Return of exp

