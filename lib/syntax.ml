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
 *       | x
 *       | function(x* ) { stmt+ }
 *       | null
 *       | ()
 *)
type value =
  | Num of number
  | Var of variable
  | Fun of variable list * stmt Nlist.t
  | Builtin of builtin
  | Null
  | Unit

(**
 * exp  := v
 *       | exp op exp
 *       | exp(exp* )
 *       | Promise.create(exp)
 *)
and exp =
  | Value of value
  | Op of binop * exp * exp
  | Call of exp * exp list
  | Promise of promise
and promise =
  | Constructor of exp
  | All of exp Nlist.t
  | Wait of exp

(**
 * stmt := exp
 *       | const x = exp
 *       | return exp
 *)
and stmt =
  |  Expression of exp
  | Def of variable * exp
  | Return of exp

type stmts = stmt Nlist.t
