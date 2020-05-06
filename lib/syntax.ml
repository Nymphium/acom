type binop =
  | Add
  | Minus
  | Mul
[@@deriving eq]

type builtin =
  | SetTimeout
  | ConsoleLog
[@@deriving eq]

type number = int [@@deriving eq]
type variable = string [@@deriving eq]

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
  | Fun of variable list * stmts
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
  | Wait of exp

(**
 * stmt := exp
 *       | const x = exp
 *       | return exp
 *)
and stmt =
  | Expression of exp
  | Def of variable * exp
  | Return of exp

and stmts = stmt Nlist.t [@@deriving eq]
