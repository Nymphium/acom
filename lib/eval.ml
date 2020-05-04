open Syntax
open Nlist

exception NotCoercible
exception VariableNotFound of variable

let coerce_number = function 
  | Num n -> n
  | _ -> raise NotCoercible

let lookup x env =
  try
    List.assoc x env
  with
  | Not_found -> raise @@ VariableNotFound x

let bind_args xs vs =
  let xl, vl = List.(length xs, length vs) in
  List.combine xs @@
  if xl > vl then
    Array.(make (xl - vl) Null |> to_list) @ vs
  else
    vs

let builtin bin vs =
  match bin with
  | SetTimeout ->
      let n = coerce_number @@ List.nth vs 0 in
      let () = Unix.sleepf @@ (float_of_int n) /. 1000. in
      Fun(["f"], Nlist.from_list [
        Expression(Call(Value(Var("f")), [Value(Unit)]));
        Return(Value(Unit))
      ])
  | ConsoleLog ->
      let n = coerce_number @@ List.nth vs 0 in
      let () = Printf.printf "%n\n" n in
      Unit

let binop op l r =
  let l = coerce_number l in
  let r = coerce_number r in
  match op with
  | Add -> Num(l + r)
  | Mul -> Num(l * r)
  | Minus -> Num(l - r)

let rec eval_exp env exp =
  match exp with
  | Value (Var x) -> lookup x env
  | Value v -> v
  | Op(op, e1, e2) ->
    let v1 = eval_exp env e1 in
    let v2 = eval_exp env e2 in
    binop op v1 v2
  | Call(e1, e2) ->
    let fn = eval_exp env e1 in
    let args = List.map (eval_exp env) e2 in
    begin match fn with
    | Fun(xs, body) ->
      let env' = bind_args xs args @ env in
      eval_stmts body env'
    | Builtin bin ->
      builtin bin args
    | _ -> failwith "this is not callable object"
    end
  | Promise _ -> failwith "undefined"
and eval_stmts stmts env =
  match stmts with
  | End stmt ->
    begin match stmt with
    | Expression e | Def(_, e) ->
      let _ = eval_exp env e in Unit
    | Return e -> eval_exp env e
    end
  | Last(stmt, tl) ->
    begin match stmt with
    | Expression e ->
      let _ = eval_exp env e in
      eval_stmts tl env
    | Def(x, e) ->
      let env' = (x, eval_exp env e) :: env in
      eval_stmts tl env'
    | Return e ->
      eval_exp env e
    end

let run_program stmts = eval_stmts stmts []

let%test _ =
  let stmts = Nlist.from_list [
    Def("x", Value(Num(3)));
    Def("y", Value(Num(5)));
    Return(Op(Add,  Value(Var"x"), Value(Var"y")))
  ]
  in
  let result = run_program stmts in
  result = Num(8)

let%test _ =
  let stmts = Nlist.from_list [ 
     Expression(Call(Call(Value(Builtin(SetTimeout)), [Value(Num(5000))]), [Value(Fun(["_"], Nlist.from_list [
       Expression(Call(Value(Builtin(ConsoleLog)), [Value(Num(100))])) 
     ]))] ))
  ]
  in
  let result = run_program stmts in
  result = Unit 

let%test "fun" =
  let stmts = Nlist.from_list [
    Def("f", Value(Fun(["x"; "y"], Nlist.from_list [
      Return(Op(Add, Value(Var("x")), Value(Var("y"))))
    ])));
    Return(Call(Value(Var("f")), [Value(Num(5)); Value(Num(12))]))
  ]
  in
  let result = run_program stmts in
  result = Num(17)

let%test "return" =
  let stmts = Nlist.from_list [
    Return(Value(Num(3)));
    Return(Value(Num(5)))
  ]
  in
  let result = eval_stmts stmts [] in
  result = Num(3)
