open Syntax
open Runtime_repr
open Nlist
open Base

exception NotCoercible = Runtime_repr.NotCoercible
exception VariableNotFound = Runtime_repr.VariableNotFound

let bind_args xs vs =
  let xl, vl = List.(length xs, length vs) in
  Stdlib.List.combine xs
  @@
  if xl > vl
  then Array.(create ~len:(xl - vl) RNull |> to_list) @ vs
  else if xl < vl
  then List.take vs xl
  else vs
;;

let binop op l r =
  let l = value_of_rtv l |> number_of_value in
  let r = value_of_rtv r |> number_of_value in
  match op with
  | Add -> RNum (l + r)
  | Mul -> RNum (l * r)
  | Minus -> RNum (l - r)
;;

let wait_unit : Float.t = 20. /. 1000.

let rec builtin : type ans. builtin -> runtime_value list -> (runtime_value, ans) Cont.t =
 fun bin rtvs ->
  match bin with
  | SetTimeout ->
    let n = List.nth_exn rtvs 0 |> value_of_rtv |> number_of_value in
    (match List.nth_exn rtvs 1 with
    | Closure (env, _, stmts) ->
      let time = ref @@ (Float.of_int n /. 1000.) in
      let () =
        ignore
        @@ Thread_pool.enqueue
        @@
        let rec it () =
          let () = Unix.sleepf wait_unit in
          let rest = !time -. wait_unit in
          if Float.(rest < 0.)
          then (
            let _ = Cont.(run_identity @@ eval_stmts env stmts) in
            Done RUnit)
          else (
            time := rest;
            Pending it)
        in
        it
      in
      Cont.return RUnit
    | _ -> failwith "second value of setTimeout should be a function")
  | ConsoleLog ->
    List.nth_exn rtvs 0
    |> value_of_rtv
    |> number_of_value
    |> Int.to_string
    |> Stdlib.print_endline
    |> Fn.const RUnit
    |> Cont.return

and eval_exp : type ans. env -> exp -> (runtime_value, ans) Cont.t =
 fun env exp ->
  let open Cont in
  let () = Thread_pool.run () |> ignore in
  match exp with
  | Value v -> return @@ rtv_of_value env v
  | Op (op, e1, e2) ->
    let* v1 = eval_exp env e1 in
    let* v2 = eval_exp env e2 in
    return @@ binop op v1 v2
  | Call (e, es) ->
    let* fn = eval_exp env e in
    let* args = Cont.List.map ~f:(eval_exp env) es in
    (match fn with
    | Closure (env', xs, body) ->
      let env'' = bind_args xs args @ env' in
      eval_stmts env'' body
    | RBuiltin bin -> builtin bin args
    | _ -> failwith "this is not callable object")
  | Promise p ->
    (match p with
    | Constructor exp ->
      (match exp with
      | Value (Fun (_, _) as fn) ->
        let exp' = Call (Value fn, [ Value Unit ]) in
        let uuid =
          Thread_pool.enqueue @@ fun () -> run (eval_exp env exp') @@ fun x -> Done x
        in
        return @@ RPromise uuid
      | _ -> failwith "this is not callable object")
    | Wait exp ->
      let* rtv = eval_exp env exp in
      return
        (match rtv with
        | RPromise uuid -> Thread_pool.wait uuid
        | _ -> rtv))

and eval_stmts : type ans. env -> stmts -> (runtime_value, ans) Cont.t =
 fun env stmts ->
  let open Cont in
  match stmts with
  | End stmt ->
    (match stmt with
    | Expression e | Def (_, e) ->
      let* _ = eval_exp env e in
      return RUnit
    | Return e -> eval_exp env e)
  | Last (stmt, tl) ->
    (match stmt with
    | Expression e ->
      let* _ = eval_exp env e in
      eval_stmts env tl
    | Def (x, e) ->
      let* rtv = eval_exp env e in
      let env' = (x, rtv) :: env in
      eval_stmts env' tl
    | Return e -> eval_exp env e)
;;

let run_program stmts =
  let open Cont in
  Fn.flip run value_of_rtv
  @@ let* ret = eval_stmts [] stmts in
     let () = Thread_pool.run_all () in
     return ret
;;

let%test _ =
  let stmts =
    Nlist.from_list
      [ Def ("x", Value (Num 3))
      ; Def ("y", Value (Num 5))
      ; Return (Op (Add, Value (Var "x"), Value (Var "y")))
      ]
  in
  let result = run_program stmts in
  equal_value result (Num 8)
;;

let%test "fun" =
  let stmts =
    Nlist.from_list
      [ Def ("x", Value (Num 5))
      ; Def
          ( "f"
          , Value
              (Fun
                 ( [ "y" ]
                 , Nlist.from_list [ Return (Op (Add, Value (Var "x"), Value (Var "y"))) ]
                 )) )
      ; Def ("x", Value (Num 10))
      ; Return (Call (Value (Var "f"), [ Value (Num 12) ]))
      ]
  in
  let result = run_program stmts in
  equal_value result @@ Num 17
;;

let%test "return" =
  let stmts = Nlist.from_list [ Return (Value (Num 3)); Return (Value (Num 5)) ] in
  let result = run_program stmts in
  equal_value result @@ Num 3
;;

let%expect_test _ =
  let stmts =
    Nlist.from_list
      [ Def ("x", Value (Num 100))
      ; Expression
          (Call
             ( Value (Builtin SetTimeout)
             , [ Value (Num 2000)
               ; Value
                   (Fun
                      ( []
                      , Nlist.from_list
                          [ Expression
                              (Call (Value (Builtin ConsoleLog), [ Value (Var "x") ]))
                          ] ))
               ] ))
      ; Expression (Call (Value (Builtin ConsoleLog), [ Value (Num 500) ]))
      ; Expression
          (Call
             ( Value (Builtin SetTimeout)
             , [ Value (Num 500)
               ; Value
                   (Fun
                      ( []
                      , Nlist.from_list
                          [ Expression
                              (Call (Value (Builtin ConsoleLog), [ Value (Num 40) ]))
                          ] ))
               ] ))
      ]
  in
  let () = run_program stmts |> ignore in
  [%expect {|
    500
    40
    100
    |}]
;;

let%expect_test _ =
  let stmts =
    Nlist.from_list
      [ Def ("x", Value (Num 100))
      ; Def
          ( "promise"
          , Promise
              (Constructor
                 (Value
                    (Fun
                       ( []
                       , Nlist.from_list
                           [ Expression
                               (Call
                                  ( Value (Builtin SetTimeout)
                                  , [ Value (Num 2000)
                                    ; Value
                                        (Fun
                                           ( []
                                           , Nlist.from_list
                                               [ Expression
                                                   (Call
                                                      ( Value (Builtin ConsoleLog)
                                                      , [ Value (Var "x") ] ))
                                               ] ))
                                    ] ))
                           ] )))) )
      ; Expression (Promise (Wait (Value (Var "promise"))))
      ; Expression (Call (Value (Builtin ConsoleLog), [ Value (Num 200) ]))
      ]
  in
  let () = run_program stmts |> ignore in
  [%expect {|
    100
    200
    |}]
;;
