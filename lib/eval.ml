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

let rec builtin : builtin -> runtime_value list -> (runtime_value, runtime_value) Cont.t =
 fun bin rtvs ->
  match bin with
  | SetTimeout ->
    let n = List.nth_exn rtvs 0 |> value_of_rtv |> number_of_value in
    (match List.nth_exn rtvs 1 with
    | Closure (env, _, stmts) ->
      let time = Float.(of_int n / 1000.) in
      let () =
        Time_scheduler.register time
        @@ fun () -> ignore @@ Cont.(run_identity @@ eval_program env stmts)
      in
      Cont.return RUnit
    | _ -> failwith "second value of setTimeout should be a function")
  | ConsoleLog ->
    List.nth_exn rtvs 0
    |> value_of_rtv
    |> number_of_value
    |> Int.to_string
    |> Stdio.print_endline
    |> Fn.const RUnit
    |> Cont.return

and eval_exp : env -> exp -> (runtime_value, runtime_value) Cont.t =
 fun env exp ->
  let open Cont in
  let () = Time_scheduler.step_clock () in
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
    | NativeFun fn -> return @@ fn args
    | e -> failwith @@ "this is not callable object: " ^ (value_of_rtv e |> show_value))
  | Promise (Constructor exp) ->
    (match exp with
    | Value (Fun (args, stmts)) ->
      let uuid =
        Thread_pool.register
        @@ Cont.lift
        @@ fun resolve ->
        let env' = Stdlib.List.combine args [ NativeFun resolve ] in
        Fn.flip run (fun _ -> RUnit) @@ eval_program (env' @ env) stmts
      in
      return @@ RPromise uuid
    | _ -> failwith "this is not syntactically callable object")
  | Promise (Wait exp) ->
    let* rtv = eval_exp env exp in
    (match rtv with
    | RPromise uuid ->
      let* rtvs = Thread_pool.await uuid in
      rtvs |> Base.List.hd |> Option.value ~default:RUnit |> return
    | _ -> return rtv)

and eval_stmts : env -> stmts -> (runtime_value, runtime_value) Cont.t =
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

and eval_program env stmts =
  let open Cont in
  let* rtv = eval_stmts env stmts in
  let () = Time_scheduler.run_all () in
  let () = Thread_pool.run_all () in
  Cont.return rtv
;;

let run_program stmts = value_of_rtv @@ Cont.run_identity @@ eval_program [] stmts

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
    (*
     * const x = 100;
     * const promise = new Promise(() => {
     *   setTimeout(() => console.log(x), 2000);
     * });
     * await promise;
     * console.log(200);
     *)
    Nlist.from_list
      [ Def ("x", Value (Num 100))
      ; Def
          ( "promise"
          , Promise
              (Constructor
                 (Value
                    (Fun
                       ( [ "resolve" ]
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
                                               ; Expression
                                                   (Call (Value (Var "resolve"), []))
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
