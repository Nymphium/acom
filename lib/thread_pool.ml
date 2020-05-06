open Runtime_repr
open Base

type t = (Uuid.t * thread_status) Queue.t

let q : t = Queue.init 0 ~f:(fun _ -> Stdlib.Obj.magic 0)

let enqueue th =
  let uuid = Uuid.create () in
  let () = Queue.enqueue q (uuid, Pending th) in
  uuid
;;

let run () =
  let top = Queue.dequeue q in
  match top with
  | None -> None
  | Some (uuid, ts) ->
    (match ts with
    | Done rtv -> Some rtv
    | Pending thread -> Fn.const None @@ Queue.enqueue q (uuid, thread ()))
;;

let wait =
  let rec go th =
    match th () with
    | Pending th' -> go th'
    | Done r -> r
  in
  fun id ->
    let o_th = Queue.find ~f:(fun (id', _) -> Uuid.equal id id') q in
    match o_th with
    | Some (_, ts) ->
      (match ts with
      | Done rtv -> rtv
      | Pending thread -> go thread)
    | None -> failwith "no such id"
;;

let rec run_all () = if Queue.is_empty q then () else run () |> ignore |> run_all
