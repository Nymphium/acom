open Runtime_repr
open Base

type elt = Uuid.t * thread_status
type t = elt Queue.t

let q : t = Queue.create ()

let register th =
  let uuid = Uuid.create () in
  let () = Queue.enqueue q (uuid, Pending th) in
  uuid
;;

let await id =
  let o_th = Queue.find ~f:(fun (id', _) -> Uuid.equal id id') q in
  match o_th with
  | Some (_, ts) ->
    (match ts with
    | Done rtv -> Cont.return [ rtv ]
    | Pending thread ->
      let () = Queue.filter_inplace q ~f:(fun (id', _) -> not @@ Uuid.equal id id') in
      thread)
  | None -> failwith "no such id"
;;

let run () =
  Queue.dequeue q
  |> function
  | None -> None
  | Some (uuid, ts) ->
    (match ts with
    | Done rtv -> Some rtv
    | Pending thread ->
      let res = Cont.run thread @@ Fn.compose (Option.value ~default:RUnit) List.hd in
      let () = Queue.enqueue q (uuid, Done res) in
      None)
;;

let rec run_all () = if Queue.is_empty q then () else run () |> ignore |> run_all
