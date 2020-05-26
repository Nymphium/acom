open Base

type elt = Float.t * (unit -> unit)

let unit_time = Float.(1. / 1000.)
let queue : elt list ref = ref []
let register time thunk = queue := (time, thunk) :: !queue

let step_clock () =
  let queue' = !queue in
  let () = queue := [] in
  queue'
  |> List.filter_map ~f:(fun (time, thunk) ->
         let () = Unix.sleepf unit_time in
         let time' = Float.(time - unit_time) in
         if Float.is_non_positive time'
         then (
           thunk ();
           None)
         else Some (time', thunk))
  |> ( := ) queue
;;

let run_all () =
  while not @@ List.is_empty !queue do
    step_clock ()
  done
;;
