open Base.Fn

type ('hole, 'ans) t = { run_cont : ('hole -> 'ans) -> 'ans }

let ( >>= ) { run_cont } k =
  { run_cont =
      (fun k' ->
        run_cont
        @@ fun a ->
        let { run_cont } = k a in
        run_cont k')
  }
;;

let ( let* ) v k = v >>= k
let return v = { run_cont = (fun k -> k v) }
let lift f = { run_cont = (fun k -> f k) }
let run { run_cont } = run_cont
let run_identity c = run c id

module List = struct
  let rec map ~f xs =
    match xs with
    | [] -> return []
    | x :: xs' ->
      let* y = f x in
      let* ys = map ~f xs' in
      return @@ (y :: ys)
  ;;
end
