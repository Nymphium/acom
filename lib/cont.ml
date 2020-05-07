type ('hole, 'ans) t = ('hole -> 'ans) -> 'ans

let ( >>= ) cont k k' =
  cont
  @@ fun a ->
  let cont' = k a in
  cont' k'
;;

let ( let* ) v k = v >>= k
let return v k = k v
let lift f k = f k
let run = Fun.id
let run_identity c = run c Fun.id

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
