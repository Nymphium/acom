(* non-empty list *)
type 'a t =
  | End of 'a
  | Last of 'a * 'a t
[@@deriving eq, show]

let rec from_list = function
  | [] -> failwith "non empty list is not allowed"
  | [ e ] -> End e
  | e :: tl -> Last (e, from_list tl)
;;
