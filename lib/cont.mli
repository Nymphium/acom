type ('hole, 'ans) t

val ( >>= ) : ('a, 'ans) t -> ('a -> ('b, 'ans) t) -> ('b, 'ans) t
val ( let* ) : ('a, 'ans) t -> ('a -> ('b, 'ans) t) -> ('b, 'ans) t
val return : 'a -> ('a, 'ans) t
val lift : (('a -> 'b) -> 'b) -> ('a, 'b) t
val run : ('a, 'b) t -> ('a -> 'b) -> 'b
val run_identity : ('a, 'a) t -> 'a

module List : sig
  val map : f:('a -> ('b, 'c) t) -> 'a list -> ('b list, 'c) t
end
