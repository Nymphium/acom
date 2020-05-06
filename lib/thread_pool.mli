open Runtime_repr

type t = (Uuid.t * thread_status) Base.Queue.t

val enqueue : (unit -> thread_status) -> Uuid.t
val run : unit -> runtime_value option
val run_all : unit -> unit
val wait : Uuid.t -> runtime_value
