open Runtime_repr

val register : (runtime_value list, runtime_value) Cont.t -> Uuid.t
val run : unit -> runtime_value option
val run_all : unit -> unit
val await : Uuid.t -> (runtime_value list, runtime_value) Cont.t
