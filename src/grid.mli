type t

exception Full_grid

val make: int -> t
val rotate: t -> t
val is_full: t -> bool

val move_right: t -> (bool * t)
val move_left: t -> (bool * t)
val move_down: t -> (bool * t)
val move_up: t -> (bool * t)

val update: t -> t

val print: t -> unit
