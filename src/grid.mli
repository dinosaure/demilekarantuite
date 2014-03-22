type t

exception Full_grid

val make: int -> t
val rotate: t -> t
val is_full: t -> bool

val move_right: t -> (bool * Tile.t list * t)
val move_left: t -> (bool * Tile.t list * t)
val move_down: t -> (bool * Tile.t list * t)
val move_up: t -> (bool * Tile.t list * t)

val update: t -> t

val pp_print: t -> unit
