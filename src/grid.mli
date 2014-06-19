type t

type diff_update = [
  | `Add of (Position.t * Tile.t)
]

type diff_action = [
  | `Update of (Position.t * Tile.t)
  | `Move of (Position.t * Position.t * Tile.t)
]

exception Full_grid

val make: int -> t
val rotate: t -> t
val is_full: t -> bool

val move_right: t -> ([> diff_action] list * t)
val move_left: t -> ([> diff_action] list * t)
val move_down: t -> ([> diff_action] list * t)
val move_up: t -> ([> diff_action] list * t)

val update: t -> ([> diff_update] * t)

val pp_print: t -> unit
