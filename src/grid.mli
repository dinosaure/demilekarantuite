type t

exception Full_grid

val nth: (int * int) -> t -> Tile.t
val make: int -> t
val mapi: ((int * int) -> Tile.t -> Tile.t) -> t -> t
val iteri: ((int * int) -> Tile.t -> unit) -> t -> unit
val foldl: ((int * int) -> 'a -> Tile.t -> 'a) -> 'a -> t -> 'a
val rotate: t -> t

val move_right: t -> (bool * t)
val move_left: t -> (bool * t)
val move_down: t -> (bool * t)
val move_up: t -> (bool * t)

val update: t -> t

val print: t -> unit
