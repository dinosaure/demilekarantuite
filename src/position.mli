type t

type direction =
  | North
  | South
  | East
  | West

val make : int -> int -> t
val origin : t

val x : t -> int
val y : t -> int

val equal : t -> t -> bool

val move : t -> direction -> t
val nmove : t -> int -> direction -> t
val target : direction -> direction -> direction
val opposite : direction -> direction

val print_position : out_channel -> t -> unit
val print_direction : out_channel -> direction -> unit
