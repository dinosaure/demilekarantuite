exception End_of_game of int

type t =
  | Left
  | Down
  | Up
  | Right

val end_of_game: int -> Grid.t -> Grid.t
val step: int -> Grid.t -> t -> (int * Grid.t)
