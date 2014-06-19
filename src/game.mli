exception End_of_game of int

type t =
  | Left
  | Down
  | Up
  | Right

val of_keycode : int -> t
val end_of_game: int -> Grid.t -> Grid.t
val step: int -> Grid.t -> t ->
  ([> Grid.diff_action | Grid.diff_update] list * int * Grid.t)
