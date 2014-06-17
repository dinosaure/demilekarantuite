exception End_of_game of int

type t =
  | Left
  | Down
  | Up
  | Right

let ( >|= ) (stat, combine, grid) move =
  if stat then (true, combine, grid)
  else move grid

let ( >>= ) value func =
  func value

let end_of_game score grid =
  if Grid.is_full grid
  then Grid.move_left grid
    >|= Grid.move_down
    >|= Grid.move_right
    >|= Grid.move_up
    >>= (fun (stat, _, _) -> if stat then grid else raise (End_of_game score))
  else grid

let step score grid action =
  let (stat, combine, grid) = match action with
    | Left -> Grid.move_left grid
    | Right -> Grid.move_right grid
    | Up -> Grid.move_up grid
    | Down -> Grid.move_down grid
  in let new_score = List.fold_left
      (fun acc x -> Tile.to_int x + acc)
      score combine
  in let new_grid =
    if stat then Grid.update grid else grid
  in end_of_game new_score new_grid
     |> (fun _ -> (new_score, new_grid))
