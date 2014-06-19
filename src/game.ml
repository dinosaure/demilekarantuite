exception End_of_game of int

type t =
  | Left
  | Down
  | Up
  | Right

let of_keycode = function
  | 37 -> Left
  | 38 -> Up
  | 39 -> Right
  | 40 -> Down
  | _ -> raise (Invalid_argument "Game.of_keycode")

let stat lst = List.length lst <> 0

let ( >|= ) (diff, grid) move =
  if stat diff then (diff, grid)
  else move grid

let ( >>= ) value func =
  func value

let end_of_game score grid =
  if Grid.is_full grid
  then Grid.move_left grid
    >|= Grid.move_down
    >|= Grid.move_right
    >|= Grid.move_up
    >>= (fun (diff, _) -> if stat diff
          then grid
          else raise (End_of_game score))
  else grid

let print_list ?(sep = ", ") print_data out_ch lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> Printf.fprintf out_ch "%a" print_data x
    | x :: r -> Printf.fprintf out_ch "%a%s" print_data x sep; aux r
  in aux lst

let print_diff out_ch lst =
  let print_data out_ch = function
    | `Update (position, tile) ->
      Printf.fprintf out_ch "Merge tile %a to %d"
        Position.print_position position
        (Tile.to_int tile)
    | `Move (position, position', tile) ->
      Printf.fprintf out_ch "Move tile %a to %a"
        Position.print_position position
        Position.print_position position'
    | `Add (position, tile) ->
      Printf.fprintf out_ch "Add tile %d in %a"
        (Tile.to_int tile)
        Position.print_position position
  in
  Printf.fprintf out_ch "%a" (print_list ~sep:"\n" print_data) lst

let step score grid action =
  let (diff, grid) = match action with
    | Left -> Grid.move_left grid
    | Right -> Grid.move_right grid
    | Up -> Grid.move_up grid
    | Down -> Grid.move_down grid
  in
  let new_score = List.fold_left
      (fun acc -> function
         | `Update (_, tile) -> Tile.to_int tile + acc
         | _ -> acc)
      score diff
  in
  let new_diff, new_grid =
    if stat diff
    then (Grid.update grid
          |> (fun (new_diff, grid) -> diff @ [ new_diff ], grid))
    else diff, grid
  in

  Printf.printf "%a\n" print_diff new_diff;
  end_of_game new_score new_grid

  |> (fun _ -> (new_diff, new_score, new_grid))
