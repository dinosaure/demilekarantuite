exception End_of_game of int

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
    >>= (fun (change, _, _) -> if change then grid else raise (End_of_game score))
  else grid

let rec loop score grid =
  let () = Grid.print grid; Printf.printf "%d> " score in
  let (change, combine, grid) = match read_line () with
    | "h" | "q" -> Grid.move_left grid
    | "j" | "z" -> Grid.move_up grid
    | "k" | "s" -> Grid.move_down grid
    | "l" | "d" -> Grid.move_right grid
    | _ -> (false, [], grid)
  in let new_score = List.fold_left (fun acc x -> Tile.to_int x + acc) score combine in
  if change
  then loop new_score (Grid.update grid)
  else end_of_game new_score grid |> loop new_score

let () =
  let () = Random.self_init () in
  try loop 0 (Grid.update @@ Grid.update @@ Grid.make 4) with
    | Grid.Full_grid -> print_endline "te naze"
    | End_of_file -> print_endline "te bidon"
    | End_of_game score -> print_endline "te nul"
