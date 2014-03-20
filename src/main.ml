exception End_of_game

let ( >|= ) (change, grid) move =
  if change then (true, grid)
  else move grid

let ( >>= ) value func =
  func value

let end_of_game grid =
  if Grid.is_full grid
  then Grid.move_left grid
    >|= Grid.move_down
    >|= Grid.move_right
    >|= Grid.move_up
    >>= (fun (change, _) -> if change then grid else raise End_of_game)
  else grid

let rec loop grid =
  let () = Grid.print grid; Printf.printf "> " in
  let (change, grid) = match read_line () with
    | "h" | "q" -> Grid.move_left grid
    | "j" | "z" -> Grid.move_up grid
    | "k" | "s" -> Grid.move_down grid
    | "l" | "d" -> Grid.move_right grid
    | _ -> (false, grid)
  in if change then loop (Grid.update grid) else end_of_game grid |> loop

let () =
  let () = Random.self_init () in
  try loop (Grid.update @@ Grid.update @@ Grid.make 4) with
    | Grid.Full_grid -> print_endline "te naze"
    | End_of_file -> print_endline "te bidon"
    | End_of_game -> print_endline "te nul"
