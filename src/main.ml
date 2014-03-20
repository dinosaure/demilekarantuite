let rec loop grid =
  let () = Grid.print grid; Printf.printf "> " in
  let (change, grid) = match read_line () with
    | "h" | "q" -> Grid.move_left grid
    | "j" | "z" -> Grid.move_up grid
    | "k" | "s" -> Grid.move_down grid
    | "l" | "d" -> Grid.move_right grid
    | _ -> (false, grid)
  in if change then loop (Grid.update grid) else loop grid

let () =
  let () = Random.self_init () in
  try loop (Grid.update @@ Grid.update @@ Grid.make 4) with
    | Grid.Full_grid -> print_endline "te naze"
    | End_of_file -> print_endline "te bidon"
