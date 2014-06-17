let rec loop (score, grid) =
  let () =
    Grid.pp_print grid;
    Printf.printf "%d> " score
  in let exec = Game.step score grid
  in let game = match read_line () with
    | "h" | "q" -> exec Game.Left
    | "j" | "z" -> exec Game.Up
    | "k" | "s" -> exec Game.Down
    | "l" | "d" -> exec Game.Right
    | _ -> (score, grid)
  in loop game

let () =
  let () = Random.self_init () in
  try loop (0, (Grid.update @@ Grid.update @@ Grid.make 4)) with
  | Grid.Full_grid -> print_endline "te naze"
  | Game.End_of_game score -> print_endline "te nul"
