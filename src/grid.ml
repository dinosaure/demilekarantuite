type t = Tile.t list list

exception Full_grid

let make' f size =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (f n :: acc) (n - 1)
  in aux [] size

let make size =
  let f x = make' (fun y -> Tile.empty) size
  in make' f size

let iteri f grid =
  List.iteri
  (fun x row -> List.iteri (fun y tile -> f (x, y) tile) row)
  grid

let mapi f grid =
  List.mapi
  (fun x row -> List.mapi (fun y tile -> f (x, y) tile) row)
  grid

let nth (x, y) grid =
  let size_x = List.length grid in
  if x >= 0 && x < size_x
  then
    let size_y = List.length (List.nth grid x) in
    if y >= 0 && y < size_y
    then List.nth (List.nth grid x) y
    else raise (Invalid_argument ("Grid.nth"))
  else raise (Invalid_argument ("Grid.nth"))

let foldl f acc grid =
  snd @@ List.fold_left
  (fun (x, acc) row ->
  (x + 1, snd @@ List.fold_left (fun (y, acc) tile -> (y + 1, f (x, y) acc tile)) (0, acc) row))
  (0, acc) grid

let available_cells =
  foldl (fun (x, y) acc tile -> if Tile.is_empty tile then (x, y) :: acc else acc) []

let is_full grid =
  match  available_cells grid with
  | [] -> true
  | _ -> false

let pp_tile fmt tile =
  Format.fprintf fmt "%s" (Tile.to_string tile)
let pp_row pp_tile fmt row =
  List.iteri
  (fun i tile ->
    Format.pp_print_tab fmt ();
    Format.fprintf fmt "%a" pp_tile tile)
  row
let pp_header width fmt header =
  let first = Array.map (fun x -> String.make (x + 1) ' ') width in
  Array.iteri (fun i cell ->
    Format.pp_set_tab fmt ();
    for z = 0 to (String.length (List.nth header i)) - 1
      do cell.[z] <- (List.nth header i).[z] done;
    Format.fprintf fmt "%s" cell)
  first
let pp_grid pp_row fmt (header, grid) =
  let width = Array.create (List.length grid) 0 in

  iteri (fun (x, y) tile ->
    width.(y) <- max (String.length (Tile.to_string tile)) width.(y)) grid;
  List.iteri (fun i cell ->
    width.(i) <- max (String.length cell) width.(i)) header;

  Format.pp_open_tbox fmt ();
  Format.fprintf fmt "%a@\n" (pp_header width) header;
  List.iteri (fun i row -> pp_row fmt row) grid;
  Format.pp_close_tbox fmt ();
  Format.print_newline ()

let update grid =
  match available_cells grid with
    | [] -> raise Full_grid
    | cells ->
      let (x', y') = List.nth cells (Random.int (List.length cells)) in
      let new_tile = if Random.float 10. > 9. then Tile.two else Tile.one in
      mapi (fun (x, y) old_tile -> if x = x' && y = y' then new_tile else old_tile) grid

let rotate grid =
  let size = List.length grid in
  mapi (fun (x, y) tile -> nth ((size - y - 1), x) grid) grid

let combine_left row =
  let merge value = function
    | [] -> []
    | x :: r -> Tile.empty :: (Tile.succ value) :: r
  in
  (fun (combine, _, new_row) -> List.length combine > 0, combine, List.rev new_row)
  @@ List.fold_left
    (fun (combine, prev, row) tile -> match prev with
      | None -> combine, Some tile, tile :: row
      | Some prev ->
        if Tile.compare prev tile && not (Tile.is_empty tile)
        then Tile.succ tile :: combine, Some Tile.empty, merge tile row
        else combine, Some tile, tile :: row)
    ([], None, []) row

let deflate_left row =
  let rec complete old_row new_row =
    if (List.length old_row) > (List.length new_row)
    then complete old_row (Tile.empty :: new_row)
    else new_row
  in
  (fun ((_, compress), new_row) -> compress, List.rev @@ complete row @@ new_row)
  @@ List.fold_left
    (fun ((ignore, compress), acc) tile ->
      if Tile.is_empty tile
      then ((ignore + 1, compress), acc)
      else ((0, (ignore > 0) || compress), tile :: acc))
    ((0, false), []) row

let ( >>= ) (ret, combine, grid, row) deflate_left =
  let (new_ret, new_row) = deflate_left row in
  (new_ret || ret, combine, grid, new_row)

let ( <!> ) (ret, combine, grid, row) combine_left =
  let (new_ret, new_combine, new_row) = combine_left row in
  (new_ret || ret, new_combine :: combine, grid, new_row)

let ( >|= ) (ret, combine, grid, row) deflate_left =
  let (new_ret, new_row) = deflate_left row in
  (new_ret || ret, combine, new_row :: grid)

let move_left grid =
  (fun (stat, combine, grid) -> stat, List.concat combine, List.rev grid)
  @@ List.fold_left (fun (stat, combine, grid) row ->
    (stat, combine, grid, row)
    >>= deflate_left
    <!> combine_left
    >|= deflate_left) (false, [], []) grid

let move_right grid =
  let (stat, combine, grid) = move_left (rotate @@ rotate @@ grid) in
  (stat, combine, rotate @@ rotate @@ grid)

let move_up grid =
  let (stat, combine, grid) = move_left (rotate @@ rotate @@ rotate @@ grid) in
  (stat, combine, rotate @@ grid)

let move_down grid =
  let (stat, combine, grid) = move_left (rotate @@ grid) in
  (stat, combine, rotate @@ rotate @@ rotate @@ grid)

let pp_print grid =
  let fmt = Format.std_formatter in
  Format.fprintf fmt "%a"
  (pp_grid (pp_row pp_tile))
  ((make' (fun _ -> "_") (List.length grid)), grid)
