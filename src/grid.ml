type t = Tile.t list list

type diff_update = [
  | `Add of (Position.t * Tile.t)
]

type diff_action = [
  | `Update of (Position.t * Tile.t)
  | `Move of (Position.t * Position.t * Tile.t)
]

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

let size grid = List.length grid

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
       (x + 1, snd @@ List.fold_left (fun (y, acc) tile ->
            (y + 1, f (x, y) acc tile)) (0, acc) row))
    (0, acc) grid

let available_cells =
  foldl (fun (x, y) acc tile ->
      if Tile.is_empty tile then (x, y) :: acc else acc) []

let is_full grid =
  match  available_cells grid with
  | [] -> true
  | _ -> false

let update grid =
  match available_cells grid with
  | [] -> raise Full_grid
  | cells ->
    let (x', y') = List.nth cells (Random.int (List.length cells)) in
    let new_tile = if Random.float 10. > 9. then Tile.two else Tile.one in
    (`Add (Position.make x' y', new_tile),
     mapi
       (fun (x, y) old_tile ->
          if x = x' && y = y'
          then new_tile
          else old_tile)
       grid)

let rotate grid =
  let size = List.length grid in
  mapi (fun (x, y) tile -> nth ((size - y - 1), x) grid) grid

let reduce_merge p_merge p_from t_merge diff =
  let rec aux acc update = function
    | [] -> List.rev @@
      if update then acc
      else (`Move (p_from, p_merge, t_merge) :: acc)
    | `Move (p1, p2, tile) :: r when Position.equal p2 p_from ->
      aux (`Move (p1, p_merge, tile) :: acc) true r
    | x :: r -> aux (x :: acc) update r
  in
  aux [] false diff

let combine_left position north diff row =
  let move position = Position.move
      position
      (Position.target north Position.East) in
  let merge value = function
    | [] -> []
    | x :: r -> Tile.empty :: (Tile.succ value) :: r
  in
  (fun (_, diff, _, new_row) ->
     diff, List.rev new_row)
  @@ List.fold_left
    (fun (position, diff, prev, row) tile -> match prev with
       | None -> move position,
                 diff,
                 Some tile,
                 tile :: row
       | Some prev ->
         if Tile.compare prev tile && not (Tile.is_empty tile)
         then move position,
              (let direction = Position.target north Position.West in
               let position' = Position.move position direction in
               `Update (position', Tile.succ tile)
               :: (reduce_merge position' position tile diff)),
              Some Tile.empty,
              merge tile row
         else move position,
              diff,
              Some tile,
              tile :: row)
    (position,
     diff, None, []) row

let reduce_move p_from p_to t_move diff =
  let rec aux acc update = function
    | [] -> List.rev @@
      if update then acc
      else (`Move (p_from, p_to, t_move) :: acc)
    | `Move (p1, p2, tile) :: r when Position.equal p2 p_from ->
      aux (`Move (p1, p_to, tile) :: acc) true r
    | x :: r -> aux (x :: acc) update r
  in
  aux [] false diff

let deflate_left position north diff row =
  let move position = Position.move
      position
      (Position.target north Position.East) in
  let rec complete old_row new_row =
    if (List.length old_row) > (List.length new_row)
    then complete old_row (Tile.empty :: new_row)
    else new_row
  in
  (fun (_, diff, (_, _, compress), new_row) ->
     diff, List.rev @@ complete row @@ new_row)
  @@ List.fold_left
    (fun (position, diff, (ignore, distance, compress), acc) tile ->
       if Tile.is_empty tile
       then (move position,
             diff,
             (ignore + 1, distance + 1, compress),
             acc)
       else (move position,
             (if ignore > 0 || compress
              then
                let direction = Position.target north Position.West in
                let position' = Position.nmove position distance direction in
                `Move (position, position', tile) :: diff
              else diff),
             (0, distance, (ignore > 0) || compress),
             tile :: acc))
    (position,
     diff, (0, 0, false), []) row

let ( >>= ) ((position, north), diff, grid, row) func =
  let (new_diff, new_row) = func position north diff row in
  ((position, north), new_diff, grid, new_row)

let ( >|= ) ((position, north), diff, grid, row) deflate_left =
  let move position = Position.move
      position
      (Position.target north Position.South) in
  let (new_diff, new_row) = deflate_left position north diff row in
  ((move position, north), new_diff, new_row :: grid)

let move_left' ?(north=Position.North) ?(origin=Position.origin) grid =
  (fun (_, diff, grid) -> List.rev diff, List.rev grid)
  @@ List.fold_left (fun ((position, north), diff, grid) row ->
      ((position, north), diff, grid, row)
      >>= deflate_left
      >>= combine_left
      >|= deflate_left) ((origin, north), [], []) grid

let move_left grid = move_left' grid

let move_right grid =
  let (diff, grid) = move_left'
      ~north:Position.South
      ~origin:(Position.make (size grid - 1) (size grid - 1))
      (rotate @@ rotate @@ grid) in
  (diff, rotate @@ rotate @@ grid)

let move_up grid =
  let (diff, grid) = move_left'
      ~north:Position.East
      ~origin:(Position.make 0 (size grid - 1))
      (rotate @@ rotate @@ rotate @@ grid) in
  (diff, rotate @@ grid)

let move_down grid =
  let (diff, grid) = move_left'
      ~north:Position.West
      ~origin:(Position.make (size grid - 1) 0)
      (rotate @@ grid) in
  (diff, rotate @@ rotate @@ rotate @@ grid)

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

let pp_print grid =
  let fmt = Format.std_formatter in
  Format.fprintf fmt "%a"
    (pp_grid (pp_row pp_tile))
    ((make' (fun _ -> "_") (List.length grid)), grid)
