type t = int * int

type direction =
  | North
  | South
  | East
  | West

type rotate =
  | Left
  | Right

let make x y = (x, y)
let origin = (0, 0)

let x (a, b) = a
let y (a, b) = b

let move (x, y) = function
  | North -> (x - 1, y)
  | South -> (x + 1, y)
  | East -> (x, y + 1)
  | West -> (x, y - 1)

let to_idx = function
  | East -> 0
  | North -> 1
  | West -> 2
  | South -> 3

let of_idx = function
  | 0 -> East
  | 1 -> North
  | 2 -> West
  | 3 -> South
  | _ -> raise (Invalid_argument "Position.of_idx")

let target north direction =
  let incr i = if i = 3 then 0 else i + 1 in
  let decr i = if i = 0 then 3 else i - 1 in
  let rec rotone i n =
    if n > 0 then rotone (incr i) (n - 1)
    else if n < 0 then rotone (decr i) (n + 1)
    else i in
  of_idx (rotone (to_idx direction) (to_idx north - (to_idx North)))
