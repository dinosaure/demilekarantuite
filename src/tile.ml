type t = int

let empty = 0
let one = 1
let two = 2

let is_empty = ((=) 0)
let succ = ((+) 1)
let compare = (=)

let rec pow a n =
  if n = 0 then 1
  else if n = 1 then a
  else
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let to_string v =
  if is_empty v then "_"
  else string_of_int (pow 2 v)

let to_int v =
  if is_empty v then 0
  else pow 2 v
