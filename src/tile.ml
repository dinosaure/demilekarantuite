type t = Cnat.t

let empty = Cnat.zero
let one = Cnat.succ @@ Cnat.zero
let two = Cnat.succ @@ Cnat.succ @@ Cnat.zero

let is_empty = Cnat.is_zero
let succ = Cnat.succ
let compare = Cnat.eq

let to_string v =
  if is_empty v then "_"
  else string_of_int (Cnat.to_int (Cnat.pow (Cnat.of_int 2) v))

let to_int v =
  if is_empty v then 0
  else (Cnat.to_int (Cnat.pow (Cnat.of_int 2) v))
