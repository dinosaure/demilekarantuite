type t = { f : 'a. ('a -> 'a) -> 'a -> 'a }

let zero = { f = (fun _ x -> x) }
let succ { f = n } = { f = (fun f x -> n f (f x)) }
let pred { f = n } = fst (n (fun (_, b) -> (b, succ b)) (zero, zero))

let is_zero { f = n } = n (fun _ -> false) true

let rec lt n n' =
  if is_zero n then true
  else if is_zero n' then false
  else lt (pred n) (pred n')
let eq n n' = (&&) (lt n n') (lt n' n)

let sum { f = n } { f = n' } = { f = (fun f x -> n f (n' f x)) }
let mul { f = n } { f = n' } = { f = (fun f x -> n (n' f) x) }
let pow n { f = n' } = n' (fun x -> mul n x) (succ zero)

let rec of_int = function
  | 0 -> zero
  | n -> succ (of_int (n - 1))
let to_int { f = n } = n ((+) 1) 0
