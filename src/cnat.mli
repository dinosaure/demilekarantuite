type t

val zero: t
val succ: t -> t
val pred: t -> t

val is_zero: t -> bool
val lt: t -> t -> bool
val eq: t -> t -> bool

val sum: t -> t -> t
val mul: t -> t -> t
val pow: t -> t -> t

val of_int: int -> t
val to_int: t -> int
