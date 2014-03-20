type t

val empty: t
val one: t
val two: t
val is_empty: t -> bool
val succ: t -> t
val compare: t -> t -> bool
val to_string: t -> string
val to_int: t -> int
