type t

val new_var : unit -> t
val of_string : string -> t
val compare : t -> t -> int
val pp_var : t -> string
