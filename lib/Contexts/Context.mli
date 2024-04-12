type 'a t

val empty : unit -> 'a t

val insert : 'a t -> Variable.t -> 'a -> unit

val lookup : 'a t -> Variable.t -> 'a option

val remove : 'a t -> Variable.t -> 'a option