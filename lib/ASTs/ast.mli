type typ =
| Arrow of typ * typ
| Prod of typ list 
| Sum of typ list

type term =
| Var of Variable.t
| Lambda of Variable.t * typ *  term
| Ap of term * term
| Tup of term list
| Split of term * (Variable.t list * term)
| Inj of typ * int * term
| Case of term * (Variable.t * term) list
| Print of string * term

val pp_typ : typ -> string
val pp_term : term -> string