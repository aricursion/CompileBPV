type typ =
| Arrow of typ * typ
| Prod of typ * typ 
| Sum of typ * typ
| Unit

type term =
| Var of Variable.t
| Lambda of Variable.t * typ *  term
| Ap of term * term
| Tup of term * term
| Split of term * ((Variable.t * Variable.t) * term)
| Check of term * term
| Inj of typ * int * term
| Case of term * (Variable.t * term) * (Variable.t * term)
| Print of string * term

val pp_typ : typ -> string
val pp_term : term -> string