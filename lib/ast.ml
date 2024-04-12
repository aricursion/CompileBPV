type typ =
| Arrow of typ * typ
| Prod of typ list 
| Sum of typ list

type term =
| Lambda of Variable.t * typ * term
| Ap of term * term
| Tup of term list
| Proj of int * term
| Inj of typ * int * term
| Case of term * (Variable.t * term) list
| Print of string