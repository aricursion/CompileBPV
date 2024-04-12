type typ =
| Arrow of typ * typ
| Prod of typ list 
| Sum of typ list
| Comp of typ

type term =
| Lambda of Variable.t * typ * term
| Ap of term * term
| Tup of term list
| Proj of int * term
| Inj of typ * int * term
| Case of term * (Variable.t * term) list

type exp = 
| Bind of exp * Variable.t * exp
| Ret of term
| Print of string 