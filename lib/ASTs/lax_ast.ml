type typ =
| Arrow of typ * typ
| Prod of typ list 
| Sum of typ list
| TComp of typ

type term =
| Var of Variable.t
| Lambda of Variable.t * typ * term
| Ap of term * term
| Tup of term list
| Proj of int * term
| Inj of typ * int * term
| Case of term * (Variable.t * term) list
| Comp of exp

and exp = 
| Bind of term * Variable.t * exp
| Ret of term
| Print of string 