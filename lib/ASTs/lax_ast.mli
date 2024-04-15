type typ =
| Arrow of typ * typ
| Prod of typ * typ 
| Sum of typ * typ
| Unit
| TComp of typ

type term =
| Var of Variable.t
| Lambda of Variable.t * typ * term
| Ap of term * term
| Triv
| Tup of term * term
| Split of term * ((Variable.t * Variable.t) * term)
| Inj of typ * int * term
| Case of term * (Variable.t * term) * (Variable.t * term)
| Check of term * term
| Comp of exp

and exp = 
| Bind of term * Variable.t * exp
| Ret of term
| Print of string 