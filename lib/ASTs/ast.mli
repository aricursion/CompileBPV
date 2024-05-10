type typ =
  | Arrow of typ * typ
  | Prod of typ * typ
  | Sum of typ * typ
  | Int_Typ
  | String_Typ
  | Unit

type term =
  | Var of Variable.t
  | Lambda of Variable.t * typ * term
  | Ap of term * term
  | Triv
  | Tup of term * term
  | Split of term * ((Variable.t * Variable.t) * term)
  | Check of term * term
  | Inj of typ * int * term
  | Case of term * (Variable.t * term) * (Variable.t * term)
  | Let of Variable.t * term * term
  | Prim of Prim.prim * term list
  | Int of int
  | String of string

val pp_typ : typ -> string
val pp_term : term -> string
