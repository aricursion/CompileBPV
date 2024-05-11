module Context : Map.S with type key = Variable.t
module TContext : Set.S with type elt = Variable.t

type value_type =
  | Tensor of value_type list
  | Sum of value_type list
  | Tvar of Variable.t
  | Exists of Variable.t * value_type
  | Int_Typ
  | String_Typ
  | UU of comp_type

and comp_type =
  | Arr of value_type * comp_type
  | F of value_type
  | Forall of Variable.t * comp_type
  | Tapp of comp_type * value_type

type value_term =
  | Var of Variable.t
  | TensorProd of value_term list
  | Inj of value_type * int * value_term
  | Pack of (Variable.t * value_type) * value_type * value_term
  | Close of comp_term
  | Int of int
  | String of string

and comp_term =
  | Ret of value_term
  | Bind of comp_term * Variable.t * comp_term
  | Ap of comp_term * value_term
  | Split of value_term * (Variable.t list * comp_term)
  | Case of value_term * (Variable.t * comp_term) list
  | Unpack of value_term * (Variable.t * comp_term)
  | Open of value_term
  | Prim of Prim.prim * value_term list

and func =
  | Lam of Variable.t * value_type * comp_term
  | Flam of Variable.t * comp_term

and program = Pbody of comp_term | Plet of Variable.t * func * program

val pp_val_typ : value_type -> string
val pp_comp_typ : comp_type -> string
val pp_val_term : value_term -> string
val pp_comp_term : comp_term -> string
val pp_func : func -> string
val pp_program : program -> string
