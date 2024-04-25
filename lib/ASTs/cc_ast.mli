module Context: Map.S with type key = Variable.t

module TContext : Set.S with type elt = Variable.t

type value_type = 
| Tensor of value_type list
| Sum of value_type list
| Tvar of Variable.t
| Exists of Variable.t * value_type
| UU of comp_type

and comp_type =
| Arr of value_type * comp_type
| F of value_type

type typ = CompTyp of comp_type | ValTyp of value_type

type value_term =
| Var of Variable.t
| TensorProd of value_term list
| Inj of value_type * int * value_term
| Pack of (Variable.t * value_type) * value_type * value_term
| Close of comp_term

and comp_term = 
| Ret of value_term
| Bind of comp_term * Variable.t * comp_term
| CLet of Variable.t * value_term * comp_term
| Lam of Variable.t * value_type * comp_term
| Ap of comp_term * value_term
| Split of value_term * (Variable.t list * comp_term)
| Case of value_term * (Variable.t * comp_term) list
| Print of string
| Unpack of value_term * (Variable.t * comp_term)
| Open of value_term

type term = Comp of comp_term | Val of value_term

val pp_typ : typ -> string

val pp_term : term -> string
