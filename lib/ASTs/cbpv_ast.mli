module Context : Map.S with type key = Variable.t

type value_type = 
| Tensor of value_type list
| Sum of value_type list
| U of comp_type

and comp_type =
| Arr of value_type * comp_type
| F of value_type

type typ = CompTyp of comp_type | ValTyp of value_type

type value_term =
| Var of Variable.t
| TensorProd of value_term list
| Inj of value_type * int * value_term
| Susp of comp_term

and comp_term = 
| Ret of value_term
| Bind of comp_term * Variable.t * comp_term
| Lam of Variable.t * value_type * comp_term
| Ap of comp_term * value_term
| Force of value_term
| Split of value_term * (Variable.t list * comp_term)
| Case of value_term * (Variable.t * comp_term) list
| Print of string


type term = Comp of comp_term | Val of value_term


val pp_typ : typ -> string

val pp_term : term -> string
