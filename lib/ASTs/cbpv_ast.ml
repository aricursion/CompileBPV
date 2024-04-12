type value_type = 
| Tensor of value_type list
| Sum of value_type list
| Susp of comp_type

(* I'm not sure if we need negative products *)
and comp_type =
| Times of value_type list
| Arr of value_type * comp_type


type value_term =
| Var of Variable.t
| Tensor of value_term list
| Inj of value_type * int
| Susp of comp_term

and comp_term = 
| Ret of value_term
| Bind of comp_term * Variable.t * comp_term
| Tuple of comp_term list
| Proj of int * comp_term
| Lam of Variable.t * comp_term
| Ap of comp_term * value_term
| Force of value_term
| Split of value_term * Variable.t list * comp_term
| Case of value_term * (Variable.t * comp_term) list
