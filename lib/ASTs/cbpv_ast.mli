type value_type = 
| Tensor of value_type * value_type
| Sum of value_type * value_type
| Unit
| Susp of comp_type

and comp_type =
| Times of value_type * value_type
| Arr of value_type * comp_type


type value_term =
| Var of Variable.t
| Tensor of value_term * value_term
| Inj of value_type * int * value_term
| Susp of comp_term

and comp_term = 
| Ret of value_term
| Bind of comp_term * Variable.t * comp_term
| Tuple of comp_term * comp_term
| Proj of int * comp_term
| Lam of Variable.t * comp_term
| Ap of comp_term * value_term
| Force of value_term
| Split of value_term * (Variable.t * Variable.t) * comp_term
| Case of value_term * (Variable.t * comp_term) * (Variable.t * comp_term)
| Check of value_term * comp_term
