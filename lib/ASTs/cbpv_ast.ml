type value_type = 
| Tensor of value_type * value_type
| Sum of value_type * value_type
| Unit
| U of comp_type

and comp_type =
| Arr of value_type * comp_type
| F of value_type

type typ = CompTyp of comp_type | ValTyp of value_type

type value_term =
| Var of Variable.t
| TensorProd of value_term * value_term
| Triv
| Inj of value_type * int * value_term
| Susp of comp_term

and comp_term = 
| Ret of value_term
| Bind of comp_term * Variable.t * comp_term
| Lam of Variable.t * value_type * comp_term
| Ap of comp_term * value_term
| Force of value_term
| Split of value_term * ((Variable.t * Variable.t) * comp_term)
| Case of value_term * (Variable.t * comp_term) * (Variable.t * comp_term)
| Check of value_term * comp_term
| Print of string

type term = Comp of comp_term | Val of value_term


let rec pp_val_typ (t: value_type) =
  match t with 
  | Tensor (t1, t2) -> pp_val_typ t1 ^ " x " ^ pp_val_typ t2
  | Sum (t1, t2) -> pp_val_typ t1 ^ " + " ^ pp_val_typ t2
  | Unit -> "Unit"
  | U t -> "U(" ^ pp_comp_typ t ^ ")"

and pp_comp_typ (t : comp_type) =
  match t with 
  | Arr (t1, t2) -> pp_val_typ t1 ^ " -> " ^ pp_comp_typ t2
  | F t1 -> "F(" ^ pp_val_typ t1 ^ ")"

let pp_typ t =
  match t with
  | CompTyp t -> pp_comp_typ t
  | ValTyp t -> pp_val_typ t 

let rec pp_val_term v =
  match v with 
  | Var x -> Variable.pp_var x
  | TensorProd (v1, v2) -> "(" ^ pp_val_term v1 ^ "," ^ pp_val_term v2 ^ ")"
  | Triv -> "()"
  | Inj (_, i, v) -> "inj" ^ string_of_int i ^ "(" ^ pp_val_term v ^")"
  | Susp c -> "Susp(" ^ pp_comp_term c ^ ")"

and pp_comp_term c = 
  match c with
  | Ret v -> "Ret(" ^ pp_val_term v ^ ")"
  | Bind (c, _, c1) -> "Bind(" ^ pp_comp_term c ^ ";" ^ " " ^ pp_comp_term c1 ^ ")"
  | Lam (x, t, c) -> "\\(" ^ Variable.pp_var x ^ ":" ^ pp_val_typ t ^ ")." ^ pp_comp_term c
  | Ap (c1, v) -> "Ap(" ^ pp_comp_term c1 ^ "," ^ pp_val_term v ^ ")"
  | Force v -> "Force(" ^ pp_val_term v ^ ")"
  | Split (v, ((x, y), c)) -> "Split(" ^ pp_val_term v ^ "; " ^ Variable.pp_var x ^ "." ^ Variable.pp_var y ^ "." ^ pp_comp_term c ^ ")"
  | Case (v, (x, c1), (y, c2)) -> "Case(" ^ pp_val_term v ^ "; " ^ Variable.pp_var x ^ "." ^ pp_comp_term c1 ^ "; " ^ Variable.pp_var y ^ "." ^ pp_comp_term c2 ^ ")"
  | Check (v, c) -> "Check(" ^ pp_val_term v ^ ";" ^ pp_comp_term c ^ ")"
  | Print s -> "Print(" ^ s ^ ")"

let pp_term t = 
  match t with
  | Comp t -> pp_comp_term t
  | Val t -> pp_val_term t