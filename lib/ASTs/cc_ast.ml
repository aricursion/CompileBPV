module Context = Map.Make(Variable)

module TContext = Set.Make(Variable)

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

let rec pp_val_typ (t: value_type) =
  match t with 
  | Tensor ts ->  
      if List.length ts = 0 then "unit" else String.concat " ⊗  " (List.map pp_val_typ ts)
  | Sum sums -> String.concat " + " (List.map pp_val_typ sums)
  | Tvar t -> Variable.pp_var t
  | Exists (t, tau) -> Printf.sprintf "∃(%s. %s)" (Variable.pp_var t) (pp_val_typ tau)
  | UU t -> "UU(" ^ pp_comp_typ t ^ ")"

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
  | TensorProd vs -> "<" ^ String.concat "," (List.map pp_val_term vs) ^ ">"
  | Inj (_, i, v) -> Printf.sprintf "inj%d (%s)" i (pp_val_term v)
  | Close c -> "Close(" ^ pp_comp_term c ^ ")"
  | Pack ((t, tau), r, v) -> Printf.sprintf "Pack[%s.%s](%s; %s)" (Variable.pp_var t) (pp_typ (ValTyp tau)) (pp_val_typ r) (pp_val_term v)

and pp_comp_term c = 
  match c with
  | Ret v -> "Ret(" ^ pp_val_term v ^ ")"
  | Bind (c, x, c1) -> "Bind(" ^ pp_comp_term c ^ "; " ^ Variable.pp_var x ^ "." ^ pp_comp_term c1 ^ ")"
  | CLet (x, v, c) -> Printf.sprintf "CLet(%s = %s in %s)" (Variable.pp_var x) (pp_val_term v) (pp_comp_term c)
  | Lam (x, t, c) -> "λ(" ^ Variable.pp_var x ^ ":" ^ pp_val_typ t ^ ")." ^ pp_comp_term c
  | Ap (c1, v) -> "Ap(" ^ pp_comp_term c1 ^ "," ^ pp_val_term v ^ ")"
  | Open v -> "Open(" ^ pp_val_term v ^ ")"
  | Split (v, (vars, c)) -> "Split(" ^ pp_val_term v ^ "; " ^  String.concat "." (List.map Variable.pp_var vars) ^"." ^ pp_comp_term c ^ ")"
  | Case (v, arms) -> "Case(" ^ pp_val_term v ^ "; " ^ ";" ^ String.concat ";" (List.map (fun (x, c) -> Variable.pp_var x^ "." ^ pp_comp_term c) arms)^ ")"
  | Print s -> "Print(" ^ s ^ ")"
  | Unpack (v, (x, c)) -> Printf.sprintf "Unpack(%s as %s in %s)" (pp_val_term v) (Variable.pp_var x) (pp_comp_term c)

let pp_term t = 
  match t with
  | Comp t -> pp_comp_term t
  | Val t -> pp_val_term t