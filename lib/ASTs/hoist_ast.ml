module Context = Map.Make (Variable)
module TContext = Set.Make (Variable)

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

let rec pp_val_typ (t : value_type) =
  match t with
  | Tensor ts ->
      if List.length ts = 0 then "unit"
      else String.concat " ⊗  " (List.map pp_val_typ ts)
  | Sum sums -> String.concat " + " (List.map pp_val_typ sums)
  | Tvar t -> Variable.pp_var t
  | Exists (t, tau) ->
      Printf.sprintf "∃(%s. %s)" (Variable.pp_var t) (pp_val_typ tau)
  | UU t -> "𝕌(" ^ pp_comp_typ t ^ ")"
  | Int_Typ -> "int"
  | String_Typ -> "string"

and pp_comp_typ (t : comp_type) =
  match t with
  | Arr (t1, t2) -> pp_val_typ t1 ^ " -> " ^ pp_comp_typ t2
  | F t1 -> "F(" ^ pp_val_typ t1 ^ ")"
  | Forall (t, c) -> Printf.sprintf "∀%s.%s" (Variable.pp_var t) (pp_comp_typ c)
  | Tapp (c, v) -> Printf.sprintf "%s[%s]" (pp_comp_typ c) (pp_val_typ v)

let rec pp_val_term v =
  match v with
  | Var x -> Variable.pp_var x
  | TensorProd vs -> "<" ^ String.concat "," (List.map pp_val_term vs) ^ ">"
  | Inj (_, i, v) -> Printf.sprintf "inj%d (%s)" i (pp_val_term v)
  | Close c -> "Close(" ^ pp_comp_term c ^ ")"
  | Pack ((t, tau), r, v) ->
      Printf.sprintf "Pack[%s.%s](%s; %s)" (Variable.pp_var t) (pp_val_typ tau)
        (pp_val_typ r) (pp_val_term v)
  | Int i -> string_of_int i
  | String s -> s

and pp_comp_term c =
  match c with
  | Ret v -> "Ret(" ^ pp_val_term v ^ ")"
  | Bind (c, x, c1) ->
      "Bind(" ^ pp_comp_term c ^ "; " ^ Variable.pp_var x ^ "."
      ^ pp_comp_term c1 ^ ")"
  | Ap (c1, v) -> "Ap(" ^ pp_comp_term c1 ^ "," ^ pp_val_term v ^ ")"
  | Open v -> "Open(" ^ pp_val_term v ^ ")"
  | Split (v, (vars, c)) ->
      if List.length vars > 0 then
        "Split(" ^ pp_val_term v ^ "; "
        ^ String.concat "." (List.map Variable.pp_var vars)
        ^ "." ^ pp_comp_term c ^ ")"
      else "Check(" ^ pp_val_term v ^ "; " ^ pp_comp_term c ^ ")"
  | Case (v, arms) ->
      "Case(" ^ pp_val_term v ^ "; " ^ ";"
      ^ String.concat ";"
          (List.map
             (fun (x, c) -> Variable.pp_var x ^ "." ^ pp_comp_term c)
             arms)
      ^ ")"
  | Unpack (v, (x, c)) ->
      Printf.sprintf "Unpack(%s as %s in %s)" (pp_val_term v)
        (Variable.pp_var x) (pp_comp_term c)
  | Prim (p, t) ->
      Printf.sprintf "%s[%s]" (Prim.pp_prim p)
        (String.concat ", " (List.map pp_val_term t))

and pp_func f =
  match f with
  | Lam (x, t, c) ->
      "λ(" ^ Variable.pp_var x ^ ":" ^ pp_val_typ t ^ ")." ^ pp_comp_term c
  | Flam (x, c) -> "Λ" ^ Variable.pp_var x ^ "." ^ pp_comp_term c

and pp_program p =
  match p with
  | Pbody c -> pp_comp_term c
  | Plet (f, func, p) ->
      Printf.sprintf "let %s = %s in %s" (Variable.pp_var f) (pp_func func)
        (pp_program p)
