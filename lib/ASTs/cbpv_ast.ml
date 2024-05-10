module Context = Map.Make (Variable)

type value_type =
  | Tensor of value_type list
  | Sum of value_type list
  | Int_Typ
  | String_Typ
  | U of comp_type

and comp_type = Arr of value_type * comp_type | F of value_type

type typ = CompTyp of comp_type | ValTyp of value_type

type value_term =
  | Var of Variable.t
  | TensorProd of value_term list
  | Inj of value_type * int * value_term
  | Susp of comp_term
  | Int of int
  | String of string

and comp_term =
  | Ret of value_term
  | Bind of comp_term * Variable.t * comp_term
  | Lam of Variable.t * value_type * comp_term
  | Ap of comp_term * value_term
  | Force of value_term
  | Split of value_term * (Variable.t list * comp_term)
  | Case of value_term * (Variable.t * comp_term) list
  | Prim of Prim.prim * value_term list

type term = Comp of comp_term | Val of value_term

let rec pp_val_typ (t : value_type) =
  match t with
  | Tensor ts ->
      if List.length ts = 0 then "unit"
      else String.concat " ⊗  " (List.map pp_val_typ ts)
  | Sum sums -> String.concat " + " (List.map pp_val_typ sums)
  | U t -> "U(" ^ pp_comp_typ t ^ ")"
  | Int_Typ -> "int"
  | String_Typ -> "string"

and pp_comp_typ (t : comp_type) =
  match t with
  | Arr (t1, t2) -> pp_val_typ t1 ^ " -> " ^ pp_comp_typ t2
  | F t1 -> "F(" ^ pp_val_typ t1 ^ ")"

let pp_typ t =
  match t with CompTyp t -> pp_comp_typ t | ValTyp t -> pp_val_typ t

let rec pp_val_term v =
  match v with
  | Var x -> Variable.pp_var x
  | TensorProd vs -> "<" ^ String.concat "," (List.map pp_val_term vs) ^ ">"
  | Inj (_, i, v) -> Printf.sprintf "inj%d (%s)" i (pp_val_term v)
  | Susp c -> "Susp(" ^ pp_comp_term c ^ ")"
  | Int i -> string_of_int i
  | String s -> s

and pp_comp_term c =
  match c with
  | Ret v -> "Ret(" ^ pp_val_term v ^ ")"
  | Bind (c, x, c1) ->
      "Bind(" ^ pp_comp_term c ^ "; " ^ Variable.pp_var x ^ "."
      ^ pp_comp_term c1 ^ ")"
  | Lam (x, t, c) ->
      "λ(" ^ Variable.pp_var x ^ ":" ^ pp_val_typ t ^ ")." ^ pp_comp_term c
  | Ap (c1, v) -> "Ap(" ^ pp_comp_term c1 ^ "," ^ pp_val_term v ^ ")"
  | Force v -> "Force(" ^ pp_val_term v ^ ")"
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
  | Prim (p, t) ->
      Printf.sprintf "%s[%s]" (Prim.pp_prim p)
        (String.concat ", " (List.map pp_val_term t))

let pp_term t = match t with Comp t -> pp_comp_term t | Val t -> pp_val_term t
