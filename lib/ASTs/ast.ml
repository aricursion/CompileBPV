type typ =
| Arrow of typ * typ
| Prod of typ list 
| Sum of typ list

type term =
| Var of Variable.t
| Lambda of Variable.t * typ *  term
| Ap of term * term
| Tup of term list
| Split of term * (Variable.t list * term)
| Inj of typ * int * term
| Case of term * (Variable.t * term) list
(* A bit ad hoc, but I want print "foo" ; Tup [] : Prod [] *)
| Print of string * term

let rec pp_term e = 
  match e with 
  | Var x -> Variable.pp_var x
  | Lambda (x, _, e) -> "\\" ^  Variable.pp_var x ^ "." ^ pp_term e
  | Ap(e1, e2) -> "ap(" ^ pp_term e1 ^ "," ^ pp_term e2 ^ ")"
  | Tup ts -> "<" ^ String.concat "," (List.map pp_term ts) ^ ">"
  | Split (e1, (vs, e2)) -> "split(" ^ pp_term e1 ^ " " ^ String.concat "." (List.map Variable.pp_var vs) ^ ". " ^ pp_term e2 ^ ")"
  | Inj (_, i, e) -> "inj" ^ string_of_int i ^ " " ^ pp_term e
  | Case (t, arms) -> "case(" ^ pp_term t ^ ";" ^ String.concat ";" (List.map (fun (x, e) -> Variable.pp_var x ^ "." ^ pp_term e) arms) ^ ")"
  | Print (s, e) -> "print " ^ s ^"; " ^ pp_term e 

let rec pp_typ t =
  match t with
  | Arrow (t1, t2) -> pp_typ t1 ^ " -> " ^ pp_typ t2
  | Prod ts -> 
    (match ts with
    | [] -> "unit"
    | _ -> String.concat " x " (List.map pp_typ ts))
  | Sum ts -> 
    (match ts with
    | [] -> "void" 
    | _ -> String.concat " + " (List.map pp_typ ts))