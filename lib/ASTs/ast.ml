open Stdlib.Printf

type typ =
| Arrow of typ * typ
| Prod of typ * typ 
| Sum of typ * typ
| Unit

type term =
| Var of Variable.t
| Lambda of Variable.t * typ *  term
| Ap of term * term
| Triv
| Tup of term * term
| Split of term * ((Variable.t * Variable.t) * term)
| Check of term * term
| Inj of typ * int * term
| Case of term * (Variable.t * term) * (Variable.t * term)
(* A bit ad hoc, but I want print "foo" ; Tup [] : Prod [] *)
| Print of string * term

let rec pp_term e = 
  match e with 
  | Var x -> Variable.pp_var x
  | Lambda (x, _, e) -> "λ" ^  Variable.pp_var x ^ "." ^ pp_term e
  | Ap (e1, e2) -> "ap(" ^ pp_term e1 ^ "," ^ pp_term e2 ^ ")"
  | Tup (e1, e2) -> sprintf "<%s, %s>" (pp_term e1) (pp_term e2)
  | Split (e1, ((v1, v2), e2)) -> sprintf "split %s as %s, %s in %s" (pp_term e1) (Variable.pp_var v1) (Variable.pp_var v2) (pp_term e2)
  | Inj (_, i, e) -> "inj" ^ string_of_int i ^ " " ^ pp_term e
  | Case (t, (v1, m1), (v2, m2)) -> sprintf "case %s { %s => %s | %s => %s }" (pp_term t) (Variable.pp_var v1) (pp_term m1) (Variable.pp_var v2) (pp_term m2)
  | Print (s, e) -> "print " ^ s ^"; " ^ pp_term e 
  | Check (m, m1) -> "check " ^ (pp_term m) ^ " in " ^ (pp_term m1)
  | Triv -> "<>"

let rec pp_typ t =
  match t with
  | Arrow (t1, t2) -> pp_typ t1 ^ " -> " ^ pp_typ t2
  | Prod (t1, t2) -> sprintf "%s * %s" (pp_typ t1) (pp_typ t2)
  | Sum (t1, t2) -> sprintf "%s + %s" (pp_typ t1) (pp_typ t2)
  | Unit -> "unit"