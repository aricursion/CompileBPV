open Lax_ast

let rec trans_typ (t : Ast.typ) = 
  match t with
  | Ast.Arrow (t1, t2) -> Arrow (trans_typ t1, trans_typ t2)
  | Ast.Prod ts -> Prod (List.map trans_typ ts)
  | Ast.Sum ts -> Sum (List.map trans_typ ts)

let rec elab m =
  match m with
  | Ast.Var x -> Var x
  | Ast.Lambda (x, t, e) -> Lambda (x, trans_typ t, elab e)
  | Ast.Ap (e1, e2) -> Ap (elab e1, elab e2)
  | Ast.Tup ts -> Tup (List.map elab ts)
  | Ast.Proj (i, t) -> Proj(i, elab t)
  | Ast.Inj (t, i, e) -> Inj (trans_typ t, i, elab e)
  | Ast.Case (e, arms) -> Case (elab e, List.map (fun (x, e) -> (x, elab e)) arms)
  | Ast.Print (s, t) -> let x = Variable.new_var () in Comp (Bind (Comp (Print s), x, Ret (elab t)))