open Lax_ast

let rec trans_typ (t : Ast.typ) = 
  match t with
  | Ast.Arrow (t1, t2) -> Arrow (trans_typ t1, trans_typ t2)
  | Ast.Prod (t1, t2) -> Prod (trans_typ t1, trans_typ t2)
  | Ast.Sum (t1, t2) -> Sum (trans_typ t1, trans_typ t2)
  | Ast.Unit -> Unit

let rec elab m =
  match m with
  | Ast.Var x -> Var x
  | Ast.Lambda (x, t, e) -> Lambda (x, trans_typ t, elab e)
  | Ast.Ap (e1, e2) -> Ap (elab e1, elab e2)
  | Ast.Triv -> Triv
  | Ast.Tup (e1, e2) -> Tup (elab e1, elab e2)
  | Ast.Split (m, (vars, e)) -> Split (elab m, (vars, elab e))
  | Ast.Inj (t, i, e) -> Inj (trans_typ t, i, elab e)
  | Ast.Case (e, (v1, m1), (v2, m2)) -> Case (elab e, (v1, elab m1), (v2, elab m2))
  | Ast.Check (e, e1) -> Check (elab e, elab e1)
  | Ast.Print (s, t) -> let x = Variable.new_var () in Comp (Bind (Comp (Print s), x, Ret (elab t)))
