open Cbpv_ast

let rec trans_typ (t : Ast.typ) =
  match t with
  | Ast.Arrow(t1, t2) -> U(Arr(trans_typ t1, F (trans_typ t2)))
  | Ast.Prod (t1, t2) -> U (F (Tensor (trans_typ t1, trans_typ t2)))
  | Ast.Sum (t1, t2) -> U (F (Sum (trans_typ t1, trans_typ t2)))
  | Ast.Unit -> U (F (Unit))

let rec elab (e : Ast.term) =
  match e with 
  | Ast.Var x -> Var x
  | Ast.Lambda (x, t, e) -> Susp (Lam (x, trans_typ t, Ret (elab e)))
  | Ast.Ap (e1, e2) -> Susp (Ap (Force (elab e1), elab e2))
  | Ast.Triv -> Triv
  | Ast.Tup (e1, e2) -> TensorProd(elab e1, elab e2)
  | Ast.Split (e, ((x, y), e')) -> Susp (Split (elab e, ((x, y), Ret (elab e'))))
  | Ast.Check (e1, e2) -> Susp (Check (elab e1, Force (elab e2)))
  | Ast.Inj (t, i, e) -> Inj (trans_typ t, i, elab e)
  | Ast.Case (e, (x, e1), (y, e2)) -> Susp (Case (elab e, (x, Ret (elab e1)), (y, Ret (elab e2))))
  | Ast.Print (s, e) -> Susp (Bind(Print s, Variable.new_var(), Ret (elab e)))