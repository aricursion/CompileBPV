open Cbpv_ast

let rec trans_typ (t : Ast.typ) =
  match t with
  | Ast.Arrow(t1, t2) -> U(Arr(trans_typ t1, F (trans_typ t2)))
  | Ast.Prod (t1, t2) -> Tensor (trans_typ t1, trans_typ t2)
  | Ast.Sum (t1, t2) -> Sum (trans_typ t1, trans_typ t2)
  | Ast.Unit -> Unit

let rec elab (e : Ast.term) =
  match e with 
  | Ast.Var x -> Ret (Var x)
  | Ast.Lambda (x, t, e) -> Ret (Susp (Lam (x, trans_typ t, elab e)))
  | Ast.Ap (e1, e2) -> Ap (elab e1, Susp (elab e2))
  | Ast.Triv -> Ret Triv
  | Ast.Tup (e1, e2) -> Ret (Tensor (Susp (elab e1), Susp (elab e2)))
  | Ast.Split (e, ((x, y), e')) -> Split (Susp (elab e), ((x, y), elab e'))
  | Ast.Check (e1, e2) -> Check (Susp (elab e1), elab e2)
  | Ast.Inj (t, i, e) -> Ret (Inj (trans_typ t, i, Susp (elab e)))
  | Ast.Case (e, (x, e1), (y, e2)) -> Ret Triv
  | Ast.Print (s, e) -> Ret Triv

