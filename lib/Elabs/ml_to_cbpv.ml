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
  | Ast.Lambda (x, t, e) -> Lam (x, trans_typ t, elab e)
  | Ast.Ap (e1, e2) -> 
    let x = Variable.new_var() in
    Bind (elab e2, x, Ap(elab e1, Var x))
  | Ast.Triv -> Ret Triv
  | Ast.Tup (e1, e2) -> 
      let x = Variable.new_var() in
      let y = Variable.new_var() in 
      Bind (elab e1, x, Bind (elab e2, y, Ret (TensorProd (Var x, Var y))))
  | Ast.Split (e, ((x, y), e')) -> 
      let z = Variable.new_var() in 
      Bind (elab e, z, Split (Var z, ((x, y), elab e')))
  | Ast.Check (e1, e2) ->
      let z = Variable.new_var() in 
      Bind (elab e1, z, Check(Var z, elab e2))
  | Ast.Inj (t, i, e) -> 
      let z = Variable.new_var() in
      Bind (elab e, z, Ret (Inj(trans_typ t, i, Var z)))
  | Ast.Case (e, (x, e1), (y, e2)) ->
      let z = Variable.new_var() in 
      Bind(elab e, z, Case(Var z, (x, elab e1), (y, elab e2)))
  | Ast.Print (s, e) -> Bind (Print s, Variable.new_var(), elab e)