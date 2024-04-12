open Lax_ast

let rec trans_typ (t : Ast.typ) = 
  match t with
  | Ast.Arrow (t1, t2) -> Arrow (trans_typ t1, trans_typ t2)
  | Ast.Prod ts -> Prod (List.map trans_typ ts)
  | Ast.Sum ts -> Sum (List.map trans_typ ts)

let rec elab m =
  match m with
  | Ast.Lambda (x, t, e) -> Comp (Ret (Lambda(x, trans_typ t, elab e)))
  | Ast.Ap (e1, e2) -> Comp (Ret (Ap (elab e1, elab e2)))
  | Ast.Tup ts -> Comp (Ret (Tup (List.map elab ts)))
  | Ast.Proj (i, t) -> Comp (Ret (Proj(i, elab t)))
  | Ast.Inj (t, i, e) -> Comp (Ret (Inj (trans_typ t, i, elab e)))
  | Ast.Case (e, arms) -> Comp (Ret (Case (elab e, List.map (fun (x, e) -> (x, elab e)) arms)))
  | Ast.Print (s, t) -> let x = Variable.new_var () in Comp (Bind (Comp (Print s), x, Ret (elab t)))