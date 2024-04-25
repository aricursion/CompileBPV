open Cc_ast

type ctx = { typCtx : TContext.t; valCtx : value_type Context.t }

(* this is just checking structural congruence and not exact equality because types variables are hard *)
let rec valTypEqual (t : value_type) (t' : value_type) =
  (match (t, t') with
  | (Tensor ts, Tensor ts') -> List.fold_left (fun acc (t1, t2) -> valTypEqual t1 t2 && acc) true (List.combine ts ts')
  | (Sum ts, Sum ts') -> List.fold_left (fun acc (t1, t2) -> valTypEqual t1 t2 && acc) true (List.combine ts ts')
  | (Tvar _, Tvar _) -> true
  | (Exists (_, tau1), Exists (_, tau2)) -> valTypEqual tau1 tau2
  | (UU c1, UU c2) -> compTypEqual c1 c2
  | _ -> false)

and compTypEqual (t : comp_type) (t' : comp_type) =
  (match (t, t') with
  | (Arr (t1, t2), Arr (t1', t2')) -> valTypEqual t1 t1' && compTypEqual t2 t2'
  | (F t, F t') -> valTypEqual t t'
  | _ -> false)

let rec substValTyp (tau : value_type) (t : Variable.t) (tau' : value_type) : value_type =
  (match tau' with
  | Tensor ts -> Tensor (List.map (substValTyp tau t) ts)
  | Sum ts -> Sum (List.map (substValTyp tau t) ts)
  | Tvar t' -> if Variable.compare t t' = 0 then tau else tau'
  | Exists (t', tau'') -> Exists (t', substValTyp tau t tau'')
  | UU ct -> UU (substCompTyp tau t ct))

and substCompTyp (tau : value_type) (t : Variable.t) (ct : comp_type) : comp_type = 
  (match ct with
  | Arr (tau', ct') -> Arr (substValTyp tau t tau', substCompTyp tau t ct')
  | F tau' -> F (substValTyp tau t tau'))

let rec isValType (ctx : ctx) (t : value_type) : (unit, string) result =
  match t with
  | Tensor ts ->
    let tys = List.map (isValType ctx) ts in
    (match Base.Result.all tys with
    | Ok _ -> Ok(())
    | Error e -> Error e)
  | Sum ts -> 
    let tys = List.map (isValType ctx) ts in
    (match Base.Result.all tys with
    | Ok _ -> Ok(())
    | Error e -> Error e)
  | Tvar t ->
    (match TContext.find_opt t ctx.typCtx with
    | Some _ -> Ok(())
    | None -> Error (Printf.sprintf "Type variable %s is not in the context" (pp_typ (ValTyp (Tvar t)))))
  | Exists (t, tau) -> isValType { ctx with typCtx = TContext.add t ctx.typCtx } tau
  | UU c -> isCompType ctx c

and isCompType (ctx : ctx) (t : comp_type) : (unit, string) result =
  match t with
  | Arr (t1, t2) -> 
    (match (isValType ctx t1, isCompType ctx t2) with
    | (Ok _, Ok _) -> Ok(())
    | (Error e, _) -> Error e
    | (_, Error e) -> Error e)
  | F t' -> isValType ctx t'

let rec infer_value_type (ctx : ctx) (v : value_term) : (value_type, string) result =
  match v with
  | Var x -> (
      match Context.find_opt x ctx.valCtx with
      | Some(t') -> Ok(t')
      | None -> Error (Printf.sprintf "Variable %s is not in the context" (Variable.pp_var x))
    )
  | TensorProd vs -> (
    let inferTys = List.map (infer_value_type ctx) vs in 
    (match Base.Result.all inferTys with
      | Ok ts -> Ok(Tensor(ts))
      | Error e -> Error e)
    )
  | Inj (sum_t, i, v_inj) -> (
    match sum_t with
    | Sum ts ->
      (match infer_value_type ctx v_inj with
      | Ok(t) -> 
        (* sums are still one indexed *)
        if 0 < i || i >= (List.length ts) then
        let t' = List.nth ts (i - 1) in
          if t = t' then Ok(sum_t) else Error (Printf.sprintf "Type of expression being injected does not match corresponding component of sum type: %s" (pp_term (Val v)))
        else Error (Printf.sprintf "Index out of bounds for inject: %s" (pp_term (Val v)))
      | Error e -> Error e)
    | _ -> Error (Printf.sprintf "Injection is not annotated with sum type: %s" (pp_term (Val v)))
    )
  | Close c ->
    (match infer_comp_type { typCtx = TContext.empty; valCtx = Context.empty } c with
    | Ok(ct) -> Ok(UU ct)
    | Error e -> Error e)
  | Pack ((t, tau), r, v) ->
    (match (isValType ctx r, isValType { ctx with typCtx = TContext.add t ctx.typCtx } tau) with
    | (Ok _, Ok _) -> 
      (match check_value_type ctx v (substValTyp r t tau) with
      | Ok (()) -> Ok(Exists(t, tau))
      | Error e -> Error e)
    | (Error e, _) -> Error e
    | (_, Error e) -> Error e)

and infer_comp_type (ctx : ctx) (c : comp_term) : (comp_type, string) result =
  match c with 
  | Ret v -> (
      match infer_value_type ctx v with
      | Ok(t) -> Ok(F t)
      | Error e -> Error e)
  | Bind (c1, x, c2) ->
    (match infer_comp_type ctx c1 with
    | Ok ct -> 
      (match ct with
      | F a -> infer_comp_type { ctx with valCtx = Context.add x a ctx.valCtx } c2
      | _ -> Error (Printf.sprintf "First computation in Bind does not have F type: %s" (pp_term (Comp c1))))
    | Error e -> Error e)
  | CLet (x, v, c') -> 
    (match infer_value_type ctx v with
    | Ok t -> infer_comp_type { ctx with valCtx = Context.add x t ctx.valCtx } c'
    | Error e -> Error e)
  | Lam (x, typ, c') -> (
      match infer_comp_type {ctx with valCtx = (Context.add x typ ctx.valCtx)} c' with
      | Ok(t2) -> Ok(Arr(typ, t2))
      | Error e -> Error e
    )
  | Ap (c1, v2) -> (
      match infer_comp_type ctx c1 with
      | Ok(Arr(t1, t2)) -> 
        (match check_value_type ctx v2 t1 with
        | Ok _ -> Ok(t2)
        | Error e -> Error e)
      | Ok(t) -> Error (Printf.sprintf "First component of application %s is not a function and has type %s " (pp_term (Comp c1)) (pp_typ (CompTyp t)))
      | Error e -> Error e
    )
  | Open v ->
    (match infer_value_type ctx v with
    | Ok(UU ct) -> Ok(ct)
    | Ok(t') -> Error (Printf.sprintf "Value being forced %s does not have U type -- instead %s" (pp_term (Val v)) (pp_typ (ValTyp t')))
    | Error e -> Error e)
  | Split (v, (xs, c')) -> (
    match infer_value_type ctx v with
    | Ok(Tensor ts) -> 
        let new_ctx = List.fold_left (fun acc (x, t) -> Context.add x t acc) ctx.valCtx (List.combine xs ts) in
        infer_comp_type {ctx with valCtx = new_ctx} c'
    | Ok(t) -> Error (Printf.sprintf "Tried to split on term %s with type %s" (pp_term (Val v)) (pp_typ (ValTyp t)))
    | Error e -> Error e
  )
  | Case (v, arms) -> (
      match infer_value_type ctx v with
      | Ok (Sum ts) -> 
        let armTyps = List.map (fun ((x, c), t) -> infer_comp_type { ctx with valCtx = (Context.add x t ctx.valCtx) } c) (List.combine arms ts) in
        (match Base.Result.all armTyps with
        | (Ok ts') -> 
          if List.length ts' = 0 then
            Error "You somehow got a void which shouldn't be possible"
          else
            if List.for_all (fun t -> (t = List.nth ts' 0)) ts' then
              Ok (List.nth ts' 0) else Error (Printf.sprintf "Branches of case do not have same type: %s" (pp_term (Comp c)))
        | Error e -> Error e)
      | Ok _ -> Error (Printf.sprintf "Tried to case on a non-sum: %s" (pp_term (Comp c)))
      | Error e -> Error e
    )
  | Print _ -> Ok(F(Tensor []))
  | Unpack (v, (x, c')) -> 
    (match infer_value_type ctx v with 
    | Ok(Exists (t, tau)) -> infer_comp_type { typCtx = TContext.add t ctx.typCtx; valCtx = Context.add x tau ctx.valCtx } c'
    | Ok _ -> Error (Printf.sprintf "Tried to unpack non-exists: %s" (pp_term (Val v)))
    | Error e -> Error e)

and check_value_type (ctx : ctx) (v : value_term) (t : value_type) =
  (match infer_value_type ctx v with
  | Ok(t') -> if valTypEqual t t' then Ok(()) else Error (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Val v)) (pp_typ (ValTyp t)) (pp_typ (ValTyp t')))
  | Error e -> Error e)
and check_comp_type (ctx : ctx) (c : comp_term) (t : comp_type) =
  (match infer_comp_type ctx c with
  | Ok(t') -> if compTypEqual t t' then Ok(()) else Error (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Comp c)) (pp_typ (CompTyp t)) (pp_typ (CompTyp t')))
  | Error e -> Error e)

let check_type (c : comp_term) (t : comp_type) = check_comp_type { typCtx = TContext.empty; valCtx = Context.empty } c t

let infer_type (c : comp_term) = infer_comp_type { typCtx = TContext.empty; valCtx = Context.empty } c