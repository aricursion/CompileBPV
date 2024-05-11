open Cc_ast

type ctx = { typCtx : TContext.t; valCtx : value_type Context.t }

module PrimTypeCC : Prim.PrimTypeParam with type t = value_type = struct
  type t = value_type

  let unit = Tensor []
  let int_typ = Int_Typ
  let string_typ = String_Typ
end

module PrimType = Prim.PrimTypeFun (PrimTypeCC)

(* this is just checking structural congruence and not exact equality because types variables are hard *)
let rec valTypEqual (t : value_type) (t' : value_type) =
  match (t, t') with
  | Tensor ts, Tensor ts' ->
      List.fold_left
        (fun acc (t1, t2) -> valTypEqual t1 t2 && acc)
        true (List.combine ts ts')
  | Sum ts, Sum ts' ->
      List.fold_left
        (fun acc (t1, t2) -> valTypEqual t1 t2 && acc)
        true (List.combine ts ts')
  | Tvar _, Tvar _ -> true
  | Exists (_, tau1), Exists (_, tau2) -> valTypEqual tau1 tau2
  | UU c1, UU c2 -> compTypEqual c1 c2
  | Int_Typ, Int_Typ -> true
  | String_Typ, String_Typ -> true
  | _ -> false

and compTypEqual (t : comp_type) (t' : comp_type) =
  match (t, t') with
  | Arr (t1, t2), Arr (t1', t2') -> valTypEqual t1 t1' && compTypEqual t2 t2'
  | F t, F t' -> valTypEqual t t'
  | _ -> false

let rec substValTyp (tau : value_type) (t : Variable.t) (tau' : value_type) :
    value_type =
  match tau' with
  | Tensor ts -> Tensor (List.map (substValTyp tau t) ts)
  | Sum ts -> Sum (List.map (substValTyp tau t) ts)
  | Tvar t' -> if Variable.compare t t' = 0 then tau else tau'
  | Exists (t', tau'') -> Exists (t', substValTyp tau t tau'')
  | UU ct -> UU (substCompTyp tau t ct)
  | Int_Typ -> Int_Typ
  | String_Typ -> String_Typ

and substCompTyp (tau : value_type) (t : Variable.t) (ct : comp_type) :
    comp_type =
  match ct with
  | Arr (tau', ct') -> Arr (substValTyp tau t tau', substCompTyp tau t ct')
  | F tau' -> F (substValTyp tau t tau')

let rec isValType (ctx : ctx) (t : value_type) : unit =
  match t with
  | Tensor ts ->
      let _ = List.map (isValType ctx) ts in
      ()
  | Sum ts ->
      let _ = List.map (isValType ctx) ts in
      ()
  | Tvar t -> (
      match TContext.find_opt t ctx.typCtx with
      | Some _ -> ()
      | None ->
          failwith
            (Printf.sprintf "Type variable %s is not in the context"
               (pp_typ (ValTyp (Tvar t)))))
  | Exists (t, tau) ->
      isValType { ctx with typCtx = TContext.add t ctx.typCtx } tau
  | UU c -> isCompType ctx c
  | _ -> ()

and isCompType (ctx : ctx) (t : comp_type) : unit =
  match t with
  | Arr (t1, t2) ->
      isValType ctx t1;
      isCompType ctx t2
  | F t' -> isValType ctx t'

let rec infer_value_type (ctx : ctx) (v : value_term) : value_type =
  match v with
  | Var x -> (
      match Context.find_opt x ctx.valCtx with
      | Some t' -> t'
      | None ->
          failwith
            (Printf.sprintf "Variable %s is not in the context"
               (Variable.pp_var x)))
  | TensorProd vs -> Tensor (List.map (infer_value_type ctx) vs)
  | Inj (sum_t, i, v_inj) -> (
      match sum_t with
      | Sum ts ->
          let t = infer_value_type ctx v_inj in
          if 0 < i || i >= List.length ts then
            let t' = List.nth ts (i - 1) in
            if t = t' then sum_t
            else
              failwith
                (Printf.sprintf
                   "Type of expression being injected does not match \
                    corresponding component of sum type: %s"
                   (pp_term (Val v)))
          else
            failwith
              (Printf.sprintf "Index out of bounds for inject: %s"
                 (pp_term (Val v)))
      | _ ->
          failwith
            (Printf.sprintf "Injection is not annotated with sum type: %s"
               (pp_term (Val v))))
  | Close c ->
      UU (infer_comp_type { typCtx = TContext.empty; valCtx = Context.empty } c)
  | Pack ((t, tau), r, v) ->
      isValType ctx r;
      isValType { ctx with typCtx = TContext.add t ctx.typCtx } tau;
      check_value_type ctx v (substValTyp r t tau);
      Exists (t, tau)
  | Int _ -> Int_Typ
  | String _ -> String_Typ

and infer_comp_type (ctx : ctx) (c : comp_term) : comp_type =
  match c with
  | Ret v -> F (infer_value_type ctx v)
  | Bind (c1, x, c2) -> (
      let ct = infer_comp_type ctx c1 in
      match ct with
      | F a ->
          infer_comp_type { ctx with valCtx = Context.add x a ctx.valCtx } c2
      | _ ->
          failwith
            (Printf.sprintf "First computation in Bind does not have F type: %s"
               (pp_term (Comp c1))))
  | Lam (x, typ, c') ->
      let t2 =
        infer_comp_type { ctx with valCtx = Context.add x typ ctx.valCtx } c'
      in
      Arr (typ, t2)
  | Ap (c1, v2) -> (
      match infer_comp_type ctx c1 with
      | Arr (t1, t2) ->
          check_value_type ctx v2 t1;
          t2
      | t ->
          failwith
            (Printf.sprintf
               "First component of application %s is not a function and has \
                type %s "
               (pp_term (Comp c1)) (pp_typ (CompTyp t))))
  | Open v -> (
      match infer_value_type ctx v with
      | UU ct -> ct
      | t' ->
          failwith
            (Printf.sprintf
               "Value being forced %s does not have U type -- instead %s"
               (pp_term (Val v)) (pp_typ (ValTyp t'))))
  | Split (v, (xs, c')) -> (
      match infer_value_type ctx v with
      | Tensor ts ->
          let new_ctx =
            List.fold_left
              (fun acc (x, t) -> Context.add x t acc)
              ctx.valCtx (List.combine xs ts)
          in
          infer_comp_type { ctx with valCtx = new_ctx } c'
      | t ->
          failwith
            (Printf.sprintf "Tried to split on term %s with type %s"
               (pp_term (Val v)) (pp_typ (ValTyp t))))
  | Case (v, arms) -> (
      match infer_value_type ctx v with
      | Sum ts ->
          let armTyps =
            List.map
              (fun ((x, c), t) ->
                infer_comp_type
                  { ctx with valCtx = Context.add x t ctx.valCtx }
                  c)
              (List.combine arms ts)
          in
          if List.length armTyps = 0 then
            failwith "You somehow got a void which shouldn't be possible"
          else if List.for_all (fun t -> t = List.nth armTyps 0) armTyps then
            List.nth armTyps 0
          else
            failwith
              (Printf.sprintf "Branches of case do not have same type: %s"
                 (pp_term (Comp c)))
      | _ ->
          failwith
            (Printf.sprintf "Tried to case on a non-sum: %s" (pp_term (Comp c)))
      )
  | Unpack (v, (x, c')) -> (
      match infer_value_type ctx v with
      | Exists (t, tau) ->
          infer_comp_type
            {
              typCtx = TContext.add t ctx.typCtx;
              valCtx = Context.add x tau ctx.valCtx;
            }
            c'
      | _ ->
          failwith
            (Printf.sprintf "Tried to unpack non-exists: %s" (pp_term (Val v))))
  | Prim (p, args) ->
      let arg_typs, ret_typ = PrimType.primtype p in
      List.fold_left
        (fun _ (e, t) -> check_value_type ctx e t)
        ()
        (List.combine args arg_typs);
      F ret_typ

and check_value_type (ctx : ctx) (v : value_term) (t : value_type) =
  let t' = infer_value_type ctx v in
  if valTypEqual t t' then ()
  else
    failwith
      (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Val v))
         (pp_typ (ValTyp t)) (pp_typ (ValTyp t')))

and check_comp_type (ctx : ctx) (c : comp_term) (t : comp_type) =
  let t' = infer_comp_type ctx c in
  if compTypEqual t t' then ()
  else
    failwith
      (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Comp c))
         (pp_typ (CompTyp t)) (pp_typ (CompTyp t')))

let check_type (c : comp_term) (t : comp_type) =
  check_comp_type { typCtx = TContext.empty; valCtx = Context.empty } c t

let infer_type (c : comp_term) =
  infer_comp_type { typCtx = TContext.empty; valCtx = Context.empty } c
