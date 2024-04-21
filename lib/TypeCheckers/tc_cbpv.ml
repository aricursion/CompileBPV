 open Cbpv_ast
(*
(* context should only contain variables with value type *)
module Context = Map.Make(Variable)

let rec infer_tc_val (ctx : value_type Context.t) (v : value_term) : (value_type, string) result =
  match v with
  | Var x -> (
      match Context.find_opt x ctx with
      | Some(t') -> Ok(t')
      | None -> Error (Printf.sprintf "Variable %s is not in the context" (Variable.pp_var x))
    )
  | TensorProd (v1, v2) -> (
    (match (infer_tc_val ctx v1, infer_tc_val ctx v2) with
      | (Ok t1, Ok t2) -> Ok(Tensor(t1, t2))
      | (Error e, _) -> Error e
      | (_, Error e) -> Error e)
    )
  | Triv -> Ok(Unit)
  | Inj (sum_t, i, v_inj) -> (
    match sum_t with
    | Sum (t1, t2) ->
      (match infer_tc_val ctx v_inj with
      | Ok(t) -> 
        if i = 1 then
          if t = t1 then Ok(sum_t) else Error (Printf.sprintf "Type of expression being injected does not match corresponding component of sum type: %s" (pp_term (Val v)))
        else if i = 2 then
          if t = t2 then Ok(sum_t) else Error (Printf.sprintf "Type of expression being injected does not match corresponding component of sum type: %s" (pp_term (Val v)))
        else 
          Error (Printf.sprintf "Type of expression being injected does not match corresponding component of sum type: %s" (pp_term (Val v)))
      | Error e -> Error e)
    | _ -> Error (Printf.sprintf "Injection is not annotated with sum type: %s" (pp_term (Val v)))
    )
  | Susp c ->
    (match infer_tc_comp ctx c with
    | Ok(ct) -> Ok(U ct)
    | Error e -> Error e)

and infer_tc_comp (ctx : value_type Context.t) (c : comp_term) : (comp_type, string) result =
  match c with 
  | Ret v -> (
      match infer_tc_val ctx v with
      | Ok(t) -> Ok(F t)
      | Error e -> Error e)
  | Bind (c1, x, c2) ->
    (match infer_tc_comp ctx c1 with
    | Ok ct -> 
      (match ct with
      | F a -> infer_tc_comp (Context.add x a ctx) c2
      | _ -> Error (Printf.sprintf "First computation in Bind does not have F type: %s" (pp_term (Comp c1))))
    | Error e -> Error e)
  | Lam (x, typ, c') -> (
      match infer_tc_comp (Context.add x typ ctx) c' with
      | Ok(t2) -> Ok(Arr(typ, t2))
      | Error e -> Error e
    )
  | Ap (c1, v2) -> (
      match infer_tc_comp ctx c1 with
      | Ok(Arr(t1, t2)) -> 
        (match tc_val ctx v2 t1 with
        | Ok _ -> Ok(t2)
        | Error e -> Error e)
      | Ok(t) -> Error (Printf.sprintf "First component of application %s is not a function and has type %s " (pp_term (Comp c1)) (pp_typ (CompTyp t)))
      | Error e -> Error e
    )
  | Force v ->
    (match infer_tc_val ctx v with
    | Ok(U ct) -> Ok(ct)
    | Ok(t') -> Error (Printf.sprintf "Value being forced %s does not have U type -- instead %s" (pp_term (Val v)) (pp_typ (ValTyp t')))
    | Error e -> Error e)
  | Split (v, ((x1, x2), c')) -> (
    match infer_tc_val ctx v with
    | Ok(Tensor (t1, t2)) -> 
        let new_ctx = Context.add x2 t2 (Context.add x1 t1 ctx) in
        infer_tc_comp new_ctx c'
    | Ok(t) -> Error (Printf.sprintf "Tried to split on term %s with type %s" (pp_term (Val v)) (pp_typ (ValTyp t)))
    | Error e -> Error e
  )
  | Case (v, (x1, c1), (x2, c2)) -> (
      match infer_tc_val ctx v with
      | Ok (Sum (t1, t2)) -> 
        (match (infer_tc_comp (Context.add x1 t1 ctx) c1, infer_tc_comp (Context.add x2 t2 ctx) c2) with 
        | (Ok t, Ok t') -> if t = t' then Ok(t) else Error (Printf.sprintf "Branches of case do not have same type: %s" (pp_term (Comp c)))
        | (Error e, _) -> Error e
        | (_, Error e) -> Error e)
      | Ok _ -> Error (Printf.sprintf "Tried to case on a non-sum: %s" (pp_term (Comp c)))
      | Error e -> Error e
    )
  | Check (v, c') ->
    (match infer_tc_val ctx v with
    | Ok(Unit) -> infer_tc_comp ctx c'
    | Ok _ -> Error (Printf.sprintf "Expression being checked does not have unit type: `%s`" (pp_term (Val v)))
    | Error e -> Error e)
  | Print _ -> Ok(F(Unit))

and tc_val (ctx : value_type Context.t) (v : value_term) (t : value_type) : (unit, string) result =
  match infer_tc_val ctx v with
  | Ok(t') -> if t = t' then Ok(()) else Error (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Val v)) (pp_typ (ValTyp t)) (pp_typ (ValTyp t')))
  | Error e -> Error e

and tc_comp (ctx : value_type Context.t) (c : comp_term) (t : comp_type) : (unit, string) result =
  match infer_tc_comp ctx c with
  | Ok(t') -> if t = t' then Ok(()) else Error (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Comp c)) (pp_typ (CompTyp t)) (pp_typ (CompTyp t')))
  | Error e -> Error e *)

let check_type (_ : comp_term) (_ : comp_type) = raise (Failure "foo")

let infer_type (_ : comp_term) = raise (Failure "foo")