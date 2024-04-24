open Cbpv_ast

(* context should only contain variables with value type *)
module Context = Map.Make(Variable)

let rec infer_tc_val (ctx : value_type Context.t) (v : value_term) : (value_type, string) result =
  match v with
  | Var x -> (
      match Context.find_opt x ctx with
      | Some(t') -> Ok(t')
      | None -> Error (Printf.sprintf "Variable %s is not in the context" (Variable.pp_var x))
    )
  | TensorProd vs -> (
    let inferTys = List.map (infer_tc_val ctx) vs in 
    (match Base.Result.all inferTys with
      | Ok ts -> Ok(Tensor(ts))
      | Error e -> Error e)
    )
  | Inj (sum_t, i, v_inj) -> (
    match sum_t with
    | Sum ts ->
      (match infer_tc_val ctx v_inj with
      | Ok(t) -> 
        (* sums are still one indexed *)
        if 0 < i || i >= (List.length ts) then
        let t' = List.nth ts (i - 1) in
          if t = t' then Ok(sum_t) else Error (Printf.sprintf "Type of expression being injected does not match corresponding component of sum type: %s" (pp_term (Val v)))
        else Error (Printf.sprintf "Index out of bounds for inject: %s" (pp_term (Val v)))
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
  | Split (v, (xs, c')) -> (
    match infer_tc_val ctx v with
    | Ok(Tensor ts) -> 
        let new_ctx = List.fold_left (fun acc (x, t) -> Context.add x t acc) ctx (List.combine xs ts) in
        infer_tc_comp new_ctx c'
    | Ok(t) -> Error (Printf.sprintf "Tried to split on term %s with type %s" (pp_term (Val v)) (pp_typ (ValTyp t)))
    | Error e -> Error e
  )
  | Case (v, arms) -> (
      match infer_tc_val ctx v with
      | Ok (Sum ts) -> 
        let armTyps = List.map (fun ((x, c), t) -> infer_tc_comp (Context.add x t ctx) c) (List.combine arms ts) in
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

and tc_val (ctx : value_type Context.t) (v : value_term) (t : value_type) : (unit, string) result =
  match infer_tc_val ctx v with
  | Ok(t') -> if t = t' then Ok(()) else Error (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Val v)) (pp_typ (ValTyp t)) (pp_typ (ValTyp t')))
  | Error e -> Error e

and tc_comp (ctx : value_type Context.t) (c : comp_term) (t : comp_type) : (unit, string) result =
  match infer_tc_comp ctx c with
  | Ok(t') -> if t = t' then Ok(()) else Error (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Comp c)) (pp_typ (CompTyp t)) (pp_typ (CompTyp t')))
  | Error e -> Error e

let check_type (c : comp_term) (t : comp_type) = tc_comp Context.empty c t

let infer_type (c : comp_term) = infer_tc_comp Context.empty c