open Cbpv_ast

module PrimTypeCbpv : Prim.PrimTypeParam with type t = value_type = struct
  type t = value_type

  let unit = Tensor []
  let int_typ = Int_Typ
  let string_typ = String_Typ
end

module PrimType = Prim.PrimTypeFun (PrimTypeCbpv)

let rec infer_tc_val (ctx : value_type Context.t) (v : value_term) : value_type
    =
  match v with
  | Var x -> (
      match Context.find_opt x ctx with
      | Some t' -> t'
      | None ->
          failwith
            (Printf.sprintf "Variable %s is not in the context"
               (Variable.pp_var x)))
  | TensorProd vs -> Tensor (List.map (infer_tc_val ctx) vs)
  | Inj (sum_t, i, v_inj) -> (
      match sum_t with
      | Sum ts ->
          let t = infer_tc_val ctx v_inj in
          (* sums are still one indexed *)
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
  | Susp c -> U (infer_tc_comp ctx c)
  | Int _ -> Int_Typ
  | String _ -> String_Typ

and infer_tc_comp (ctx : value_type Context.t) (c : comp_term) : comp_type =
  match c with
  | Ret v -> F (infer_tc_val ctx v)
  | Bind (c1, x, c2) -> (
      match infer_tc_comp ctx c1 with
      | F a -> infer_tc_comp (Context.add x a ctx) c2
      | _ ->
          failwith
            (Printf.sprintf "First computation in Bind does not have F type: %s"
               (pp_term (Comp c1))))
  | Lam (x, typ, c') -> Arr (typ, infer_tc_comp (Context.add x typ ctx) c')
  | Ap (c1, v2) -> (
      match infer_tc_comp ctx c1 with
      | Arr (t1, t2) ->
          tc_val ctx v2 t1;
          t2
      | t ->
          failwith
            (Printf.sprintf
               "First component of application %s is not a function and has \
                type %s "
               (pp_term (Comp c1)) (pp_typ (CompTyp t))))
  | Force v -> (
      match infer_tc_val ctx v with
      | U ct -> ct
      | t' ->
          failwith
            (Printf.sprintf
               "Value being forced %s does not have U type -- instead %s"
               (pp_term (Val v)) (pp_typ (ValTyp t'))))
  | Split (v, (xs, c')) -> (
      match infer_tc_val ctx v with
      | Tensor ts ->
          let new_ctx =
            List.fold_left
              (fun acc (x, t) -> Context.add x t acc)
              ctx (List.combine xs ts)
          in
          infer_tc_comp new_ctx c'
      | t ->
          failwith
            (Printf.sprintf "Tried to split on term %s with type %s"
               (pp_term (Val v)) (pp_typ (ValTyp t))))
  | Case (v, arms) -> (
      match infer_tc_val ctx v with
      | Sum ts ->
          let armTyps =
            List.map
              (fun ((x, c), t) -> infer_tc_comp (Context.add x t ctx) c)
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
  | Prim (p, args) ->
      let arg_typs, ret_typ = PrimType.primtype p in
      List.fold_left
        (fun _ (e, t) -> tc_val ctx e t)
        ()
        (List.combine args arg_typs);
      F ret_typ

and tc_val (ctx : value_type Context.t) (v : value_term) (t : value_type) : unit
    =
  let t' = infer_tc_val ctx v in
  if t = t' then ()
  else
    failwith
      (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Val v))
         (pp_typ (ValTyp t)) (pp_typ (ValTyp t')))

and tc_comp (ctx : value_type Context.t) (c : comp_term) (t : comp_type) : unit
    =
  let t' = infer_tc_comp ctx c in
  if t = t' then ()
  else
    failwith
      (Printf.sprintf "Expected %s to have type %s, got %s" (pp_term (Comp c))
         (pp_typ (CompTyp t)) (pp_typ (CompTyp t')))

let check_type (c : comp_term) (t : comp_type) = tc_comp Context.empty c t
let infer_type (c : comp_term) = infer_tc_comp Context.empty c
let infer_comp_type_ctx ctx c = infer_tc_comp ctx c
let infer_value_type_ctx ctx m = infer_tc_val ctx m
