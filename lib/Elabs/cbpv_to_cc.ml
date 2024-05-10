open Cc_ast

let rec trans_value_typ t =
  match t with
  | Cbpv_ast.Tensor vs -> Tensor (List.map trans_value_typ vs)
  | Cbpv_ast.Sum vs -> Sum (List.map trans_value_typ vs)
  | Cbpv_ast.U x ->
      let t = Variable.new_var () in
      Exists (t, Tensor [ Tvar t; UU (Arr (Tvar t, trans_comp_typ x)) ])

and trans_comp_typ t =
  match t with
  | Cbpv_ast.Arr (t, c) -> Arr (trans_value_typ t, trans_comp_typ c)
  | Cbpv_ast.F t -> F (trans_value_typ t)

let rec trans_value_term ctx m =
  match m with
  | Cbpv_ast.Var x -> Var x
  | Cbpv_ast.TensorProd vs -> TensorProd (List.map (trans_value_term ctx) vs)
  | Cbpv_ast.Inj (t, i, v) -> Inj (trans_value_typ t, i, trans_value_term ctx v)
  | Cbpv_ast.Susp c -> (
      let ctx_list = Cbpv_ast.Context.to_list ctx in
      let trans_var_typ_list =
        List.map (fun (x, t) -> (x, trans_value_typ t)) ctx_list
      in
      let var_list, typ_list =
        List.fold_right
          (fun (x, t) (acc1, acc2) -> (x :: acc1, t :: acc2))
          trans_var_typ_list ([], [])
      in
      let pack_typ = Tensor typ_list in
      let l = TensorProd (List.map (fun x -> Var x) var_list) in
      let g = Variable.new_var () in
      let r =
        Close
          (Lam (g, pack_typ, Split (Var g, (var_list, trans_comp_term ctx c))))
      in
      let t = Variable.new_var () in
      match Tc_cbpv.infer_comp_type_ctx ctx c with
      | Ok tau ->
          Pack
            ( (t, Tensor [ Tvar t; UU (Arr (Tvar t, trans_comp_typ tau)) ]),
              pack_typ,
              TensorProd [ l; r ] )
      | Error e -> failwith e)

and trans_comp_term ctx c =
  match c with
  | Cbpv_ast.Ret v -> Ret (trans_value_term ctx v)
  | Cbpv_ast.Bind (c, x, c1) -> (
      match Tc_cbpv.infer_comp_type_ctx ctx c with
      | Ok (F a) ->
          Bind
            ( trans_comp_term ctx c,
              x,
              trans_comp_term (Cbpv_ast.Context.add x a ctx) c1 )
      | Ok t ->
          failwith
            (Printf.sprintf
               "Expected comp being bound to have F type, had type %s instead"
               (Cbpv_ast.pp_typ (CompTyp t)))
      | Error e -> failwith e)
  | Cbpv_ast.Lam (x, t, c) ->
      let new_ctx = Cbpv_ast.Context.add x t ctx in
      Lam (x, trans_value_typ t, trans_comp_term new_ctx c)
  | Cbpv_ast.Ap (c, v) -> Ap (trans_comp_term ctx c, trans_value_term ctx v)
  | Cbpv_ast.Force m ->
      let x = Variable.new_var () in
      let yenv = Variable.new_var () in
      let c = Variable.new_var () in
      Unpack
        ( trans_value_term ctx m,
          (x, Split (Var x, ([ yenv; c ], Ap (Open (Var c), Var yenv)))) )
  | Cbpv_ast.Split (v, (vars, c)) ->
      let prod_typ =
        match Tc_cbpv.infer_value_type_ctx ctx v with
        | Ok (Tensor vs) -> vs
        | Ok t ->
            failwith
              (Printf.sprintf
                 "Expected value being split to have tensor type, had type %s \
                  instead"
                 (Cbpv_ast.pp_typ (ValTyp t)))
        | Error e -> failwith e
      in
      let new_ctx =
        List.fold_left
          (fun acc_ctx (t, x) -> Cbpv_ast.Context.add x t acc_ctx)
          ctx
          (List.combine prod_typ vars)
      in
      Split (trans_value_term ctx v, (vars, trans_comp_term new_ctx c))
  | Cbpv_ast.Case (v, arms) ->
      let sum_typ =
        match Tc_cbpv.infer_value_type_ctx ctx v with
        | Ok (Sum vs) -> vs
        | Ok t ->
            failwith
              (Printf.sprintf
                 "Expecting value being cased on to have some type type, had \
                  type %s instead"
                 (Cbpv_ast.pp_typ (ValTyp t)))
        | Error e -> failwith e
      in
      let rec arm_helper arms =
        match arms with
        | [] -> []
        | (t, (x, c)) :: arms ->
            let new_ctx = Cbpv_ast.Context.add x t ctx in
            (x, trans_comp_term new_ctx c) :: arm_helper arms
      in
      Case (trans_value_term ctx v, arm_helper (List.combine sum_typ arms))
  | Cbpv_ast.Print s -> Print s

let translate : Cbpv_ast.comp_term -> Cc_ast.comp_term =
  trans_comp_term Cbpv_ast.Context.empty
