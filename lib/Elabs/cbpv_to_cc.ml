open Cc_ast

let rec trans_value_typ t = 
  match t with 
  | Cbpv_ast.Tensor vs -> Tensor (List.map trans_value_typ vs)
  | Cbpv_ast.Sum vs -> Sum (List.map trans_value_typ vs)
  | Cbpv_ast.U x -> 
      let t = Variable.new_var() in 
      Exists(t, Tensor [Tvar t; UU(Arr(Tvar t, trans_comp_typ x))])

and trans_comp_typ t = 
  match t with 
  | Cbpv_ast.Arr (t, c) -> Arr (trans_value_typ t, trans_comp_typ c)
  | Cbpv_ast.F t -> F (trans_value_typ t)

let rec trans_value_term ctx m = 
  match m with 
  | Cbpv_ast.Var x -> Var x
  | Cbpv_ast.TensorProd vs -> TensorProd (List.map (trans_value_term ctx) vs)
  | Cbpv_ast.Inj (t, i, v) -> Inj (trans_value_typ t, i, trans_value_term ctx v)
  | Cbpv_ast.Susp c -> 
      let ctx_list = Cbpv_ast.Context.to_list ctx in
      let trans_var_typ_list = List.map (fun (x, t) -> (Var x, trans_value_typ t)) ctx_list in 
      let (var_list, typ_list) = List.fold_right (fun  (x, t) (acc1, acc2) -> (x::acc1, t::acc2)) trans_var_typ_list ([], []) in 
      let pack_typ = Tensor typ_list in 
      let l = TensorProd var_list in 
      let g = Variable.new_var() in 
      let rec split_helper old_vars = 
        match old_vars with 
        | [] -> ([], trans_comp_term ctx c) 
        | x::xs -> 
            let ai = Variable.new_var() in 
            let (new_vars, res) = split_helper xs in 
            (ai::new_vars, CLet (ai, x, res))
      in
      let (split_vars, res_term) = split_helper var_list in 
      let r = Close (Lam(g, pack_typ, Split(Var g, (split_vars, res_term)))) in
      Pack (pack_typ, TensorProd [l; r])

and trans_comp_term ctx c = 
  match c with 
  | Cbpv_ast.Ret v -> Ret (trans_value_term ctx v)
  | Cbpv_ast.Bind (c, x, c1) -> Bind (trans_comp_term ctx c, x, trans_comp_term ctx c1)
  | Cbpv_ast.Lam (x, t, c) ->
      let new_ctx = Cbpv_ast.Context.add x t ctx in 
      Lam (x, trans_value_typ t, trans_comp_term new_ctx c)
  | Cbpv_ast.Ap (c, v) -> Ap (trans_comp_term ctx c, trans_value_term ctx v)
  | Cbpv_ast.Force m -> 
      let x = Variable.new_var() in 
      let yenv = Variable.new_var() in 
      let c = Variable.new_var() in 
      Unpack (trans_value_term ctx m, (x, Split(Var x, ([yenv; c], Ap(Open (Var c), Var(yenv))))))
  | Cbpv_ast.Split (v, (xs, c)) -> 
      let prod_typ = match (Tc_cbpv.infer_value_type_ctx ctx v) with 
                    | Ok(Tensor vs) -> vs
                      (* Hopefully unreachable *)
                    | _ -> failwith "types wrong" in
      let rec ctx_helper vars i acc =
        match vars with 
        | [] -> acc
        | x::xs -> ctx_helper xs (i+1) (Cbpv_ast.Context.add x (List.nth prod_typ i) ctx) in
      let new_ctx = ctx_helper xs 0 ctx in 
      Split (trans_value_term ctx v, (xs, trans_comp_term new_ctx c))
  | Cbpv_ast.Case (v, arms) -> 
      let sum_typ = match (Tc_cbpv.infer_value_type_ctx ctx v) with 
                    | Ok(Sum vs) -> vs
                    | _ -> failwith "types wrong" in
      let rec arm_helper arms i acc =
        match arms with 
        | [] -> acc
        | (x, c)::arms -> 
            let new_ctx = Cbpv_ast.Context.add x (List.nth sum_typ i) ctx in
            let new_acc = acc @ [(x, trans_comp_term new_ctx c)] in
            arm_helper arms (i+1) new_acc
      in
      Case (trans_value_term ctx v, arm_helper arms 0 [])
  | Cbpv_ast.Print s -> Print s