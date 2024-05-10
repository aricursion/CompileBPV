open Ast
module Context = Map.Make (Variable)

module PrimTypeML : Prim.PrimTypeParam with type t = Ast.typ = struct
  type t = Ast.typ

  let unit = Ast.Unit
  let int_typ = Ast.Int_Typ
  let string_typ = Ast.String_Typ
end

module PrimType = Prim.PrimTypeFun (PrimTypeML)

let rec tc (ctx : typ Context.t) (e : term) (t : typ) =
  match e with
  | Var x -> (
      match Context.find_opt x ctx with
      | Some t' ->
          if t' = t then ()
          else
            failwith
              (Printf.sprintf
                 "Variable %s is in the context, but has type %s when expected \
                  %s"
                 (Variable.pp_var x) (pp_typ t') (pp_typ t))
      | None ->
          failwith
            (Printf.sprintf "Variable %s is not in the context"
               (Variable.pp_var x)))
  | Lambda (x, typ, e') -> (
      match t with
      | Arrow (t1, t2) ->
          if t1 = typ then
            let new_ctx = Context.add x t1 ctx in
            tc new_ctx e' t2
          else
            failwith
              (Printf.sprintf "Term %s does not have expected type %s"
                 (pp_term e) (pp_typ t))
      | _ ->
          failwith
            (Printf.sprintf "Term %s does not have expected type %s" (pp_term e)
               (pp_typ t)))
  | Ap (e1, e2) -> (
      match infer_tc ctx e1 with
      | Arrow (t1, t2) ->
          if t = t2 then tc ctx e2 t1
          else
            failwith
              (Printf.sprintf
                 "Term %s is an arrow which returns an %s, but should return %s"
                 (pp_term e1) (pp_typ t2) (pp_typ t))
      | t ->
          failwith
            (Printf.sprintf
               "First component of application %s is not a function and has \
                type %s "
               (pp_term e) (pp_typ t)))
  | Triv ->
      if t = Unit then () else failwith "Somehow <> doesn't have type unit"
  | Tup (e1, e2) -> (
      match t with
      | Prod (t1, t2) ->
          tc ctx e1 t1;
          tc ctx e2 t2
      | _ -> failwith "foo")
  | Split (m, ((v1, v2), e)) -> (
      match infer_tc ctx m with
      | Prod (t1, t2) ->
          let new_ctx = Context.add v2 t2 (Context.add v1 t1 ctx) in
          tc new_ctx e t
      | _ -> failwith "bar")
  | Inj (sum_t, i, e_inj) ->
      if t = sum_t then
        match t with
        | Sum (t1, t2) ->
            if i = 1 then
              (* injections are no longer zero indexed *)
              tc ctx e_inj t1
            else if i = 2 then tc ctx e_inj t2
            else
              failwith
                (Printf.sprintf "Injection into type %s has invalid index %i"
                   (pp_typ sum_t) i)
        | _ ->
            failwith
              (Printf.sprintf
                 "Injection %s is not annotated with a sum - instead %s"
                 (pp_term e) (pp_typ sum_t))
      else
        failwith
          (Printf.sprintf
             "Expected type %s does not match type %s of expression %s"
             (pp_typ t) (pp_typ sum_t) (pp_term e_inj))
  | Case (e, (v1, m1), (v2, m2)) -> (
      match infer_tc ctx e with
      | Sum (t1, t2) ->
          tc (Context.add v1 t1 ctx) m1 t;
          tc (Context.add v2 t2 ctx) m2 t
      | _ -> failwith "Tried to case on a non-sum")
  | Check (e, e1) -> (
      match infer_tc ctx e with
      | Unit -> tc ctx e1 t
      | _ ->
          failwith
            (Printf.sprintf
               "Expression being checked does not have unit type: `%s`"
               (Ast.pp_term e)))
  | Let (x, e1, e2) -> tc (Context.add x (infer_tc ctx e1) ctx) e2 t
  | Prim (p, args) ->
      let arg_typs, res_typ = PrimType.primtype p in
      if List.length args != List.length arg_typs then
        failwith
          (Printf.sprintf "Primitive %s expects %d arguments, %d provided"
             (Prim.pp_prim p) (List.length arg_typs) (List.length args))
      else if res_typ != t then
        failwith
          (Printf.sprintf "Primitive %s returns type %s, expected %s"
             (Prim.pp_prim p) (pp_typ res_typ) (pp_typ t))
      else
        let typ_args = List.combine args arg_typs in
        List.fold_left (fun _ (e, t) -> tc ctx e t) () typ_args
  | Int _ ->
      if t == Ast.Int_Typ then ()
      else failwith (Printf.sprintf "Tried to check an int as a %s" (pp_typ t))
  | String _ ->
      if t == Ast.String_Typ then ()
      else
        failwith (Printf.sprintf "Tried to check an string as a %s" (pp_typ t))

and infer_tc (ctx : typ Context.t) (e : term) =
  match e with
  | Var x -> (
      match Context.find_opt x ctx with
      | Some t -> t
      | None ->
          failwith
            (Printf.sprintf "Variable %s is not in the context"
               (Variable.pp_var x)))
  | Lambda (x, t1, e) ->
      let new_ctx = Context.add x t1 ctx in
      Arrow (t1, infer_tc new_ctx e)
  | Ap (e1, e2) -> (
      let t1 = infer_tc ctx e1 in
      let t' = infer_tc ctx e2 in
      match t1 with
      | Arrow (t1, t2) ->
          if t1 = t' then t2
          else
            failwith
              "Error while inferring application - input type doesn't match \
               negative position in arrow"
      | _ ->
          failwith
            "Error while inferring application - first arg is not an arrow")
  | Triv -> Unit
  | Tup (e1, e2) -> Prod (infer_tc ctx e1, infer_tc ctx e2)
  | Split (m, ((v1, v2), e)) -> (
      match infer_tc ctx m with
      | Prod (t1, t2) -> infer_tc (Context.add v2 t2 (Context.add v1 t1 ctx)) e
      | t ->
          failwith
            (Printf.sprintf "Tried to split on term %s with type %s" (pp_term m)
               (pp_typ t)))
  | Inj (t, i, e) -> (
      match t with
      | Sum (t1, t2) ->
          if i = 1 then tc ctx e t1
          else if i = 2 then tc ctx e t2
          else failwith "Injection not 1 or 2";
          t
      | _ -> failwith "Injection not annotated with sum")
  | Case (e, (v1, m1), (v2, m2)) -> (
      match infer_tc ctx e with
      | Sum (t1, t2) ->
          let t = infer_tc (Context.add v1 t1 ctx) m1 in
          let t' = infer_tc (Context.add v2 t2 ctx) m2 in
          if t = t' then t
          else failwith "Branches of case arms don't have the same type"
      | _ -> failwith "Argument to case is not a sum")
  | Check (e, e1) ->
      tc ctx e Unit;
      infer_tc ctx e1
  | Prim (p, args) ->
      let arg_typs, res_typ = PrimType.primtype p in
      if List.length args != List.length arg_typs then
        failwith
          (Printf.sprintf "Primitive %s expects %d arguments, %d provided"
             (Prim.pp_prim p) (List.length arg_typs) (List.length args))
      else
        let typ_args = List.combine args arg_typs in
        List.fold_left (fun _ (e, t) -> tc ctx e t) () typ_args;
        res_typ
  | Let (x, v, e) -> infer_tc (Context.add x (infer_tc ctx v) ctx) e
  | Int _ -> Int_Typ
  | String _ -> String_Typ

let check_type (e : term) (t : typ) = tc Context.empty e t
let infer_type (e : term) = infer_tc Context.empty e
