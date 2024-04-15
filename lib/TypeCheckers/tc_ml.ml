open Ast

module Context = Map.Make(Variable)

let rec tc (ctx : typ Context.t) (e : term) (t : typ) =
  match e with 
  | Var x -> (
      match Context.find_opt x ctx with
      | Some(t') ->
          if t' = t then
            Ok(())
          else 
            Error (Printf.sprintf "Variable %s is in the context, but has type %s when expected %s" (Variable.pp_var x) (pp_typ t') (pp_typ t))
      | None -> Error (Printf.sprintf "Variable %s is not in the context" (Variable.pp_var x))
    )
  | Lambda (x, typ, e') -> (
      match t with
      | Arrow(t1, t2) -> 
          if t1 = typ then
            let new_ctx = Context.add x t1 ctx in tc new_ctx e' t2
          else
            Error (Printf.sprintf "Term %s does not have expected type %s" (pp_term e) (pp_typ t))
      | _ -> Error (Printf.sprintf "Term %s does not have expected type %s" (pp_term e) (pp_typ t))
    )
  | Ap (e1, e2) -> (
      match infer_tc ctx e1 with
      | Ok(Arrow(t1, t2)) -> 
            if t = t2 then 
              tc ctx e2 t1
            else
              Error (Printf.sprintf "Term %s is an arrow which returns an %s, but should return %s" (pp_term e1) (pp_typ t2) (pp_typ t))
      | Ok(t) -> Error (Printf.sprintf "First component of application %s is not a function and has type %s " (pp_term e) (pp_typ t))
      | Error e -> Error e
    )
  | Triv -> if t = Unit then Ok(()) else Error "Somehow <> doesn't have type unit"
  | Tup (e1, e2) -> (
      match t with 
      | Prod (t1, t2) -> (
        match (tc ctx e1 t1, tc ctx e2 t2) with
        | (Ok _, Ok _) -> Ok(())
        | (Error e, _) -> Error e
        | (_, Error e) -> Error e
        )
      | _ -> Error (Printf.sprintf "Term %s has type %s" (pp_term e) (pp_typ t))
    )
    | Split (m, ((v1, v2), e)) -> (
      match infer_tc ctx m with
      | Ok(Prod (t1, t2)) -> 
          let new_ctx = Context.add v2 t2 (Context.add v1 t1 ctx) in
          tc new_ctx e t
      | Ok(t) -> Error (Printf.sprintf "Tried to split on term %s with type %s" (pp_term m) (pp_typ t))
      | Error e -> Error e
    )
  | Inj (sum_t, i, e_inj) -> (
      if t = sum_t then
        match t with 
        | Sum (t1, t2) -> 
          if i = 1 then (* injections are no longer zero indexed *)
            tc ctx e_inj t1
          else if i = 2 then
            tc ctx e_inj t2
          else
            Error (Printf.sprintf "Injection into type %s has invalid index %i" (pp_typ sum_t) i)
        | _ -> Error (Printf.sprintf "Injection %s is not annotated with a sum - instead %s" (pp_term e) (pp_typ sum_t))
      else
        Error (Printf.sprintf "Expected type %s does not match type %s of expression %s" (pp_typ t) (pp_typ sum_t) (pp_term e_inj))
    )
  | Case (e, (v1, m1), (v2, m2)) -> (
      match infer_tc ctx e with
      | Ok (Sum (t1, t2)) -> 
        (match (tc (Context.add v1 t1 ctx) m1 t, tc (Context.add v2 t2 ctx) m2 t) with 
        | (Ok _, Ok _) -> Ok(())
        | (Error e, _) -> Error e
        | (_, Error e) -> Error e)
      | Ok _ -> Error "Tried to case on a non-sum"
      | Error e -> Error e
    )
  | Check (e, e1) ->
    (match infer_tc ctx e with
    | Ok(Unit) -> tc ctx e1 t
    | Ok _ -> Error (Printf.sprintf "Expression being checked does not have unit type: `%s`" (Ast.pp_term e))
    | Error e -> Error e)
  | Print (_, e) -> tc ctx e t

and infer_tc (ctx : typ Context.t) (e : term ) = 
  match e with 
  | Var x -> (
    match Context.find_opt x ctx with
    | Some(t) -> Ok(t)
    | None -> Error (Printf.sprintf "Variable %s is not in the context" (Variable.pp_var x))
   )
  | Lambda (x, t1, e) -> (
      let new_ctx = Context.add x t1 ctx in
      match infer_tc new_ctx e with
      | Ok (t2) -> Ok (Arrow (t1, t2))
      | Error e -> Error e
    )
  | Ap (e1, e2) -> (
    match (infer_tc ctx e1, infer_tc ctx e2) with
    | (Ok t1, Ok t') -> (
        match t1 with
        | Arrow (t1, t2) -> 
            if t1 = t' then
              Ok t2
            else 
              Error "Error while inferring application - input type doesn't match negative position in arrow"
        | _ -> Error "Error while inferring application - first arg is not an arrow"
      )
    | (Ok _, Error e) -> Error e
    | (Error e, _) -> Error e
    )
  | Triv -> Ok(Unit)
  | Tup (e1, e2) -> (
      match (infer_tc ctx e1, infer_tc ctx e2) with
      | (Ok t1, Ok t2) -> Ok (Prod (t1, t2))
      | (Error e, _) -> Error e
      | (_, Error e) -> Error e
    )
  | Split (m, ((v1, v2), e)) -> (
      match infer_tc ctx m with
      | Ok(Prod (t1, t2)) -> 
          infer_tc (Context.add v2 t2 (Context.add v1 t1 ctx)) e
      | Ok(t) -> Error (Printf.sprintf "Tried to split on term %s with type %s" (pp_term m) (pp_typ t))
      | Error e -> Error e
    )
  | Inj (t, i, e) -> (
      match t with 
      | Sum (t1, t2) -> 
          if i = 1 then 
            (match tc ctx e t1 with
            | Ok _ -> Ok t
            | Error e -> Error e)
          else if i = 2 then
            (match tc ctx e t2 with
            | Ok _ -> Ok t
            | Error e -> Error e) 
          else 
            Error "Injection annotated sum has invalid index"
      | _ -> Error "Injection not annotated with sum"
    )
  | Case (e, (v1, m1), (v2, m2)) -> (
      match infer_tc ctx e with
      | Ok (Sum (t1, t2)) -> (
        match (infer_tc (Context.add v1 t1 ctx) m1, infer_tc (Context.add v2 t2 ctx) m2) with
        | (Ok t, Ok t') -> 
            if t = t' then
              Ok t
            else 
              Error "Branches of case arms don't have the same type"
        | (Error e, _) -> Error e
        | (_, Error e) -> Error e
        )
      | Ok _ -> Error "Argument to case is not a sum"
      | Error e -> Error e
    )
  | Check (e, e1) -> 
    (match infer_tc ctx e with
    | Ok(Unit) -> infer_tc ctx e1
    | Ok _ -> Error (Printf.sprintf "Term being checked does not have unit type: `%s`" (Ast.pp_term e))
    | Error e -> Error e)
  | Print (_, e) ->  infer_tc ctx e


let check_type (e : term) (t : typ) = tc Context.empty e t

let infer_type (e : term) = infer_tc Context.empty e
