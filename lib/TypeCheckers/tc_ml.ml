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
  | Tup es -> (
      match t with 
      | Prod ts -> (
          if List.length ts = List.length es then
            let res = List.map (fun (e, t) -> tc ctx e t) (List.combine es ts) in
            match Base.Result.all res with
            | Ok _ -> Ok(())
            | Error e -> Error e
          else
            Error (Printf.sprintf "Term %s has type %s - tuple has incorrect length" (pp_term e) (pp_typ t))
        )
      | _ -> Error (Printf.sprintf "Term %s has type %s" (pp_term e) (pp_typ t))
    )
    | Split (m, (vars, e)) -> (
      match infer_tc ctx m with
      | Ok(Prod ts) -> 
          let new_ctx = List.fold_left (fun ctx (t, var) -> Context.add var t ctx) ctx (List.combine ts vars) in
          tc new_ctx e t
      | Ok(t) -> Error (Printf.sprintf "Tried to split on term %s with type %s" (pp_term m) (pp_typ t))
      | Error e -> Error e
    )
  | Inj (sum_t, i, e_inj) -> (
      if t = sum_t then
        match t with 
        | Sum ts -> 
          if 0 <= i && i < List.length ts then 
            tc ctx e_inj (List.nth ts i)
          else
            Error (Printf.sprintf "Injection into type %s has invalid index %i" (pp_typ sum_t) i)
        | _ -> Error (Printf.sprintf "Injection %s is not annotated with a sum - instead %s" (pp_term e) (pp_typ sum_t))
      else
        Error (Printf.sprintf "Expected type %s does not match type %s of expression %s" (pp_typ t) (pp_typ sum_t) (pp_term e_inj))
    )
  | Case (e, arms) -> (
      match infer_tc ctx e with
      | Ok (Sum ts) -> 
          if List.length ts = List.length arms then 
            let helper (t', (x, e')) = 
              let new_ctx = Context.add x t' ctx in
              tc new_ctx e' t
            in
            let res = List.map helper (List.combine ts arms) in 
            match Base.Result.all res with 
            | Ok _ -> Ok(())
            | Error e -> Error e
          else 
            Error "Number of arms in a case does not match the number of cases in the sum"
      | Ok _ -> Error "Tried to case on a non-sum"
      | Error e -> Error e
    )
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
  | Tup es -> (
      let res = List.map (infer_tc ctx) es in
      match Base.Result.all res with
      | Ok ts -> Ok (Prod ts)
      | Error e -> Error e
    )
  | Split (m, (vars, e)) -> (
      match infer_tc ctx m with
      | Ok(Prod ts) -> 
          let new_ctx = List.fold_left (fun ctx (t, var) -> Context.add var t ctx) ctx (List.combine ts vars) in
          infer_tc new_ctx e
      | Ok(t) -> Error (Printf.sprintf "Tried to split on term %s with type %s" (pp_term m) (pp_typ t))
      | Error e -> Error e
    )
  | Inj (t, i, e) -> (
      match t with 
      | Sum ts -> 
          if 0 <= i && i < List.length ts then 
            match tc ctx e (List.nth ts i) with
            | Ok _ -> Ok t
            | Error e -> Error e
          else 
            Error "Injection annotated sum has invalid index"
      | _ -> Error "Injection not annotated with sum"
    )
  | Case (e, arms) -> (
      match infer_tc ctx e with
      | Ok (Sum ts) -> (
        let helper (t', (x, e')) = 
          let new_ctx = Context.add x t' ctx in
          infer_tc new_ctx e'
        in
        let res = List.map helper (List.combine ts arms) in
        match Base.Result.all res with
        | Ok ts -> 
            if List.length ts = 0 then
              raise (Failure "How did you make a void weary weary weary")
            else
              if List.fold_left (fun b t -> b && (t = List.nth ts 0)) true ts then
                Ok (List.nth ts 0)
              else 
                Error "Branches of arms don't have the same type"
        | _ -> raise (Failure "todo")
        )
      | Ok _ -> Error "Argument to case is not a sum"
      | Error e -> Error e
    )
  | Print (_, e) ->  infer_tc ctx e


let check_type (e : term) (t : typ) = tc Context.empty e t

let infer_type (e : term) = infer_tc Context.empty e
