open Ast

module Context = Map.Make(Variable)

let rec tc (ctx : typ Context.t) (e : term) (t : typ) =
  match e with 
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
      match infer_type ctx e1 with
      | Ok(Arrow(t1, t2)) -> 
            if t = t2 then 
              tc ctx e2 t2
            else
              Error (Printf.sprintf "Term %s is an arrow which returns an %s, but should return %s" (pp_term e1) (pp_typ t2) (pp_typ t))
      | Ok(t) -> Error (Printf.sprintf "First component of application %s is not a function and has type %s " (pp_term e) (pp_typ t))
      | Error e -> Error e
    )
  | Tup es -> raise (Failure "foo")
  | Proj (i, e) -> raise (Failure "foo")
  | Inj (t, i, e) -> raise (Failure "foo")
  | Case (e, arms) -> raise (Failure "foo")
  | Print (s, e) -> raise (Failure "foo")

and infer_type (ctx : typ Context.t) (e : term ) = raise (Failure "unimplemented")
let check_type (e : term) (t : typ) = tc Context.empty e t

