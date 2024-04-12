open Ast

let rec tc (ctx : typ Context.t) (e : term) (t : typ) =
  match e with 
  | Lambda (x, typ, e) -> (
      match t with 
      | Arrow (t1, t2) ->
          if t1 = typ then 
            let _ = Context.insert ctx x t1 in tc ctx e t2
          else
            false
      | _ -> false
    )
  | Ap (e1, e2) -> (
      match e1 with 
      | Lambda (x, typ, e) -> raise (Failure "foo")
      | _ -> false
    )
  | Tup es -> raise (Failure "foo")
  | Proj (i, e) -> raise (Failure "foo")
  | Inj (t, i, e) -> raise (Failure "foo")
  | Case (e, arms) -> raise (Failure "foo")
  | Print (s, e) -> raise (Failure "foo")

let check_type (e : term) (t : typ) = tc (Context.empty ()) e t

let infer_type (e : term ) = raise (Failure "unimplemented")