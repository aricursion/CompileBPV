open Ast

module Context = Map.Make(Variable)

let rec tc (ctx : typ Context.t) (e : term) (t : typ) =
  match e with 
  | Lambda (x, typ, e) -> raise (Failure "foo")
  | Ap (e1, e2) -> raise (Failure "foo")
  | Tup es -> raise (Failure "foo")
  | Proj (i, e) -> raise (Failure "foo")
  | Inj (t, i, e) -> raise (Failure "foo")
  | Case (e, arms) -> raise (Failure "foo")
  | Print (s, e) -> raise (Failure "foo")

let check_type (e : term) (t : typ) = tc Context.empty e t

let infer_type (e : term ) = raise (Failure "unimplemented")