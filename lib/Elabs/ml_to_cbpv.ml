open Cbpv_ast

let rec elab (t : Ast.term) : comp_term = 
  match t with
  | Ast.Var x -> Ret (Var x)
  | Lambda _ -> failwith "unimplemented"
  | Ap _ -> failwith "unimplemented"
  | Triv -> failwith "unimplemented"
  | Tup _ -> failwith "unimplemented"
  | Split _ -> failwith "unimplemented"
  | Check _ -> failwith "unimplemented"
  | Inj _ -> failwith "unimplemented"
  | Case _ -> failwith "unimplemented"
  | Print _ -> failwith "unimplemented"