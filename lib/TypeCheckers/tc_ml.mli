val check_type : Ast.term -> Ast.typ -> (unit, string) result

val infer_type : Ast.term -> (Ast.typ, string) result
