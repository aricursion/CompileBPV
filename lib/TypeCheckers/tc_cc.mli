val check_type : Cc_ast.comp_term -> Cc_ast.comp_type -> (unit, string) result

val infer_type : Cc_ast.comp_term -> (Cc_ast.comp_type, string) result