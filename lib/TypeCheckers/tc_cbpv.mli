val check_type : Cbpv_ast.comp_term -> Cbpv_ast.comp_type -> (unit, string) result

val infer_type : Cbpv_ast.comp_term -> (Cbpv_ast.comp_type, string) result