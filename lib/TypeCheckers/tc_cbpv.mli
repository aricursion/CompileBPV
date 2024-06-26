val check_type : Cbpv_ast.comp_term -> Cbpv_ast.comp_type -> unit
val infer_type : Cbpv_ast.comp_term -> Cbpv_ast.comp_type

val infer_comp_type_ctx :
  Cbpv_ast.value_type Cbpv_ast.Context.t ->
  Cbpv_ast.comp_term ->
  Cbpv_ast.comp_type

val infer_value_type_ctx :
  Cbpv_ast.value_type Cbpv_ast.Context.t ->
  Cbpv_ast.value_term ->
  Cbpv_ast.value_type
