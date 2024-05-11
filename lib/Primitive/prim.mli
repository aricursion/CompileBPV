type prim = Add | Sub | Mul | Div | IntToString | Print | Concat

val pp_prim : prim -> string

module type PrimTypeParam = sig
  type t

  val int_typ : t
  val unit : t
  val string_typ : t
end

module type PrimType = sig
  type t

  val primtype : prim -> t list * t
end

module PrimTypeFun (Param : PrimTypeParam) : PrimType with type t = Param.t

module type PrimEvalParam = sig
  type t
  type internal = IntCon of int | StringCon of string | UnitCon

  val internalize : t -> internal
  val externalize : internal -> t
end

module type PrimEval = sig
  type t

  val primeval : prim -> t list -> t
end

module PrimEvalFun (Param : PrimEvalParam) : PrimEval with type t = Param.t
