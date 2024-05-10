type prim = Add | Sub | Mul | Div | IntToString | Print | Concat

let pp_prim p =
  match p with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | IntToString -> "I2S"
  | Print -> "Print"
  | Concat -> "^"

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

module PrimTypeFun (Param : PrimTypeParam) : PrimType with type t = Param.t =
struct
  type t = Param.t

  let primtype p =
    match p with
    | Add | Sub | Mul | Div -> ([ Param.int_typ; Param.int_typ ], Param.int_typ)
    | IntToString -> ([ Param.int_typ ], Param.string_typ)
    | Print -> ([ Param.string_typ ], Param.unit)
    | Concat -> ([ Param.string_typ; Param.string_typ ], Param.unit)
end
