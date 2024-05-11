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

module PrimEvalFun (Param : PrimEvalParam) : PrimEval with type t = Param.t =
struct
  type t = Param.t

  let primeval p args =
    match p with
    | Add -> (
        match
          ( Param.internalize (List.nth args 0),
            Param.internalize (List.nth args 1) )
        with
        | IntCon i, IntCon j -> Param.externalize (IntCon (i + j))
        | _ -> failwith "Tried to add non-integers")
    | Sub -> (
        match
          ( Param.internalize (List.nth args 0),
            Param.internalize (List.nth args 1) )
        with
        | IntCon i, IntCon j -> Param.externalize (IntCon (i - j))
        | _ -> failwith "Tried to sub non-integers")
    | Mul -> (
        match
          ( Param.internalize (List.nth args 0),
            Param.internalize (List.nth args 1) )
        with
        | IntCon i, IntCon j -> Param.externalize (IntCon (i * j))
        | _ -> failwith "Tried to mul non-integers")
    | Div -> (
        match
          ( Param.internalize (List.nth args 0),
            Param.internalize (List.nth args 1) )
        with
        | IntCon i, IntCon j -> Param.externalize (IntCon (i / j))
        | _ -> failwith "Tried to div non-integers")
    | IntToString -> (
        match Param.internalize (List.nth args 0) with
        | IntCon i -> Param.externalize (StringCon (string_of_int i))
        | _ -> failwith "Tried to Convert non-int to string")
    | Print -> (
        match Param.internalize (List.nth args 0) with
        | StringCon s ->
            print_string s;
            Param.externalize UnitCon
        | _ -> failwith "Tried to print non-string")
    | Concat -> (
        match
          ( Param.internalize (List.nth args 0),
            Param.internalize (List.nth args 1) )
        with
        | StringCon s1, StringCon s2 -> Param.externalize (StringCon (s1 ^ s2))
        | _ -> failwith "Tried to concat non-strings")
end
