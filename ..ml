
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | Var of (
# 3 "lib/Parser/parser.mly"
       (Variable.t)
# 15 "..ml"
  )
    | Unit
    | String of (
# 4 "lib/Parser/parser.mly"
       (string)
# 21 "..ml"
  )
    | Star
    | Split
    | Semicolon
    | R_staple
    | R_paren
    | R_bracket
    | R_brace
    | Print
    | Plus
    | Pipe
    | Lambda
    | L_staple
    | L_paren
    | L_bracket
    | L_brace
    | Inj
    | In
    | Eof
    | Dec_const of (
# 2 "lib/Parser/parser.mly"
       (int)
# 44 "..ml"
  )
    | Comma
    | Colon
    | Check
    | Case
    | As
    | Arrow_typ
    | Arrow
  
end

include MenhirBasics

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_program) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: program. *)

  | MenhirState02 : (('s, _menhir_box_program) _menhir_cell1_Split, _menhir_box_program) _menhir_state
    (** State 02.
        Stack shape : Split.
        Start symbol: program. *)

  | MenhirState03 : (('s, _menhir_box_program) _menhir_cell1_Print, _menhir_box_program) _menhir_state
    (** State 03.
        Stack shape : Print.
        Start symbol: program. *)

  | MenhirState07 : (('s, _menhir_box_program) _menhir_cell1_Lambda _menhir_cell0_Var, _menhir_box_program) _menhir_state
    (** State 07.
        Stack shape : Lambda Var.
        Start symbol: program. *)

  | MenhirState09 : (('s, _menhir_box_program) _menhir_cell1_L_paren, _menhir_box_program) _menhir_state
    (** State 09.
        Stack shape : L_paren.
        Start symbol: program. *)

  | MenhirState11 : (('s, _menhir_box_program) _menhir_cell1_typ, _menhir_box_program) _menhir_state
    (** State 11.
        Stack shape : typ.
        Start symbol: program. *)

  | MenhirState13 : (('s, _menhir_box_program) _menhir_cell1_typ, _menhir_box_program) _menhir_state
    (** State 13.
        Stack shape : typ.
        Start symbol: program. *)

  | MenhirState16 : (('s, _menhir_box_program) _menhir_cell1_typ, _menhir_box_program) _menhir_state
    (** State 16.
        Stack shape : typ.
        Start symbol: program. *)

  | MenhirState20 : ((('s, _menhir_box_program) _menhir_cell1_Lambda _menhir_cell0_Var, _menhir_box_program) _menhir_cell1_typ, _menhir_box_program) _menhir_state
    (** State 20.
        Stack shape : Lambda Var typ.
        Start symbol: program. *)

  | MenhirState21 : (('s, _menhir_box_program) _menhir_cell1_L_paren, _menhir_box_program) _menhir_state
    (** State 21.
        Stack shape : L_paren.
        Start symbol: program. *)

  | MenhirState22 : (('s, _menhir_box_program) _menhir_cell1_L_bracket, _menhir_box_program) _menhir_state
    (** State 22.
        Stack shape : L_bracket.
        Start symbol: program. *)

  | MenhirState25 : (('s, _menhir_box_program) _menhir_cell1_Inj, _menhir_box_program) _menhir_state
    (** State 25.
        Stack shape : Inj.
        Start symbol: program. *)

  | MenhirState31 : ((('s, _menhir_box_program) _menhir_cell1_Inj, _menhir_box_program) _menhir_cell1_typ _menhir_cell0_Dec_const, _menhir_box_program) _menhir_state
    (** State 31.
        Stack shape : Inj typ Dec_const.
        Start symbol: program. *)

  | MenhirState32 : (('s, _menhir_box_program) _menhir_cell1_Check, _menhir_box_program) _menhir_state
    (** State 32.
        Stack shape : Check.
        Start symbol: program. *)

  | MenhirState33 : (('s, _menhir_box_program) _menhir_cell1_Case, _menhir_box_program) _menhir_state
    (** State 33.
        Stack shape : Case.
        Start symbol: program. *)

  | MenhirState35 : (('s, _menhir_box_program) _menhir_cell1_term, _menhir_box_program) _menhir_state
    (** State 35.
        Stack shape : term.
        Start symbol: program. *)

  | MenhirState38 : (('s, _menhir_box_program) _menhir_cell1_appTerm, _menhir_box_program) _menhir_state
    (** State 38.
        Stack shape : appTerm.
        Start symbol: program. *)

  | MenhirState39 : ((('s, _menhir_box_program) _menhir_cell1_appTerm, _menhir_box_program) _menhir_cell1_L_bracket, _menhir_box_program) _menhir_state
    (** State 39.
        Stack shape : appTerm L_bracket.
        Start symbol: program. *)

  | MenhirState43 : ((('s, _menhir_box_program) _menhir_cell1_Case, _menhir_box_program) _menhir_cell1_term _menhir_cell0_Var, _menhir_box_program) _menhir_state
    (** State 43.
        Stack shape : Case term Var.
        Start symbol: program. *)

  | MenhirState47 : (((('s, _menhir_box_program) _menhir_cell1_Case, _menhir_box_program) _menhir_cell1_term _menhir_cell0_Var, _menhir_box_program) _menhir_cell1_term _menhir_cell0_Var, _menhir_box_program) _menhir_state
    (** State 47.
        Stack shape : Case term Var term Var.
        Start symbol: program. *)

  | MenhirState51 : ((('s, _menhir_box_program) _menhir_cell1_Check, _menhir_box_program) _menhir_cell1_term, _menhir_box_program) _menhir_state
    (** State 51.
        Stack shape : Check term.
        Start symbol: program. *)

  | MenhirState56 : ((('s, _menhir_box_program) _menhir_cell1_L_bracket, _menhir_box_program) _menhir_cell1_term, _menhir_box_program) _menhir_state
    (** State 56.
        Stack shape : L_bracket term.
        Start symbol: program. *)

  | MenhirState68 : ((('s, _menhir_box_program) _menhir_cell1_Split, _menhir_box_program) _menhir_cell1_term _menhir_cell0_Var _menhir_cell0_Var, _menhir_box_program) _menhir_state
    (** State 68.
        Stack shape : Split term Var Var.
        Start symbol: program. *)


and ('s, 'r) _menhir_cell1_appTerm = 
  | MenhirCell1_appTerm of 's * ('s, 'r) _menhir_state * (
# 26 "lib/Parser/parser.mly"
      (Ast.term)
# 179 "..ml"
)

and ('s, 'r) _menhir_cell1_term = 
  | MenhirCell1_term of 's * ('s, 'r) _menhir_state * (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 186 "..ml"
)

and ('s, 'r) _menhir_cell1_typ = 
  | MenhirCell1_typ of 's * ('s, 'r) _menhir_state * (
# 23 "lib/Parser/parser.mly"
      (Ast.typ)
# 193 "..ml"
)

and ('s, 'r) _menhir_cell1_Case = 
  | MenhirCell1_Case of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_Check = 
  | MenhirCell1_Check of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_Dec_const = 
  | MenhirCell0_Dec_const of 's * (
# 2 "lib/Parser/parser.mly"
       (int)
# 206 "..ml"
)

and ('s, 'r) _menhir_cell1_Inj = 
  | MenhirCell1_Inj of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_L_bracket = 
  | MenhirCell1_L_bracket of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_L_paren = 
  | MenhirCell1_L_paren of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_Lambda = 
  | MenhirCell1_Lambda of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_Print = 
  | MenhirCell1_Print of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_Split = 
  | MenhirCell1_Split of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_Var = 
  | MenhirCell0_Var of 's * (
# 3 "lib/Parser/parser.mly"
       (Variable.t)
# 231 "..ml"
)

and _menhir_box_program = 
  | MenhirBox_program of (
# 24 "lib/Parser/parser.mly"
      (Ast.term)
# 238 "..ml"
) [@@unboxed]

let _menhir_action_01 =
  fun a ->
    (
# 51 "lib/Parser/parser.mly"
    ( a )
# 246 "..ml"
     : (
# 26 "lib/Parser/parser.mly"
      (Ast.term)
# 250 "..ml"
    ))

let _menhir_action_02 =
  fun m m1 ->
    (
# 53 "lib/Parser/parser.mly"
    ( Ast.Ap (m, m1) )
# 258 "..ml"
     : (
# 26 "lib/Parser/parser.mly"
      (Ast.term)
# 262 "..ml"
    ))

let _menhir_action_03 =
  fun m ->
    (
# 57 "lib/Parser/parser.mly"
      ( m )
# 270 "..ml"
     : (
# 25 "lib/Parser/parser.mly"
      (Ast.term)
# 274 "..ml"
    ))

let _menhir_action_04 =
  fun var ->
    (
# 59 "lib/Parser/parser.mly"
      ( Ast.Var var )
# 282 "..ml"
     : (
# 25 "lib/Parser/parser.mly"
      (Ast.term)
# 286 "..ml"
    ))

let _menhir_action_05 =
  fun () ->
    (
# 61 "lib/Parser/parser.mly"
      ( Ast.Triv )
# 294 "..ml"
     : (
# 25 "lib/Parser/parser.mly"
      (Ast.term)
# 298 "..ml"
    ))

let _menhir_action_06 =
  fun m ->
    (
# 33 "lib/Parser/parser.mly"
  ( m )
# 306 "..ml"
     : (
# 24 "lib/Parser/parser.mly"
      (Ast.term)
# 310 "..ml"
    ))

let _menhir_action_07 =
  fun m ->
    (
# 65 "lib/Parser/parser.mly"
      ( m )
# 318 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 322 "..ml"
    ))

let _menhir_action_08 =
  fun m1 m2 ->
    (
# 67 "lib/Parser/parser.mly"
      ( Ast.Tup (m1, m2) )
# 330 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 334 "..ml"
    ))

let _menhir_action_09 =
  fun body m v1 v2 ->
    (
# 69 "lib/Parser/parser.mly"
      ( Ast.Split (m, ((v1, v2), body)) )
# 342 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 346 "..ml"
    ))

let _menhir_action_10 =
  fun body typ var ->
    (
# 71 "lib/Parser/parser.mly"
      ( Ast.Lambda (var, typ, body) )
# 354 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 358 "..ml"
    ))

let _menhir_action_11 =
  fun i m typ ->
    (
# 73 "lib/Parser/parser.mly"
      ( Ast.Inj (typ, i, m) )
# 366 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 370 "..ml"
    ))

let _menhir_action_12 =
  fun m m1 m2 v1 v2 ->
    (
# 75 "lib/Parser/parser.mly"
      ( Ast.Case (m, (v1, m1), (v2, m2)) )
# 378 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 382 "..ml"
    ))

let _menhir_action_13 =
  fun body m ->
    (
# 77 "lib/Parser/parser.mly"
      ( Ast.Check (m, body) )
# 390 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 394 "..ml"
    ))

let _menhir_action_14 =
  fun t ->
    (
# 79 "lib/Parser/parser.mly"
      ( Ast.Prim (Prim.Print, [t]) )
# 402 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 406 "..ml"
    ))

let _menhir_action_15 =
  fun m n ->
    (
# 81 "lib/Parser/parser.mly"
      ( Ast.Prim (Prim.Add, [m; n]))
# 414 "..ml"
     : (
# 22 "lib/Parser/parser.mly"
      (Ast.term)
# 418 "..ml"
    ))

let _menhir_action_16 =
  fun () ->
    (
# 38 "lib/Parser/parser.mly"
      ( Ast.Unit )
# 426 "..ml"
     : (
# 23 "lib/Parser/parser.mly"
      (Ast.typ)
# 430 "..ml"
    ))

let _menhir_action_17 =
  fun t ->
    (
# 40 "lib/Parser/parser.mly"
      ( t )
# 438 "..ml"
     : (
# 23 "lib/Parser/parser.mly"
      (Ast.typ)
# 442 "..ml"
    ))

let _menhir_action_18 =
  fun t1 t2 ->
    (
# 42 "lib/Parser/parser.mly"
      ( Ast.Prod (t1, t2) )
# 450 "..ml"
     : (
# 23 "lib/Parser/parser.mly"
      (Ast.typ)
# 454 "..ml"
    ))

let _menhir_action_19 =
  fun t1 t2 ->
    (
# 44 "lib/Parser/parser.mly"
      ( Ast.Sum (t1, t2) )
# 462 "..ml"
     : (
# 23 "lib/Parser/parser.mly"
      (Ast.typ)
# 466 "..ml"
    ))

let _menhir_action_20 =
  fun t1 t2 ->
    (
# 46 "lib/Parser/parser.mly"
      ( Ast.Arrow (t1, t2) )
# 474 "..ml"
     : (
# 23 "lib/Parser/parser.mly"
      (Ast.typ)
# 478 "..ml"
    ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | Arrow ->
        "Arrow"
    | Arrow_typ ->
        "Arrow_typ"
    | As ->
        "As"
    | Case ->
        "Case"
    | Check ->
        "Check"
    | Colon ->
        "Colon"
    | Comma ->
        "Comma"
    | Dec_const _ ->
        "Dec_const"
    | Eof ->
        "Eof"
    | In ->
        "In"
    | Inj ->
        "Inj"
    | L_brace ->
        "L_brace"
    | L_bracket ->
        "L_bracket"
    | L_paren ->
        "L_paren"
    | L_staple ->
        "L_staple"
    | Lambda ->
        "Lambda"
    | Pipe ->
        "Pipe"
    | Plus ->
        "Plus"
    | Print ->
        "Print"
    | R_brace ->
        "R_brace"
    | R_bracket ->
        "R_bracket"
    | R_paren ->
        "R_paren"
    | R_staple ->
        "R_staple"
    | Semicolon ->
        "Semicolon"
    | Split ->
        "Split"
    | Star ->
        "Star"
    | String _ ->
        "String"
    | Unit ->
        "Unit"
    | Var _ ->
        "Var"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let var = _v in
      let _v = _menhir_action_04 var in
      _menhir_goto_atom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_atom : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState38 ->
          _menhir_run_40 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState68 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState20 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState21 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState22 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState56 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState31 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState32 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState51 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState33 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState43 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState47 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState35 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_40 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_appTerm -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_appTerm (_menhir_stack, _menhir_s, m) = _menhir_stack in
      let m1 = _v in
      let _v = _menhir_action_02 m m1 in
      _menhir_goto_appTerm _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_appTerm : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Var _v_0 ->
          let _menhir_stack = MenhirCell1_appTerm (_menhir_stack, _menhir_s, _v) in
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState38
      | L_paren ->
          let _menhir_stack = MenhirCell1_appTerm (_menhir_stack, _menhir_s, _v) in
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState38
      | L_bracket ->
          let _menhir_stack = MenhirCell1_appTerm (_menhir_stack, _menhir_s, _v) in
          let _menhir_stack = MenhirCell1_L_bracket (_menhir_stack, MenhirState38) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | R_bracket ->
              _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
          | _ ->
              _eRR ())
      | As | Comma | Eof | In | L_brace | Pipe | Plus | R_brace | R_bracket | R_paren ->
          let m = _v in
          let _v = _menhir_action_07 m in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_21 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_paren (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState21 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Split ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Print ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lambda ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_bracket ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Inj ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Check ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Case ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_Split (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState02 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Split ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Print ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lambda ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_bracket ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Inj ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Check ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Case ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_Print (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Split ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Print ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lambda ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_bracket ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Inj ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Check ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Case ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_Lambda (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | L_paren ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              let _menhir_stack = MenhirCell0_Var (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Colon ->
                  let _menhir_s = MenhirState07 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | Unit ->
                      _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | L_paren ->
                      _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_16 () in
      _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState25 ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState16 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState13 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_26 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_Inj as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Star ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | R_staple ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | L_paren ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Dec_const _v ->
                  let _menhir_stack = MenhirCell0_Dec_const (_menhir_stack, _v) in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | R_paren ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | L_paren ->
                          let _menhir_s = MenhirState31 in
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | Var _v ->
                              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                          | Split ->
                              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Print ->
                              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Lambda ->
                              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | L_paren ->
                              _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | L_bracket ->
                              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Inj ->
                              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Check ->
                              _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Case ->
                              _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | Plus ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Arrow_typ ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_typ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState11 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Unit ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_paren (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState09 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Unit ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_22 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_L_bracket (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState22 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Split ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | R_bracket ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Print ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lambda ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_bracket ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Inj ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Check ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Case ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_23 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_L_bracket -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_L_bracket (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_05 () in
      _menhir_goto_atom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_24 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_Inj (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | L_staple ->
          let _menhir_s = MenhirState25 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Unit ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_paren ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_32 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_Check (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState32 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Split ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Print ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lambda ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_bracket ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Inj ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Check ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Case ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_33 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_Case (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState33 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Split ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Print ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lambda ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_bracket ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Inj ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Check ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Case ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_typ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState13 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Unit ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_typ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState16 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Unit ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_18 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_Lambda _menhir_cell0_Var as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Star ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | R_paren ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Arrow ->
              let _menhir_s = MenhirState20 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Var _v ->
                  _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | Split ->
                  _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | Print ->
                  _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | Lambda ->
                  _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_paren ->
                  _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | L_bracket ->
                  _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | Inj ->
                  _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | Check ->
                  _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | Case ->
                  _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | Plus ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Arrow_typ ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_17 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_typ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Star ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Plus ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Arrow_typ ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | R_paren | R_staple ->
          let MenhirCell1_typ (_menhir_stack, _menhir_s, t1) = _menhir_stack in
          let t2 = _v in
          let _v = _menhir_action_20 t1 t2 in
          _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_typ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Plus ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Arrow_typ | R_paren | R_staple | Star ->
          let MenhirCell1_typ (_menhir_stack, _menhir_s, t1) = _menhir_stack in
          let t2 = _v in
          let _v = _menhir_action_19 t1 t2 in
          _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_typ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Star ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Plus ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Arrow_typ | R_paren | R_staple ->
          let MenhirCell1_typ (_menhir_stack, _menhir_s, t1) = _menhir_stack in
          let t2 = _v in
          let _v = _menhir_action_18 t1 t2 in
          _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_L_paren as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Star ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | R_paren ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_L_paren (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_17 t in
          _menhir_goto_typ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | Plus ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Arrow_typ ->
          let _menhir_stack = MenhirCell1_typ (_menhir_stack, _menhir_s, _v) in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_term : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_70 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState68 ->
          _menhir_run_69 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState02 ->
          _menhir_run_63 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_62 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState20 ->
          _menhir_run_61 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState21 ->
          _menhir_run_59 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState56 ->
          _menhir_run_57 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState22 ->
          _menhir_run_55 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState31 ->
          _menhir_run_53 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState51 ->
          _menhir_run_52 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState32 ->
          _menhir_run_50 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState47 ->
          _menhir_run_48 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState43 ->
          _menhir_run_44 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState35 ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState33 ->
          _menhir_run_34 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_70 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Plus ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Eof ->
          let m = _v in
          let _v = _menhir_action_06 m in
          MenhirBox_program _v
      | _ ->
          _eRR ()
  
  and _menhir_run_35 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_term -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState35 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Split ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Print ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lambda ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_bracket ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Inj ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Check ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Case ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_69 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_Split, _menhir_box_program) _menhir_cell1_term _menhir_cell0_Var _menhir_cell0_Var -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_Var (_menhir_stack, v2) = _menhir_stack in
      let MenhirCell0_Var (_menhir_stack, v1) = _menhir_stack in
      let MenhirCell1_term (_menhir_stack, _, m) = _menhir_stack in
      let MenhirCell1_Split (_menhir_stack, _menhir_s) = _menhir_stack in
      let body = _v in
      let _v = _menhir_action_09 body m v1 v2 in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_63 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_Split as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Plus ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | As ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              let _menhir_stack = MenhirCell0_Var (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Comma ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | Var _v ->
                      let _menhir_stack = MenhirCell0_Var (_menhir_stack, _v) in
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      (match (_tok : MenhirBasics.token) with
                      | In ->
                          let _menhir_s = MenhirState68 in
                          let _tok = _menhir_lexer _menhir_lexbuf in
                          (match (_tok : MenhirBasics.token) with
                          | Var _v ->
                              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                          | Split ->
                              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Print ->
                              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Lambda ->
                              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | L_paren ->
                              _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | L_bracket ->
                              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Inj ->
                              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Check ->
                              _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | Case ->
                              _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                          | _ ->
                              _eRR ())
                      | _ ->
                          _eRR ())
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_62 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_Print as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Plus ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | As | Comma | Eof | In | L_brace | Pipe | R_brace | R_bracket | R_paren ->
          let MenhirCell1_Print (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_14 t in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_61 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_Lambda _menhir_cell0_Var, _menhir_box_program) _menhir_cell1_typ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Plus ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | As | Comma | Eof | In | L_brace | Pipe | R_brace | R_bracket | R_paren ->
          let MenhirCell1_typ (_menhir_stack, _, typ) = _menhir_stack in
          let MenhirCell0_Var (_menhir_stack, var) = _menhir_stack in
          let MenhirCell1_Lambda (_menhir_stack, _menhir_s) = _menhir_stack in
          let body = _v in
          let _v = _menhir_action_10 body typ var in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_59 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_L_paren as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_paren ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_L_paren (_menhir_stack, _menhir_s) = _menhir_stack in
          let m = _v in
          let _v = _menhir_action_03 m in
          _menhir_goto_atom _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | Plus ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_57 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_L_bracket, _menhir_box_program) _menhir_cell1_term as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_bracket ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_term (_menhir_stack, _, m1) = _menhir_stack in
          let MenhirCell1_L_bracket (_menhir_stack, _menhir_s) = _menhir_stack in
          let m2 = _v in
          let _v = _menhir_action_08 m1 m2 in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | Plus ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_55 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_L_bracket as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Plus ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Comma ->
          let _menhir_s = MenhirState56 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Split ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Print ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lambda ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_paren ->
              _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_bracket ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Inj ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Check ->
              _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Case ->
              _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_53 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_Inj, _menhir_box_program) _menhir_cell1_typ _menhir_cell0_Dec_const as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_paren ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_Dec_const (_menhir_stack, i) = _menhir_stack in
          let MenhirCell1_typ (_menhir_stack, _, typ) = _menhir_stack in
          let MenhirCell1_Inj (_menhir_stack, _menhir_s) = _menhir_stack in
          let m = _v in
          let _v = _menhir_action_11 i m typ in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | Plus ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_52 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_Check, _menhir_box_program) _menhir_cell1_term -> _ -> _ -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_term (_menhir_stack, _, m) = _menhir_stack in
      let MenhirCell1_Check (_menhir_stack, _menhir_s) = _menhir_stack in
      let body = _v in
      let _v = _menhir_action_13 body m in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_50 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_Check as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Plus ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | In ->
          let _menhir_s = MenhirState51 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | Split ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Print ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Lambda ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_paren ->
              _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | L_bracket ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Inj ->
              _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Check ->
              _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | Case ->
              _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_48 : type  ttv_stack. ((((ttv_stack, _menhir_box_program) _menhir_cell1_Case, _menhir_box_program) _menhir_cell1_term _menhir_cell0_Var, _menhir_box_program) _menhir_cell1_term _menhir_cell0_Var as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | R_brace ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_Var (_menhir_stack, v2) = _menhir_stack in
          let MenhirCell1_term (_menhir_stack, _, m1) = _menhir_stack in
          let MenhirCell0_Var (_menhir_stack, v1) = _menhir_stack in
          let MenhirCell1_term (_menhir_stack, _, m) = _menhir_stack in
          let MenhirCell1_Case (_menhir_stack, _menhir_s) = _menhir_stack in
          let m2 = _v in
          let _v = _menhir_action_12 m m1 m2 v1 v2 in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | Plus ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_44 : type  ttv_stack. (((ttv_stack, _menhir_box_program) _menhir_cell1_Case, _menhir_box_program) _menhir_cell1_term _menhir_cell0_Var as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Plus ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | Pipe ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              let _menhir_stack = MenhirCell0_Var (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Arrow ->
                  let _menhir_s = MenhirState47 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | Var _v ->
                      _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | Split ->
                      _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Print ->
                      _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Lambda ->
                      _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | L_paren ->
                      _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | L_bracket ->
                      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Inj ->
                      _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Check ->
                      _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Case ->
                      _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_36 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_term as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | Plus ->
          let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | As | Comma | Eof | In | L_brace | Pipe | R_brace | R_bracket | R_paren ->
          let MenhirCell1_term (_menhir_stack, _menhir_s, m) = _menhir_stack in
          let n = _v in
          let _v = _menhir_action_15 m n in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_34 : type  ttv_stack. ((ttv_stack, _menhir_box_program) _menhir_cell1_Case as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | Plus ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer
      | L_brace ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Var _v ->
              let _menhir_stack = MenhirCell0_Var (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Arrow ->
                  let _menhir_s = MenhirState43 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | Var _v ->
                      _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | Split ->
                      _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Print ->
                      _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Lambda ->
                      _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | L_paren ->
                      _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | L_bracket ->
                      _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Inj ->
                      _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Check ->
                      _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | Case ->
                      _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_37 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let a = _v in
      let _v = _menhir_action_01 a in
      _menhir_goto_appTerm _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Var _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | Split ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Print ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Lambda ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_paren ->
          _menhir_run_21 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | L_bracket ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Inj ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Check ->
          _menhir_run_32 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | Case ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 84 "lib/Parser/parser.mly"
  

# 1548 "..ml"
