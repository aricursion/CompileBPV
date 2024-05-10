
(* The type of tokens. *)

type token = 
  | Var of (Variable.t)
  | Unit
  | String of (string)
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
  | Dec_const of (int)
  | Comma
  | Colon
  | Check
  | Case
  | As
  | Arrow_typ
  | Arrow

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.term)
