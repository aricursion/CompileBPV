{
open Core.Caml.Printf

module T = Parser

let errors : string list ref = ref []

let text = Lexing.lexeme

let dec_constant s lexbuf =
  let i =
    try int_of_string s
    with Failure _ ->
      errors := (sprintf "Cannot parse decimal constant `%s`" (text lexbuf)) :: !errors;
      0
  in
  T.Dec_const i
}

let var = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let dec_num = ("0" | ['1'-'9'](['0'-'9']*))

let ws = [' ' '\t' '\r' '\011' '\012']

rule initial = parse
  | ws+  { initial lexbuf }
  | '\n' { Lexing.new_line lexbuf;
           initial lexbuf
         }

  | '(' { T.L_paren }
  | ')' { T.R_paren }
  | '<' { T.L_bracket }
  | '>' { T.R_bracket }
  | '{' { T.L_brace }
  | '}' { T.R_brace }
  | '[' { T.L_staple }
  | ']' { T.R_staple }
  | '|' { T.Pipe }
  | ':' { T.Colon }
  | ';' { T.Semicolon }
  | ',' { T.Comma }
  | '"' { T.Quote }

  | "=>"  { T.Arrow }
  | "fn"  { T.Lambda }

  | "->" { T.Arrow_typ }
  | '*'  { T.Star }
  | '+'  { T.Plus }

  | "unit"    { T.Unit }
  | "void"    { T.Void }

  | "print"   { T.Print }
  | "inj"     { T.Inj }
  | "split"   { T.Split }
  | "case"    { T.Case }

  | "as"      { T.As }
  | "in"      { T.In }

  | dec_num as n { dec_constant n lexbuf }

  | var as name { T.Var (Variable.of_string name) }

  | "(*" { comment 1 lexbuf }
  | "*)" { errors := "Unbalanced comments." :: !errors ;
           initial lexbuf
         }

  | eof { T.Eof }

  | _  { errors := (sprintf "Illegal character '%s'" (text lexbuf)) :: !errors;
         initial lexbuf
       }

and comment nesting = parse
  | "(*" { comment (nesting + 1) lexbuf }
  | "*)" { match nesting - 1 with
           | 0 -> initial lexbuf
           | nesting' -> comment nesting' lexbuf
         }
  | eof  { errors := "Reached EOF in comment." :: !errors;
           T.Eof
         }
  | '\n' { Lexing.new_line lexbuf;
           comment nesting lexbuf
         }
  | _    { comment nesting lexbuf }


{}
