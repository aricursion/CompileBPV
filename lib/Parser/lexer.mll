{
open Core.Caml.Printf

module T = Parser

(* A record of all errors that happened. *)
let errors = Error_msg.create ()

let text = Lexing.lexeme

let from_lexbuf : Lexing.lexbuf -> Mark.src_span option =
  fun lexbuf ->
    Mark.of_positions
      (Lexing.lexeme_start_p lexbuf)
      (Lexing.lexeme_end_p lexbuf)
    |> Option.some

let error lexbuf ~msg : unit =
  let src_span = from_lexbuf lexbuf in
  Error_msg.error errors src_span ~msg

let dec_constant s lexbuf =
  let handle_int_min str =
    if String.equal str "2147483648"
      then "0x80000000"
      else str
  in
  let i32 =
    try Int32.of_string (handle_int_min s)
    with Failure _ ->
      let src_span = from_lexbuf lexbuf in
      Error_msg.error errors src_span
        ~msg:(sprintf "Cannot parse decimal constant `%s`" (text lexbuf));
      Int32.zero
  in
  T.Dec_const i32

  (* mutable state for parsing typedefs *)
  let typedef_idents : (string, unit) Core.Hashtbl.t = Core.Hashtbl.create (module Core.String)

  let add_typedef_ident ident lexbuf =
    if Core.Hashtbl.mem typedef_idents ident
      then error lexbuf ~msg:(sprintf "Identifier `%s` already defined" ident)
    else Core.Hashtbl.add_exn typedef_idents ~key:ident ~data:()
  
  let is_typedef_ident ident = Core.Hashtbl.mem typedef_idents ident

  let in_typedef = ref false 
  let in_struct = ref false 

  let ident_logic name lexbuf = 
    (* quickly print that you're parsing name as ident *)
    (* if (not !in_typedef) && !in_struct then (* error out *) 
      error lexbuf ~msg:(sprintf "shouldn't be in struct but not in typedef") else  *)
    match (!in_typedef, !in_struct) with 
      | (false, _) -> 
        if is_typedef_ident name 
        then 
          (* print name *) 
          (
            (* sprintf "parsing %s as typedef ident" name |> print_endline; *)
          T.TypIdent (Symbol.symbol name))
        else 
          (
            (* sprintf "parsing %s as regular ident" name |> print_endline; *)
          T.Ident (Symbol.symbol name))
      | (true, true) -> in_struct := false; 
      (* sprintf "parsing %s as regular ident" name |> print_endline;  *)
      T.Ident (Symbol.symbol name)
      | (true, false) -> 
        if is_typedef_ident name 
        then (* still in the typdef, just seeing a pretypdef'd ident *)
          (
            (* sprintf "parsing %s as typedef ident" name |> print_endline; *)
          T.TypIdent (Symbol.symbol name) )
        else (* no longer in the typedef, this is the new ident *)
          (add_typedef_ident name lexbuf; (* add to typedefs *)
          in_typedef := false; (* leave typdef *)
          (* sprintf "parsing %s as typedef ident" name |> print_endline; *)
          T.TypIdent (Symbol.symbol name)) (* emit token *)
}

let ident = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
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
  | '|' { T.Pipe }

  | ',' { T.Comma }

  | "=>"  { T.Arrow }
  | "fn"  { T.Lambda }

  | "->" { T.Arrow_typ }
  | '*' { T.Star }
  | '+' { T.Plus }

  | "unit"    { T.Unit }

  | "print"   { T.String }
  | "continue" { T.Continue }
  | "break"    { T.Break }

  | dec_num as n { dec_constant n lexbuf }


  | ident as name { ident_logic name lexbuf }
  | ("*" ws* ident ws* postop) | ("*" ident ws* "[" _* "]" ws* postop) { error lexbuf ~msg:"some day I will forget that this has ever happened to me and that is the day i will be free" ; initial lexbuf }

  | "/*" { comment 1 lexbuf }
  | "*/" { error lexbuf ~msg:"Unbalanced comments.";
           initial lexbuf
         }
  | "//" { comment_line lexbuf }

  | eof { T.Eof }

  | _  { error lexbuf
           ~msg:(sprintf "Illegal character '%s'" (text lexbuf));
         initial lexbuf
       }

and comment nesting = parse
  | "/*" { comment (nesting + 1) lexbuf }
  | "*/" { match nesting - 1 with
           | 0 -> initial lexbuf
           | nesting' -> comment nesting' lexbuf
         }
  | eof  { error lexbuf ~msg:"Reached EOF in comment.";
           T.Eof
         }
  | '\n' { Lexing.new_line lexbuf;
           comment nesting lexbuf
         }
  | _    { comment nesting lexbuf }

and comment_line = parse
  | '\n' { Lexing.new_line lexbuf;
           initial lexbuf
         }
  | eof  { error lexbuf ~msg:"Reached EOF in comment.";
           T.Eof
         }
  | _    { comment_line lexbuf }


{}
