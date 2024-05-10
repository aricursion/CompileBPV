(* Credit to Kaustuv Chaudhuri, Frank Pfenning, Michael Duggan, and Alice Rao for overall parser structure *)
open Core

let initialize_lexbuf (filename : string) : Lexing.lexbuf -> unit =
  let open Lexing in
  let pos = { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  fun lexbuf ->
    lexbuf.lex_start_p <- pos;
    lexbuf.lex_curr_p <- pos

let parse (filename : string) : Ast.term =
  try
    let ast =
      In_channel.with_file filename ~f:(fun chan ->
          let lexbuf = Lexing.from_channel chan in
          initialize_lexbuf filename lexbuf;
          try Parser.program Lexer.initial lexbuf
          with _ -> failwith "Parse error.")
    in
    if List.length !Lexer.errors > 0 then failwith "Lex error." else ast
  with Sys_error s -> failwith ("System error: " ^ s)
