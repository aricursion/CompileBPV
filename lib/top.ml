let compile (filename : string) = 
  let ast = Parse.parse filename in
  print_endline (Ast.pp_term ast);
  match Tc_ml.infer_type ast with
  | Ok tau -> 
      print_endline (Ast.pp_typ tau) ;
      print_endline (Cbpv_ast.pp_term (Cbpv_ast.Comp (Ml_to_cbpv.elab ast)))
  | Error s -> failwith s

let main () =
  let open Cmdliner in
  let cmd_line_info = Cmd.info "fsml" ~doc:"Compile a fake-SML source file." in
  let doc = "The source file $(docv) to compile." in
  let filename = Arg.(required (pos 0 (some non_dir_file) None (info [] ~doc ~docv:"FILE"))) in
  let cli_parse_result : string Cmd.t = Cmd.v cmd_line_info filename in
  match Cmd.eval_value cli_parse_result with
  | Ok (`Ok cmd_line) -> compile cmd_line
  | Ok (`Version)     -> Stdlib.exit (Cmd.Exit.ok)
  | Ok (`Help)        -> Stdlib.exit (Cmd.Exit.ok)
  | Error _           -> Stdlib.exit (Cmd.Exit.cli_error)
;;
