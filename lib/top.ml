let compile (filename : string) =
  let ast = Parse.parse filename in
  print_endline ("AST Term: " ^ Ast.pp_term ast);
  let tau = Tc_ml.infer_type ast in
  print_endline ("ML Type: " ^ Ast.pp_typ tau);
  print_newline ();
  let cbpv_ast = Ml_to_cbpv.elab ast in
  print_endline ("CBPV Term: " ^ Cbpv_ast.pp_term (Cbpv_ast.Comp cbpv_ast));
  let tau' = Tc_cbpv.infer_type cbpv_ast in
  print_endline ("CBPV Type: " ^ Cbpv_ast.pp_typ (CompTyp tau'));
  let runCBPV = Cbpv_interpreter.interpret cbpv_ast in
  print_endline
    (Printf.sprintf "CBPV interpreter returns with value: %s"
       (Cbpv_ast.pp_term (Val runCBPV)));
  print_newline ();
  let cc_ast = Cbpv_to_cc.translate cbpv_ast in
  print_endline ("CC Term: " ^ Cc_ast.pp_term (Comp cc_ast));
  let t = Tc_cc.infer_type cc_ast in
  print_endline ("CC Type: " ^ Cc_ast.pp_typ (CompTyp t));
  let runCC = Cc_interpreter.interpret cc_ast in
  print_endline
    (Printf.sprintf "CC interpreter returns with value: %s"
       (Cc_ast.pp_term (Val runCC)))

let main () =
  let open Cmdliner in
  let cmd_line_info = Cmd.info "fsml" ~doc:"Compile a fake-SML source file." in
  let doc = "The source file $(docv) to compile." in
  let filename =
    Arg.(required (pos 0 (some non_dir_file) None (info [] ~doc ~docv:"FILE")))
  in
  let cli_parse_result : string Cmd.t = Cmd.v cmd_line_info filename in
  match Cmd.eval_value cli_parse_result with
  | Ok (`Ok cmd_line) -> compile cmd_line
  | Ok `Version -> Stdlib.exit Cmd.Exit.ok
  | Ok `Help -> Stdlib.exit Cmd.Exit.ok
  | Error _ -> Stdlib.exit Cmd.Exit.cli_error
