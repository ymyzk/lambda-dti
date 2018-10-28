open Format
open Lambda_dti

(* Altenative to Lexing.flush_input so that pos_bol is also reset to 0 *)
let flush_input lexbuf =
  Lexing.flush_input lexbuf;
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_bol = 0}

let debug = ref false

let rec read_eval_print lexbuf env tyenv =
  (* Used in all modes *)
  let print f = fprintf std_formatter f in
  (* Used in debug mode *)
  let print_debug f = Utils.Format.make_print_debug !debug f in
  flush stderr;
  flush stdout;
  if lexbuf.Lexing.lex_curr_p.pos_fname = "" then print "# @?";
  begin try
      (* Parsing *)
      print_debug "***** Parser *****\n";
      let e = Parser.toplevel Lexer.main lexbuf in
      print_debug "e: %a\n" Pp.ITGL.pp_program e;

      (* Type inference *)
      print_debug "***** Typing *****\n";
      let tyenv, e, u = Typing.ITGL.type_of_program tyenv e in
      print_debug "e: %a\n" Pp.ITGL.pp_program e;
      print_debug "U: %a\n" Pp.pp_ty u;

      (* NOTE: Typing.ITGL.translate and Typing.CC.type_of_program expect
       * normalized input *)
      let tyenv, e, u = Typing.ITGL.normalize tyenv e u in

      (* Translation *)
      print_debug "***** Cast-insertion *****\n";
      let tyenv, f, u' = Typing.ITGL.translate tyenv e in
      print_debug "f: %a\n" Pp.CC.pp_program f;
      print_debug "U: %a\n" Pp.pp_ty u';
      assert (Typing.is_equal u u');
      let u'' = Typing.CC.type_of_program tyenv f in
      assert (Typing.is_equal u u'');

      (* Evaluation *)
      print_debug "***** Eval *****\n";
      let env, x, v = Eval.eval_program env f ~debug:!debug in
      print "%a : %a = %a\n"
        pp_print_string x
        Pp.pp_ty2 u
        Pp.CC.pp_value v;
      read_eval_print lexbuf env tyenv
    with
    | Failure message ->
      print "Failure: %s\n" message;
      flush_input lexbuf
    | Parser.Error -> (* Menhir *)
      let token = Lexing.lexeme lexbuf in
      print "Parser.Error: unexpected token %s\n" token;
      flush_input lexbuf
    | Typing.Type_error message ->
      print "Type_error: %s\n" message
    | Eval.Blame (r, p) -> begin
        match p with
        | Pos -> print "Blame on the expression side:\n%a\n" Utils.Error.pp_range r
        | Neg -> print "Blame on the environment side:\n%a\n" Utils.Error.pp_range r
      end
  end;
  read_eval_print lexbuf env tyenv

let start file =
  let print_debug f = Utils.Format.make_print_debug !debug f in
  print_debug "***** Lexer *****\n";
  let channel, lexbuf = match file with
    | None ->
        print_debug "Reading from stdin\n%!";
        stdin, Lexing.from_channel stdin
    | Some f ->
        print_debug "Reading from file \"%s\"\n%!" f;
        let channel = open_in f in
        let lexbuf = Lexing.from_channel channel in
        lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = f};
        channel, lexbuf
  in
  let env, tyenv = Stdlib.pervasives in
  try
    read_eval_print lexbuf env tyenv
  with
    | Lexer.Eof ->
      (* Exiting normally *)
      close_in channel
    | Stdlib.Stdlib_exit i ->
      (* Exiting with the status code *)
      close_in channel;
      exit i
    | e ->
      (* Unexpected error occurs, close the opened channel *)
      close_in_noerr channel;
      raise e

let () =
  let program = Sys.argv.(0) in
  let usage =
    "Interpreter of the ITGL with dynamic type inference\n" ^
    asprintf "Usage: %s <options> [file]\n" program ^
    "Options: "
  in
  let file = ref None in
  let options = Arg.align [
      ("-d", Arg.Set debug, " Enable debug mode");
    ]
  in
  let parse_argv arg = match !file with
    | None -> file := Some arg
    | Some _ -> raise @@ Arg.Bad "error: only one file can be specified"
  in
  Arg.parse options parse_argv usage;
  start !file
