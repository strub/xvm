(* -------------------------------------------------------------------- *)
open Utils

module P = Parser
module L = Lexing

(* -------------------------------------------------------------------- *)
let lexbuf_from_channel = fun name channel ->
  let lexbuf = Lexing.from_channel channel in
    lexbuf.Lexing.lex_curr_p <- {
        Lexing.pos_fname = name;
        Lexing.pos_lnum  = 1;
        Lexing.pos_bol   = 0;
        Lexing.pos_cnum  = 0
      };
    lexbuf

(* -------------------------------------------------------------------- *)
let parserfun_entry =
    MenhirLib.Convert.Simplified.traditional2revised P.entry

(* -------------------------------------------------------------------- *)
let from_string ?(lineno = 1) ~name data =
  let lexbuf = Lexing.from_string data in
  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname = name;
    Lexing.pos_lnum  = lineno;
    Lexing.pos_bol   = 0;
    Lexing.pos_cnum  = 0;
  };
  lexbuf

(* -------------------------------------------------------------------- *)
let lexer (lexbuf : L.lexbuf) =
  let token = Lexer.main lexbuf in
  (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

(* -------------------------------------------------------------------- *)
let parse_line ?lineno ~(name : string) (line : string) =
  let line   =
    try  fst (String.split line ~by:";")
    with Not_found -> line in

  if String.trim line = "" then Syntax.P_Empty else
  let reader = from_string ?lineno ~name line in
  parserfun_entry (fun () -> lexer reader)

(* -------------------------------------------------------------------- *)
let parse_program ?(name = "") (inc : IO.input) =
  let lines = IO.lines_of2 inc in
  let prog  = Enum.mapi (fun i -> parse_line ~lineno:(i+1) ~name) lines in
  List.of_enum prog
