(* -------------------------------------------------------------------- *)
{
  open Utils
  open Parser

  module L = Location

  (* ------------------------------------------------------------------ *)
  let lex_error lexbuf msg =
    let loc = L.of_lexbuf lexbuf in
    raise (Syntax.ParseError (Some loc, PE_LexicalError msg))

  (* ------------------------------------------------------------------ *)
  let _opcodes = [
    ("ADD"   , OC_ADD   );
    ("SUB"   , OC_SUB   );
    ("MIN"   , OC_SUB   );
    ("MUL"   , OC_MUL   );
    ("DIV"   , OC_DIV   );
    ("AND"   , OC_AND   );
    ("OR"    , OC_OR    );
    ("NOT"   , OC_NOT   );
    ("EQ"    , OC_EQ    );
    ("LT"    , OC_LT    );
    ("LE"    , OC_LE    );
    ("PUSH"  , OC_PUSH  );
    ("READ"  , OC_READ  );
    ("WRITE" , OC_WRITE );
    ("POP"   , OC_POP   );
    ("RET"   , OC_RET   );
    ("PXR"   , OC_PXR   );
    ("PRX"   , OC_PRX   );
    ("CREAD" , OC_CREAD );
    ("CWRITE", OC_CWRITE);
    ("ALLOC" , OC_ALLOC );
    ("RFR"   , OC_RFR   );
    ("WFR"   , OC_WFR   );
    ("FETCH" , OC_FETCH );
    ("GTO"   , OC_GTO   );
    ("GTZ"   , OC_GTZ   );
    ("GSB"   , OC_GSB   );
    ("STOP"  , OC_STOP  );
    ("PRT"   , OC_PRT   );
  ]

  (* ------------------------------------------------------------------ *)
  let opcodes =
    let table = Hashtbl.create 0 in
    List.iter (curry (Hashtbl.add table)) _opcodes; table
}

let empty   = ""
let blank   = [' ' '\t' '\r']
let newline = '\n'
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let letter  = upper | lower
let digit   = ['0'-'9']
let uint    = digit+
let ichar   = (letter | digit | '_' | '\'')
let ident   = (lower | '_') ichar*
let opcode  = upper ichar*

(* -------------------------------------------------------------------- *)
rule main = parse
  | newline       { Lexing.new_line lexbuf; NL }
  | blank+        { main lexbuf }
  | opcode as oc  { try Hashtbl.find opcodes oc with Not_found -> UNKNOWN oc }
  | ident  as id  { IDENT id }
  | digit+ as num { INT (Big_int.big_int_of_string num) }

  | ":" { COLON }
  | "-" { MINUS }
  | eof { EOF   }

  | _ as c
      { lex_error lexbuf (Printf.sprintf "illegal character: `%c'" c) }
