%{
  open Location
  open Syntax

  let error ?loc code = raise (ParseError (loc, code))
%}

%token <string> UNKNOWN
%token <Big_int.big_int> INT
%token <string> IDENT

%token OC_ADD
%token OC_SUB
%token OC_MUL
%token OC_DIV
%token OC_AND
%token OC_OR
%token OC_NOT
%token OC_EQ
%token OC_LT
%token OC_LE
%token OC_PUSH
%token OC_READ
%token OC_WRITE
%token OC_POP
%token OC_RET
%token OC_PXR
%token OC_PRX
%token OC_CREAD
%token OC_CWRITE
%token OC_ALLOC
%token OC_RFR
%token OC_WFR
%token OC_FETCH
%token OC_GTO
%token OC_GTZ
%token OC_GSB
%token OC_STOP
%token OC_PRT

%token COLON MINUS NL EOF

%type <Syntax.pentry> entry

%start entry
%%

(* -------------------------------------------------------------------- *)
entry:
| e=entry_r NL? EOF
    { e }

| x=loc(error)
    { error ~loc:(loc x) PE_Unknown }

(* -------------------------------------------------------------------- *)
%inline ident_r:
| x=IDENT { x }

%inline ident:
| x=loc(ident_r) { x }

(* -------------------------------------------------------------------- *)
word:
| i=loc(INT) {
    try  Big_int.int32_of_big_int (unloc i)
    with _ -> error ~loc:(loc i) (PE_InvalidLit `Word)
  }

| i=loc(MINUS i=INT { i }) {
    try  (Big_int.int32_of_big_int (Big_int.minus_big_int (unloc i)))
    with _ -> error ~loc:(loc i) (PE_InvalidLit `Word)
  }

(* -------------------------------------------------------------------- *)
addr:
| i=word    { P_ALit i }
| x=ident_r { P_ALbl x }

(* -------------------------------------------------------------------- *)
entry_r:
| i=loc(instr)
    { P_Instr i }

| x=ident COLON
    { P_Label x }

(* -------------------------------------------------------------------- *)
instr:
| OC_ADD  { P_OP `ADD }
| OC_SUB  { P_OP `SUB }
| OC_MUL  { P_OP `MUL }
| OC_DIV  { P_OP `DIV }
| OC_AND  { P_OP `AND }
| OC_OR   { P_OP `OR  }
| OC_NOT  { P_OP `NOT }
| OC_LT   { P_OP `LT  }
| OC_LE   { P_OP `LE  }
| OC_EQ   { P_OP `EQ  }

| OC_PUSH i=word { P_PUSH i }

| OC_READ   { P_READ   }
| OC_WRITE  { P_WRITE  }
| OC_POP    { P_POP    }
| OC_RET    { P_RET    }
| OC_PXR    { P_PXR    }
| OC_PRX    { P_PRX    }
| OC_CREAD  { P_CREAD  }
| OC_CWRITE { P_CWRITE }
| OC_ALLOC  { P_ALLOC  }

| OC_RFR i=word { P_RFR i }
| OC_WFR i=word { P_WFR i }

| OC_FETCH i=word { P_FETCH i }

| OC_GTO a=loc(addr)? { P_GTO a }
| OC_GTZ a=loc(addr)  { P_GTZ a }
| OC_GSB a=loc(addr)? { P_GSB a }

| OC_PRT  { P_PRT  }
| OC_STOP { P_STOP }

| x=loc(popcode) error
    { error ~loc:(loc x) (PE_InvalidOCArg (unloc x)) }

| x=loc(UNKNOWN)
    { error ~loc:(loc x) (PE_UnknownOC (unloc x)) }

(* -------------------------------------------------------------------- *)
%inline popcode:
| OC_ADD    { `OC_ADD    }
| OC_SUB    { `OC_SUB    }
| OC_MUL    { `OC_MUL    }
| OC_AND    { `OC_AND    }
| OC_OR     { `OC_OR     }
| OC_NOT    { `OC_NOT    }
| OC_EQ     { `OC_EQ     }
| OC_LT     { `OC_LT     }
| OC_LE     { `OC_LE     }
| OC_PUSH   { `OC_PUSH   }
| OC_READ   { `OC_READ   }
| OC_WRITE  { `OC_WRITE  }
| OC_POP    { `OC_POP    }
| OC_RET    { `OC_RET    }
| OC_PXR    { `OC_PXR    }
| OC_PRX    { `OC_PRX    }
| OC_CREAD  { `OC_CREAD  }
| OC_CWRITE { `OC_CWRITE }
| OC_ALLOC  { `OC_ALLOC  }
| OC_FETCH  { `OC_FETCH  }
| OC_GTO    { `OC_GTO    }
| OC_GTZ    { `OC_GTZ    }
| OC_GSB    { `OC_GSB    }
| OC_STOP   { `OC_STOP   }
| OC_PRT    { `OC_PRT    }

(* -------------------------------------------------------------------- *)
%inline loc(X):
| x=X {
    { pldesc = x;
      plloc  = Location.make $startpos $endpos;
    }
  }
