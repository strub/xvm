(* -------------------------------------------------------------------- *)
open Location

(* -------------------------------------------------------------------- *)
type pop = [
  | `EQ  | `LT  | `LE
  | `ADD | `SUB | `MUL | `DIV | `AND | `OR | `NOT
]

type pinstr_r =
  | P_OP    of pop
  | P_PUSH  of int32
  | P_READ
  | P_WRITE
  | P_RFR   of int32
  | P_WFR   of int32
  | P_FETCH of int32
  | P_POP
  | P_GTO   of paddr loced option
  | P_GTZ   of paddr loced
  | P_GSB   of paddr loced option
  | P_RET
  | P_PXR
  | P_PRX
  | P_ALLOC
  | P_CREAD
  | P_CWRITE
  | P_STOP
  | P_PRT

and pentry =
  | P_Empty
  | P_Instr of pinstr
  | P_Label of plabel

and paddr =
  | P_ALit of int32
  | P_ALbl of string

and plabel   = string loced
and pinstr   = pinstr_r loced
and pprogram = pentry list

(* -------------------------------------------------------------------- *)
type popcode = [
  | `OC_ADD
  | `OC_SUB
  | `OC_MUL
  | `OC_DIV
  | `OC_AND
  | `OC_OR
  | `OC_NOT
  | `OC_EQ
  | `OC_LT
  | `OC_LE
  | `OC_PUSH
  | `OC_READ
  | `OC_WRITE
  | `OC_POP
  | `OC_RET
  | `OC_PXR
  | `OC_PRX
  | `OC_CREAD
  | `OC_CWRITE
  | `OC_ALLOC
  | `OC_FETCH
  | `OC_GTO
  | `OC_GTZ
  | `OC_GSB
  | `OC_STOP
  | `OC_PRT
]

(* -------------------------------------------------------------------- *)
type perror =
  | PE_LexicalError of string
  | PE_UnknownOC    of string
  | PE_InvalidOCArg of popcode
  | PE_Unknown
  | PE_InvalidLit   of [`Word | `Offset]

exception ParseError of Location.t option * perror

(* -------------------------------------------------------------------- *)
let string_of_popcode (x : popcode) =
  match x with
  | `OC_ADD    -> "ADD"
  | `OC_SUB    -> "SUB"
  | `OC_MUL    -> "MUL"
  | `OC_DIV    -> "DIV"
  | `OC_AND    -> "AND"
  | `OC_OR     -> "OR "
  | `OC_NOT    -> "NOT"
  | `OC_EQ     -> "EQ"
  | `OC_LT     -> "LT"
  | `OC_LE     -> "LE"
  | `OC_PUSH   -> "PUSH"
  | `OC_READ   -> "READ"
  | `OC_WRITE  -> "WRITE"
  | `OC_POP    -> "POP"
  | `OC_RET    -> "RET"
  | `OC_PXR    -> "PXR"
  | `OC_PRX    -> "PRX"
  | `OC_CREAD  -> "CREAD"
  | `OC_CWRITE -> "CWRITE"
  | `OC_ALLOC  -> "ALLOC"
  | `OC_FETCH  -> "FETCH"
  | `OC_GTO    -> "GTO"
  | `OC_GTZ    -> "GTZ"
  | `OC_GSB    -> "GSB"
  | `OC_STOP   -> "STOP"
  | `OC_PRT    -> "PRT"

(* -------------------------------------------------------------------- *)
type karg = [`Int | `Addr]

let string_of_karg (ka : karg) =
  match ka with
  | `Int  -> "int"
  | `Addr -> "address"

(* -------------------------------------------------------------------- *)
let kargs_of_popcode (x : popcode) : karg list =
  match x with
  | `OC_ADD    -> []
  | `OC_SUB    -> []
  | `OC_MUL    -> []
  | `OC_DIV    -> []
  | `OC_AND    -> []
  | `OC_OR     -> []
  | `OC_NOT    -> []
  | `OC_EQ     -> []
  | `OC_LT     -> []
  | `OC_LE     -> []
  | `OC_PUSH   -> [`Int]
  | `OC_READ   -> []
  | `OC_WRITE  -> []
  | `OC_POP    -> []
  | `OC_RET    -> []
  | `OC_PXR    -> []
  | `OC_PRX    -> []
  | `OC_CREAD  -> []
  | `OC_CWRITE -> []
  | `OC_ALLOC  -> []
  | `OC_FETCH  -> [`Int]
  | `OC_GTO    -> [`Addr]
  | `OC_GTZ    -> [`Addr]
  | `OC_GSB    -> [`Addr]
  | `OC_STOP   -> []
  | `OC_PRT    -> []

(* -------------------------------------------------------------------- *)
let string_of_perror = function
  | PE_LexicalError x ->
      Printf.sprintf "lexical error: %s" x
  | PE_UnknownOC x ->
      Printf.sprintf "unknown op-code: `%s'" x
  | PE_InvalidOCArg x ->
      let kargs = kargs_of_popcode x in
      Printf.sprintf
        "invalid arguments for `%s' (argument(s) expected: [%s])"
        (string_of_popcode x)
        (String.concat "; " (List.map string_of_karg kargs))
  | PE_Unknown ->
      Printf.sprintf "invalid syntax"
  | PE_InvalidLit _ ->
      Printf.sprintf "invalid literal"
