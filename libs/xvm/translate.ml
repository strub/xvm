(* -------------------------------------------------------------------- *)
open Utils
open Location
open Syntax
open Engine

(* -------------------------------------------------------------------- *)
let pinstr_of_pentry = function P_Instr i -> Some i | _ -> None
let plabel_of_pentry = function P_Label l -> Some l | _ -> None

(* -------------------------------------------------------------------- *)
type translate_error =
  | InvalidLabel   of string
  | DuplicateLabel of string

exception TranslateError of Location.t * translate_error

let terror ~loc error =
  raise (TranslateError (loc, error))

(* -------------------------------------------------------------------- *)
let string_of_translate_error (e : translate_error) =
  match e with
  | InvalidLabel l ->
      Printf.sprintf "invalid label: `%s'" l
  | DuplicateLabel l ->
      Printf.sprintf "duplicate label: `%s'" l

(* -------------------------------------------------------------------- *)
type labels = (string, int32) Map.t

(* -------------------------------------------------------------------- *)
let translate_addr (m : labels) (a : paddr loced) =
  match unloc a with
  | P_ALit i -> i
  | P_ALbl l -> begin
      match Map.Exceptionless.find l m with
      | Some i -> i
      | None   -> terror ~loc:(loc a) (InvalidLabel l)
    end

(* -------------------------------------------------------------------- *)
let translate_instr (m : labels) (i : pinstr) =
  match unloc i with
  | P_OP    x -> OP    x
  | P_PUSH  x -> PUSH  x
  | P_READ    -> READ
  | P_WRITE   -> WRITE
  | P_FETCH x -> FETCH x
  | P_RFR   x -> RFR   x
  | P_WFR   x -> WFR   x
  | P_POP     -> POP
  | P_GTO   x -> GTO   (Option.map (translate_addr m) x)
  | P_GTZ   x -> GTZ   (translate_addr m x)
  | P_GSB   x -> GSB   (Option.map (translate_addr m) x)
  | P_RET     -> RET
  | P_PXR     -> PXR
  | P_PRX     -> PRX
  | P_ALLOC   -> ALLOC
  | P_CREAD   -> CREAD
  | P_CWRITE  -> CWRITE
  | P_STOP    -> STOP
  | P_PRT     -> PRT

(* -------------------------------------------------------------------- *)
let translate_labels (p : pprogram) =
  let (_, acc, lbls) =
    List.fold_lefti (fun (pc, acc, lbls) no e ->
      match e with
      | P_Empty   -> (pc, acc, lbls)
      | P_Instr i -> (Int32.succ pc, (no, i) :: acc, lbls)
      | P_Label l ->
          if Map.mem (unloc l) lbls then
            terror ~loc:(loc l) (DuplicateLabel (unloc l));
          (pc, acc, Map.add (unloc l) pc lbls))
      (0l, [], Map.empty) p

  in (lbls, List.rev acc)

(* -------------------------------------------------------------------- *)
let translate_program (p : pprogram) =
  let lbls, p = translate_labels p in
  List.map (snd_map (translate_instr lbls)) p
