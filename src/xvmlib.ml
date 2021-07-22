(* -------------------------------------------------------------------- *)
open Xvm.Utils
open Js_of_ocaml

(* -------------------------------------------------------------------- *)
module IMap = Map.Make(struct
  type t = int
  let  compare = (Pervasives.compare : t -> t -> int)
end)

(* -------------------------------------------------------------------- *)
type state = {
  xvm_code   : Xvm.Engine.aprog;
  xvm_revlno : int IMap.t;
  xvm_engine : Xvm.Engine.state;
}

(* -------------------------------------------------------------------- *)
module API : sig
  val create : (int32 -> unit) -> string -> state
  val revidx : int -> state -> int option
  val step   : state -> unit
  val run    : ?until:int -> state -> unit
  val dump   : state -> string
  val srcpc  : state -> int option
end = struct
  let create (printer : int32 -> unit) (input : string) =
    let code = Xvm.Io.parse_program (IO.input_string input) in
    let code = Xvm.Translate.translate_program code in
    let engn = Xvm.Engine.State.init ~out:printer () in
    let rev  =
      List.fold_lefti (fun m i (j, _) -> IMap.add j i m) IMap.empty code
    in

    { xvm_code   = Array.of_list code;
      xvm_revlno = rev;
      xvm_engine = engn; }

  let decoder_r (state : state) = fun i ->
    let open Int32.Compare in

    let length = Array.length state.xvm_code in
    if   i < 0l || i >= Int32.of_int length
    then None
    else Some (state.xvm_code.(Int32.to_int i))

  let srcpc (state : state) =
    Option.map fst (decoder_r state state.xvm_engine.pc)

  let revidx (i : int) (state : state) =
    try  Some (IMap.find i state.xvm_revlno)
    with Not_found -> None

  let decoder (state : state) = fun i ->
    Option.map snd (decoder_r state i)

  let step (state : state) =
    try
      Xvm.Engine.eval1 state.xvm_engine (decoder state)
    with Xvm.Engine.Stop -> ()

  let run ?(until : int option) (state : state) =
    Xvm.Engine.run ?until state.xvm_engine (decoder state)

  let dump (state : state) =
    let k   = Buffer.create 128 in
    let fmt = Format.formatter_of_buffer k in
    Format.fprintf fmt "%a@!"
      Xvm.Engine.State.dump state.xvm_engine;
    Buffer.contents k
end

(* -------------------------------------------------------------------- *)
let _maybe_error (f : unit -> 'a) : 'a =
  try
    f ()
  with _ as e ->
    let msg =
      match e with
      | Xvm.Syntax.ParseError (loc, code) ->
          Format.sprintf "%s: parse error: %s\n"
             (Xvm.Location.tostring (Option.default Xvm.Location._dummy loc))
             (Xvm.Syntax.string_of_perror code)
      | Xvm.Translate.TranslateError (loc, code) ->
          Format.sprintf "%s: translate error: %s\n"
             (Xvm.Location.tostring loc)
             (Xvm.Translate.string_of_translate_error code)
      | Xvm.Engine.EngineError code ->
          Format.sprintf "engine error: %s\n%!"
             (Xvm.Engine.string_of_engine_error code)
      | _ -> "unexpected error"

    in Js.raise_js_error (new%js Js.error_constr (Js.bytestring msg))

(* -------------------------------------------------------------------- *)
let js_object_of_state (state : state) = (object%js
  val state = state

  method step =
    _maybe_error (fun () -> API.step state)

  method run (until : int) =
    _maybe_error (fun () -> API.run ~until state)

  method revidx (i : int) =
    _maybe_error (fun () ->
        match API.revidx i state with
        | None -> -1 | Some j -> j
    )

  method dump =
    _maybe_error (fun () -> Js.bytestring (API.dump state))

  method pc =
    Option.default (-1) (API.srcpc state)
end)

(* -------------------------------------------------------------------- *)
let _ =
   Js.export "xvm" (object%js
     method create (cb : Js.Unsafe.any) (input : Js.js_string Js.t) =
       let real_cb (i : int32) : unit =
         Js.Unsafe.fun_call cb [| Js.Unsafe.inject i |]
       in

       _maybe_error (fun () ->
         let input = Js.to_bytestring input in
         let state = API.create real_cb input in
         js_object_of_state state)
   end)
