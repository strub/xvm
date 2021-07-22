(* -------------------------------------------------------------------- *)
open Xvm.Utils
open Xvm.Engine

(* -------------------------------------------------------------------- *)
exception UsageError

(* -------------------------------------------------------------------- *)
let main () =
  let decoder (p : aprog) = fun i ->
    let open Int32.Compare in
    if i < 0l || i >= Int32.of_int (Array.length p) then
      None
    else Some (snd p.(Int32.to_int i)) in

  try
    if Array.length Sys.argv - 1 < 1 then raise UsageError;
    let f = Sys.argv.(1) in
    let a = Array.sub Sys.argv 2 (Array.length Sys.argv - 2) in
    let a =
      let tx x =
        try  Int32.of_string x
        with Failure _ -> raise UsageError
      in List.map tx (Array.to_list a) in

    let f = File.with_file_in f (Xvm.Io.parse_program ~name:f) in
    let f = Xvm.Translate.translate_program f in
    let s = State.init ~out:(fun i -> Printf.eprintf "%ld\n%!" i) () in
    run s ~a (decoder (Array.of_list f))

  with _ as e ->
    (match e with
     | UsageError ->
         Printf.eprintf "Usage: %s [filename] [args...]\n%!"
           Sys.executable_name
     | Xvm.Syntax.ParseError (loc, code) ->
         Printf.eprintf "%s: parse error: %s\n"
            (Xvm.Location.tostring (Option.default Xvm.Location._dummy loc))
            (Xvm.Syntax.string_of_perror code)
     | Xvm.Translate.TranslateError (loc, code) ->
         Printf.eprintf "%s: translate error: %s\n"
            (Xvm.Location.tostring loc)
            (Xvm.Translate.string_of_translate_error code)
     | Xvm.Engine.EngineError code ->
         Printf.eprintf "engine error: %s\n%!"
            (Xvm.Engine.string_of_engine_error code)
     | _ -> raise e); exit 1

(* -------------------------------------------------------------------- *)
let () = main ()
