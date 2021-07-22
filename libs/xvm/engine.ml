(* -------------------------------------------------------------------- *)
open Utils

(* -------------------------------------------------------------------- *)
type engine_error =
  | Word_overflow
  | Int_mismatch
  | Ptr_mismatch
  | Stack_underflow
  | Stack_overflow
  | InvalidMemAccess
  | Invalid_PC
  | DivisionByZero

exception EngineError of engine_error

let eerror code =
  raise (EngineError code)

(* -------------------------------------------------------------------- *)
let string_of_engine_error = function
  | Word_overflow    -> "word overflow"
  | Int_mismatch     -> "int mismatch"
  | Ptr_mismatch     -> "ptr mismatch"
  | Stack_underflow  -> "stack underflow"
  | Stack_overflow   -> "stack overflow"
  | InvalidMemAccess -> "invalid memory access"
  | Invalid_PC       -> "invalid PC"
  | DivisionByZero   -> "division by zero"

(* -------------------------------------------------------------------- *)
type op = Syntax.pop

type opsem =
  | UniOp of (int32 -> int32)
  | BinOp of (int32 -> int32 -> int32)

(* -------------------------------------------------------------------- *)
module Op : sig
  val to_string : op -> string
  val eval : op -> opsem
end = struct
  let to_string = function
    | `ADD -> "ADD"
    | `SUB -> "SUB"
    | `MUL -> "MULT"
    | `DIV -> "DIV"
    | `AND -> "AND"
    | `OR  -> "OR"
    | `NOT -> "NOT"
    | `EQ  -> "EQ"
    | `LT  -> "LT"
    | `LE  -> "LE"

  let pint32div x y =
    if   Int32.equal y 0l
    then eerror DivisionByZero
    else Int32.div x y

  let b2w (b : bool) =
    if b then 1l else 0l

  let eval = function
    | `ADD -> BinOp Int32.add
    | `SUB -> BinOp Int32.sub
    | `MUL -> BinOp Int32.mul
    | `DIV -> BinOp pint32div
    | `AND -> BinOp (fun x y -> b2w ((x <> 0l) && (y <> 0l)))
    | `OR  -> BinOp (fun x y -> b2w ((x <> 0l) || (y <> 0l)))
    | `NOT -> UniOp (fun x   -> b2w (x = 0l))
    | `EQ  -> BinOp (fun x y -> b2w (Int32.equal x y))
    | `LT  -> BinOp (fun x y -> b2w (Int32.compare x y < 0))
    | `LE  -> BinOp (fun x y -> b2w (Int32.compare x y <= 0))
end

(* -------------------------------------------------------------------- *)
(* In the folowing comments:
 * - X designs the element on the top of the stack, and
 * - Y the element below, etc *)

type instr =
  | OP of op
  | PUSH of int32         (* literal to stack *)
  | READ                  (* read MEM(X) *)
  | WRITE                 (* push X to MEM(Y) *) 
  | FETCH of int32        (* copies i-th element of stack on top *)
  | POP
  | GTO of int32 option
  | GTZ of int32
  | GSB of int32 option
  | RET
  | PXR                  (* copy X to R *)
  | PRX                  (* copy R to X *)
  | ALLOC                (* allocates X words and returns adress *)
  | CREAD                (* reads address Y with offset X in heap *)
  | CWRITE               (* writes Z at address Y with offset X in heap *)
  | WFR of int32         (* copy X to FP+i-th position on the stack *)
  | RFR of int32         (* copy the FP+i-th element on top of the stack *)
  | STOP
  | PRT
      
(* -------------------------------------------------------------------- *)
module Instr : sig
  val to_string : instr -> string
end = struct
  let to_string =
    let warg = fun x d -> Printf.sprintf "%s %ld" x d in

    let warg_opt = fun x d ->
      match d with Some d -> warg x d | None -> x in

    function
    | PUSH i  -> warg "PUSH" i
    | READ    -> "READ"
    | WRITE   -> "WRITE"
    | FETCH i -> warg "FETCH" i
    | POP     -> "POP"
    | GTO i   -> warg_opt "GTO" i
    | GTZ i   -> warg "GTZ" i
    | GSB i   -> warg_opt "GSB" i
    | RET     -> "RET"
    | PXR     -> "PXR"
    | PRX     -> "PRX"
    | ALLOC   -> "ALLOC"
    | CREAD   -> "CREAD"
    | CWRITE  -> "CWRITE"
    | WFR i   -> warg "WFR" i
    | RFR i   -> warg "RFR" i
    | STOP    -> "STOP"
    | OP o    -> Op.to_string o
    | PRT     -> "PRT"
end

type prog  = instr array
type aprog = (int * instr) array

(* -------------------------------------------------------------------- *)
type fetch = int32 -> instr option

(* -------------------------------------------------------------------- *)
type word = INT of int32 | PTR of ptr
and  ptr  = int32 * word array

let null = PTR (0l, [| |])

(* -------------------------------------------------------------------- *)
module Word : sig
  val to_string : ?expand:bool -> word -> string

  val as_int : word -> int32
  val as_ptr : word -> ptr
end = struct
  let rec to_string ?(expand = true) = function
    | INT i ->
        Format.sprintf "INT32 %ld" i

    | PTR (id, _) when not expand ->
        Format.sprintf "PTR %#0lx" id

    | PTR (id, p) ->
        let len = min (Array.length p) 5 in
        Format.sprintf "PTR %#0lx [|%s%s|]" id
          (String.concat "; "
             (List.map (to_string ~expand:false)
             (Array.to_list (Array.sub p 0 len))))
          (if len < Array.length p then "; ..." else "") 

  let as_int = function INT i -> i | _ -> eerror Int_mismatch
  let as_ptr = function PTR p -> p | _ -> eerror Ptr_mismatch
end

(* -------------------------------------------------------------------- *)
type state = {
  mutable sp  : int32;                  (* stack pointer   *)
  mutable pc  : int32;                  (* program counter *)
  mutable fp  : int32;                  (* frame pointer   *)
  mutable r   : word;                   (* single register *)
  mutable sck : word  array;            (* stack           *)
  mutable mem : int32 array;            (* static memory   *)
  mutable tck : int;                    (* ticks           *)
  (*---*) out : (int32 -> unit) option; (* printer         *)
}

module State : sig
  val init  : ?out:(int32 -> unit) -> unit -> state
  val dump  : Format.formatter -> state -> unit
end = struct
  let init ?out () = {
    sp  = 0l;
    pc  = 0l;
    fp  = 0l;
    r   = (INT 0l);
    sck = Array.make 128 (INT 0l);
    mem = Array.make 128 Int32.zero;
    tck = 0;
    out = out;
  }

  let dump (fmt : Format.formatter) (m : state) =
    Gc.compact ();              (* try to have stable addresses... *)

    Format.fprintf fmt "TK = %04d%!\n" m.tck;
    Format.fprintf fmt "PC = %#04lx\n%!" m.pc;
    Format.fprintf fmt "SP = %#04lx\n%!" m.sp;
    Format.fprintf fmt "FP = %#04lx\n%!" m.fp;
    Format.fprintf fmt "R  = %s\n%!" (Word.to_string m.r);
    Format.fprintf fmt "\n%!";
    for i = Int32.to_int m.sp - 1 downto 0 do
      Format.fprintf fmt "%.3d: %s\n%!" i (Word.to_string (m.sck.(i)))
    done;
    Format.fprintf fmt "\n%!";

    let smem =
      let smem = IO.output_string () in
      for i = 0 to Array.length m.mem - 1 do
         IO.write_real_i32 smem m.mem.(i)
      done;
      IO.close_out smem

    in Format.fprintf fmt "%s%!" (Hex.hexdump_s (Hex.of_string smem))
end

(* -------------------------------------------------------------------- *)
module Stack : sig
  type mode = [ `SP | `FP | `Raw ]

  val size : state -> int
  val idx  : ?mode:mode -> state -> int32 -> int
  val get  : ?mode:mode -> state -> int32 -> word
  val put  : ?mode:mode -> state -> int32 -> word -> unit
  val push : state -> word -> unit
  val pop  : state -> word
  val upop : state -> unit
  val top  : state -> word

  val iget  : ?mode:mode -> state -> int32 -> int32
  val iput  : ?mode:mode -> state -> int32 -> int32 -> unit
  val ipush : state -> int32 -> unit
  val ipop  : state -> int32
  val itop  : state -> int32

  val pget  : ?mode:mode -> state -> int32 -> ptr
  val pput  : ?mode:mode -> state -> int32 -> ptr -> unit
  val ppush : state -> ptr -> unit
  val ppop  : state -> ptr
  val ptop  : state -> ptr
end = struct
  type mode = [ `SP | `FP | `Raw ]

  let size (m : state) =
    Array.length m.sck

  let idx ?(mode : mode = `Raw) (m : state) (i : int32) =
    let open Int32.Infix in
    let open Int32.Compare in

    let i =
      match mode with
      | `Raw -> i
      | `SP  -> m.sp - i - 1l
      | `FP  -> m.fp + i
    in

    if i >= m.sp then eerror Stack_overflow;
    if i <  0l   then eerror Stack_underflow;

    Int32.to_int i

  let get ?(mode = `Raw) (m : state) (i : int32) : word =
    m.sck.(idx ~mode m i)

  let put ?(mode = `Raw) (m : state) (i : int32) (w : word) =
    m.sck.(idx ~mode m i) <- w

  let push (m : state) (w : word) =
    m.sp <- Int32.succ m.sp; put ~mode:`SP m 0l w

  let pop (m : state) =
    let x = get ~mode:`SP m 0l in m.sp <- Int32.pred m.sp; x

  let upop (m : state) =
    let (_ : word) = pop m in ()

  let top (m : state) =
    get m 0l

  let iget  = fun ?mode m i -> Word.as_int (get ?mode m i)
  let iput  = fun ?mode m i n -> put ?mode m i (INT n)
  let ipush = fun m i -> push m (INT i)
  let ipop  = fun m -> Word.as_int (pop m)
  let itop  = fun m -> Word.as_int (top m)

  let pget  = fun ?mode m i -> Word.as_ptr (get ?mode m i)
  let pput  = fun ?mode m i n -> put ?mode m i (PTR n)
  let ppush = fun m i -> push m (PTR i)
  let ppop  = fun m -> Word.as_ptr (pop m)
  let ptop  = fun m -> Word.as_ptr (top m)
end

(* -------------------------------------------------------------------- *)
module Mem : sig
  val get : state -> int32 -> int32
  val set : state -> addr:int32 -> value:int32 -> unit
end = struct
  let get (m : state) (i : int32) =
    try  m.mem.(Int32.to_int i)
    with Invalid_argument _ -> eerror InvalidMemAccess
    
  let set (m : state) ~addr ~value =
    try  m.mem.(Int32.to_int addr) <- value
    with Invalid_argument _ -> eerror InvalidMemAccess
end

(* -------------------------------------------------------------------- *)
module Ptr : sig
  val alloc : int32 -> ptr
  val get   : ptr -> int32 -> word
  val set   : ptr -> int32 -> word -> unit
end = struct
  let counter = ref 0l

  let alloc (n : int32) =
    counter := Int32.add !counter 1l;
    (!counter, Array.make (Int32.to_int n) (INT 0l))

  let get ((_, p) : ptr) (i : int32) =
    let i = Int32.to_int i in
    if i < 0 || i >= Array.length p then
      eerror InvalidMemAccess;
    p.(i)

  let set ((_, p) : ptr) (i : int32) (w : word) =
    let i = Int32.to_int i in
    if i < 0 || i >= Array.length p then
      eerror InvalidMemAccess;
    p.(i) <- w
end

(* -------------------------------------------------------------------- *)
exception Stop

let eval1 (m : state) (p : fetch) =
  let module S = Stack in

  let incr32 (x : int32) = Int32.add x 1l in

  let _intx () = S.iget ~mode:`SP m 0l in
  let _inty () = S.iget ~mode:`SP m 1l in
  let _ptx  () = S.pget ~mode:`SP m 0l in
  let _pty  () = S.pget ~mode:`SP m 1l in

  let _mget = Mem.get m in
  let _mset = Mem.set m in

  let i =
    match p m.pc with
    | None   -> eerror Invalid_PC
    | Some i -> if i <> STOP then m.pc <- incr32 m.pc; i
  in

  m.tck <- m.tck + 1;

  match i with
  | OP o -> begin
      match Op.eval o with
      | UniOp f -> S.ipush m (f (Stack.ipop m))
      | BinOp f ->
          let x = S.ipop m in
          let y = S.ipop m in
          S.ipush m (f y x)
    end

  | PUSH n ->
      S.ipush m n

  | FETCH i ->
      S.push m (S.get ~mode:`SP m i)

  | POP ->
      S.upop m

  | GTO None ->
      m.pc <- S.ipop m

  | GTO (Some n) ->
      m.pc <- n

  | GTZ n ->
      if S.ipop m = 0l then m.pc <- n

  | GSB n ->
      let npc = Option.default_delayed (fun () -> S.ipop m) n in
      let osp = m.sp in
      S.ipush m m.fp; S.ipush m m.pc; m.pc <- npc; m.fp <- osp

  | RET ->
      m.pc <- S.ipop m; m.fp <- S.ipop m

  | PXR ->
      m.r <- S.pop m

  | PRX ->
      S.push m m.r

  | ALLOC ->
      let size = S.ipop m in
      S.ppush m (Ptr.alloc size)

  | READ ->
      let base = S.ipop m in
      S.ipush m (_mget base)

  | WRITE ->
      let base = S.ipop m in
      let val_ = S.ipop m in
      _mset ~addr:base ~value:val_

  | CREAD ->
      let offt = S.ipop m in
      let base = S.ppop m in
      S.push m (Ptr.get base offt)

  | CWRITE ->
      let offt = S.ipop m in
      let base = S.ppop m in
      let val_ = S.pop  m in
      Ptr.set base offt val_

  | RFR i ->
      S.push m (S.get ~mode:`FP m i)

  | WFR i ->
      let val_ = S.pop m in
      S.put ~mode:`FP m i val_

  | STOP ->
      raise Stop

  | PRT ->
      let e = S.ipop m in
      Option.may (fun f -> f e) m.out

(* -------------------------------------------------------------------- *)
let run ?fmt ?until (m : state) ?(a : int32 list = []) (p : fetch)  =
  let debug () = Option.may (fun fmt ->
    Format.fprintf fmt "%s\n%!" (String.make 72 '-');
    State.dump fmt m) fmt in

  List.iter (Stack.ipush m) a;
  try
    debug ();
    Enum.iter
      (fun _ -> eval1 m p; debug ())
      (Enum.range ?until:(Option.map (fun x -> x - 1) until) 0)

  with Stop -> ()
