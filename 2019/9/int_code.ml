open Core
open Async
open Async.Print
open Deferred.Let_syntax

type t = {
  memory : int array;
  pc : int;
  base : int;
  input_reader : int Pipe.Reader.t;
  input_writer : int Pipe.Writer.t;
  output_reader : int Pipe.Reader.t;
  output_writer : int Pipe.Writer.t;
}

(* let src1, src2, dst = decode3 memory pc `Src `Src `Dst in *)
let read_src_arg ~mode ~base memory addr =
  let value = memory.(addr) in
  match mode with
  | 0 -> memory.(value)
  | 1 -> value
  | 2 -> memory.(value + base)
  | _ -> failwith @@ sprintf "invalid mode %d at addr %d" mode addr

let read_dst_arg ~mode ~base memory addr =
  let value = memory.(addr) in
  match mode with
  | 0 -> value
  | 2 -> value + base
  | _ -> failwith @@ sprintf "invalid mode %d at addr %d" mode addr

let decode_src : t -> int =
 fun state ->
  let opcode = state.memory.(state.pc) in
  let base = state.base in
  read_src_arg ~base ~mode:(opcode / 100 mod 10) state.memory (state.pc + 1)

let decode_dst : t -> int =
 fun state ->
  let opcode = state.memory.(state.pc) in
  let base = state.base in
  read_dst_arg ~base ~mode:(opcode / 100 mod 10) state.memory (state.pc + 1)

let decode_src_src : t -> int * int =
 fun state ->
  let opcode = state.memory.(state.pc) in
  let base = state.base in
  ( read_src_arg ~base ~mode:(opcode / 100 mod 10) state.memory (state.pc + 1),
    read_src_arg ~base ~mode:(opcode / 1000 mod 10) state.memory (state.pc + 2)
  )

let decode_src_dst : t -> int * int =
 fun state ->
  let opcode = state.memory.(state.pc) in
  let base = state.base in
  ( read_src_arg ~base ~mode:(opcode / 100 mod 10) state.memory (state.pc + 1),
    read_dst_arg ~base ~mode:(opcode / 1000 mod 10) state.memory (state.pc + 2)
  )

let decode_src_src_dst : t -> int * int * int =
 fun state ->
  let opcode = state.memory.(state.pc) in
  let base = state.base in
  ( read_src_arg ~base ~mode:(opcode / 100 mod 10) state.memory (state.pc + 1),
    read_src_arg ~base ~mode:(opcode / 1000 mod 10) state.memory (state.pc + 2),
    read_dst_arg ~base ~mode:(opcode / 10000 mod 10) state.memory (state.pc + 3)
  )

let init : ?size_budget:int -> int array -> int -> t =
 fun ?(size_budget = 1) program pc ->
  let input_reader, input_writer = Pipe.create () in
  let output_reader, output_writer = Pipe.create () in
  Pipe.set_size_budget input_writer size_budget;
  let memory = Array.create ~len:1_000_000 0 in
  Array.blit ~src:program ~dst:memory ~src_pos:0 ~dst_pos:0
    ~len:(Array.length program);
  {
    memory;
    pc;
    base = 0;
    input_reader;
    input_writer;
    output_reader;
    output_writer;
  }

let input : t -> int -> unit Deferred.t =
 fun { input_writer; _ } x -> Pipe.write input_writer x

let rec run : t -> unit Deferred.t =
 fun state ->
  let { memory; pc; _ } = state in
  match memory.(pc) % 100 with
  | 1 ->
      (* add *)
      let src1, src2, dst = decode_src_src_dst state in
      memory.(dst) <- src1 + src2;
      run { state with pc = pc + 4 }
  | 2 ->
      (* mul *)
      let src1, src2, dst = decode_src_src_dst state in
      memory.(dst) <- src1 * src2;
      run { state with pc = pc + 4 }
  | 3 -> (
      (* input *)
      let dst = decode_dst state in
      match%bind Pipe.read state.input_reader with
      | `Ok x ->
          memory.(dst) <- x;
          run { state with pc = pc + 2 }
      | `Eof -> failwith "input pipe unexpectedly closed!" )
  | 4 ->
      (* output *)
      let x = decode_src state in
      let%bind () = Pipe.write state.output_writer x in
      run { state with pc = pc + 2 }
  | 5 -> (
      (* jump-if-true *)
      match decode_src_src state with
      | 0, _ -> run { state with pc = pc + 3 }
      | _, target -> run { state with pc = target } )
  | 6 -> (
      (* jump-if-false *)
      match decode_src_src state with
      | 0, target -> run { state with pc = target }
      | _ -> run { state with pc = pc + 3 } )
  | 7 ->
      (* less than *)
      let a, b, dst = decode_src_src_dst state in
      memory.(dst) <- (if a < b then 1 else 0);
      run { state with pc = pc + 4 }
  | 8 ->
      (* equals *)
      let a, b, dst = decode_src_src_dst state in
      memory.(dst) <- (if a = b then 1 else 0);
      run { state with pc = pc + 4 }
  | 9 ->
      (* adjust relative base *)
      let base = state.base + decode_src state in
      run { state with base; pc = pc + 2 }
  | 99 ->
      Pipe.close_read state.input_reader;
      Pipe.close state.output_writer;
      Deferred.unit
  | _ ->
      printf "invalid opcode %d" memory.(pc);
      Deferred.unit
