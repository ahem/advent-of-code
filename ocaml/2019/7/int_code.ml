open Core
open Async
open Async.Print

let decode_arg ~mode mem n =
  match mode with
  | 0 -> mem.(mem.(n))
  | 1 -> mem.(n)
  | _ -> failwith @@ sprintf "invalid mode %d at addr %d" mode n

let decode_args3 mem addr =
  ( decode_arg ~mode:(mem.(addr) / 100 mod 10) mem (addr + 1),
    decode_arg ~mode:(mem.(addr) / 1000 mod 10) mem (addr + 2),
    decode_arg ~mode:(mem.(addr) / 10000 mod 10) mem (addr + 3) )

let decode_arg2 mem addr =
  ( decode_arg ~mode:(mem.(addr) / 100 mod 10) mem (addr + 1),
    decode_arg ~mode:(mem.(addr) / 1000 mod 10) mem (addr + 2) )

let decode_arg1 mem addr =
  decode_arg ~mode:(mem.(addr) / 100 mod 10) mem (addr + 1)

type t = {
  memory : int array;
  pc : int;
  input_reader : int Pipe.Reader.t;
  input_writer : int Pipe.Writer.t;
  output_reader : int Pipe.Reader.t;
  output_writer : int Pipe.Writer.t;
}

let init : ?size_budget:int -> int array -> int -> t =
 fun ?(size_budget = 1) memory pc ->
  let input_reader, input_writer = Pipe.create () in
  let output_reader, output_writer = Pipe.create () in
  Pipe.set_size_budget input_writer size_budget;
  { memory; pc; input_reader; input_writer; output_reader; output_writer }

let input : t -> int -> unit Deferred.t =
 fun { input_writer; _ } x -> Pipe.write input_writer x

let rec run : t -> unit Deferred.t =
 fun state ->
  let { memory; pc; _ } = state in
  match memory.(pc) % 100 with
  | 1 ->
      (* add *)
      let src1, src2 = decode_arg2 memory pc and dst = memory.(pc + 3) in
      memory.(dst) <- src1 + src2;
      run { state with pc = pc + 4 }
  | 2 ->
      (* mul *)
      let src1, src2 = decode_arg2 memory pc and dst = memory.(pc + 3) in
      memory.(dst) <- src1 * src2;
      run { state with pc = pc + 4 }
  | 3 -> (
      let (* input *)
      open Deferred.Let_syntax in
      let dst = memory.(pc + 1) in
      match%bind Pipe.read state.input_reader with
      | `Ok x ->
          memory.(dst) <- x;
          run { state with pc = pc + 2 }
      | `Eof -> failwith "input pipe unexpectedly closed!" )
  | 4 ->
      (* output *)
      let open Deferred.Let_syntax in
      let x = decode_arg1 memory pc in
      let%bind () = Pipe.write state.output_writer x in
      run { state with pc = pc + 2 }
  | 5 -> (
      (* jump-if-true *)
      match decode_arg2 memory pc with
      | 0, _ -> run { state with pc = pc + 3 }
      | _, target -> run { state with pc = target } )
  | 6 -> (
      (* jump-if-false *)
      match decode_arg2 memory pc with
      | 0, target -> run { state with pc = target }
      | _ -> run { state with pc = pc + 3 } )
  | 7 ->
      (* less than *)
      let a, b = decode_arg2 memory pc and dst = memory.(pc + 3) in
      memory.(dst) <- (if a < b then 1 else 0);
      run { state with pc = pc + 4 }
  | 8 ->
      (* equals *)
      let a, b = decode_arg2 memory pc and dst = memory.(pc + 3) in
      memory.(dst) <- (if a = b then 1 else 0);
      run { state with pc = pc + 4 }
  | 99 ->
      Pipe.close_read state.input_reader;
      Pipe.close state.output_writer;
      Deferred.unit
  | _ ->
      printf "invalid opcode %d" memory.(pc);
      Async.Deferred.unit
