open Core
open Async
open Deferred.Let_syntax

type t = int Pipe.Writer.t * int Pipe.Reader.t

let init : int array -> int list -> t Deferred.t =
 fun program phases ->
  let make_step program phase =
    let state = Int_code.init (Array.copy program) 0 in
    let%bind () = Int_code.input state phase in
    don't_wait_for @@ Int_code.run state;
    return state
  in

  let connect a b =
    let open Int_code in
    don't_wait_for @@ Pipe.transfer a.output_reader b.input_writer ~f:ident;
    b
  in

  let%bind amps = Deferred.List.map phases ~f:(make_step program) in
  let first = List.hd_exn amps in
  let last = List.fold ~init:first (List.tl_exn amps) ~f:connect in
  return (first.input_writer, last.output_reader)

let run : t -> int Deferred.t =
 fun (input_writer, output_reader) ->
  let%bind () = Pipe.write input_writer 0 in
  match%map Pipe.read output_reader with
  | `Ok x -> x
  | `Eof -> failwith "unexpectedly closed output pipe"

let feedback : t -> int Deferred.t =
 fun (input_writer, output_reader) ->
  let%bind () = Pipe.write input_writer 0 in
  Pipe.fold output_reader ~init:0 ~f:(fun _ x ->
      don't_wait_for @@ Pipe.write_if_open input_writer x;
      return x)
