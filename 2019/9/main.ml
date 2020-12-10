open Core
open Async
open Deferred.Let_syntax

let input =
  In_channel.input_all In_channel.stdin
  |> String.strip
  |> String.split_on_chars ~on:[ ',' ]
  |> List.map ~f:Int.of_string |> Array.of_list

let main () =
  let cpu = Int_code.init input 0 in
  don't_wait_for @@ Int_code.run cpu;
  don't_wait_for @@ Int_code.input cpu 1;
  let%bind () =
    Pipe.iter cpu.output_reader ~f:(fun x ->
        return @@ Print.printf "part 1 BOOST keycode: %d\n" x)
  in

  let cpu = Int_code.init input 0 in
  don't_wait_for @@ Int_code.run cpu;
  don't_wait_for @@ Int_code.input cpu 2;
  let%bind () =
    Pipe.iter cpu.output_reader ~f:(fun x ->
        return @@ Print.printf "part 2 coordinates: %d\n" x)
  in
  exit 0

let () =
  don't_wait_for @@ main ();
  never_returns (Scheduler.go ())
