open Core
open Async

let input =
  In_channel.input_all In_channel.stdin
  |> String.strip
  |> String.split_on_chars ~on:[ ',' ]
  |> List.map ~f:Int.of_string

let main () =
  let open Deferred.Let_syntax in
  Print.printf "Part 1:\n";
  let state = Int_code.init (Core.Array.of_list input) 0 in
  let dfd = Int_code.run state in
  let%bind () = Int_code.input state 1 in
  let%bind output = Pipe.read_all state.output_reader >>| Core.Queue.to_list in
  Print.printf "output: %s\n\n"
    (List.map output ~f:(Printf.sprintf "%d") |> String.concat ~sep:" ");
  let%bind () = dfd in

  Print.printf "Part 2:\n";
  let state = Int_code.init (Core.Array.of_list input) 0 in
  let dfd = Int_code.run state in
  let%bind () = Int_code.input state 5 in
  let%bind () =
    match%map Pipe.read state.output_reader with
    | `Ok diagnostic_code ->
        Print.printf "diagnostic code: %d\n" diagnostic_code
    | `Eof -> Print.printf "no output :-(\n"
  in
  let%bind () = dfd in
  exit 0

let () =
  let _ = main () in
  never_returns (Scheduler.go ())
