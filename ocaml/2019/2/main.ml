open Core

let input =
  In_channel.input_all In_channel.stdin
  |> String.strip
  |> String.split_on_chars ~on:[ ',' ]
  |> List.map ~f:Int.of_string

let run_program noun verb =
  let memory = Array.of_list input in
  memory.(1) <- noun;
  memory.(2) <- verb;
  Int_code.run memory 0;
  memory.(0)

let () =
  Printf.printf "part 1: %d\n" @@ run_program 12 02;

  let candidate_seq =
    Sequence.cartesian_product
      (Sequence.range ~start:`inclusive ~stop:`inclusive 0 99)
      (Sequence.range ~start:`inclusive ~stop:`inclusive 0 99)
  in

  Sequence.find_exn candidate_seq ~f:(fun (noun, verb) ->
      run_program noun verb = 19690720)
  |> fun (noun, verb) ->
  (100 * noun) + verb |> Printf.printf "part 2 result: %d\n"
