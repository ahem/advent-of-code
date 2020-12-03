open Core

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:Int.of_string

let combinations list =
  Sequence.cartesian_product (Sequence.of_list list) (Sequence.of_list list)

let combinations3 list =
  Sequence.cartesian_product (combinations list) (Sequence.of_list list)
  |> Sequence.map ~f:(fun ((x, y), z) -> (x, y, z))

let () =
  let a, b =
    Sequence.find_exn (combinations input) ~f:(fun (a, b) -> a + b = 2020)
  in
  Printf.printf "Part 1: %d (%d, %d)\n" (a * b) a b;

  let a, b, c =
    Sequence.find_exn (combinations3 input) ~f:(fun (a, b, c) ->
        a + b + c = 2020)
  in
  Printf.printf "Part 2: %d (%d, %d, %d)\n" (a * b * c) a b c
