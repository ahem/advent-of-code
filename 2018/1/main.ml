open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:Int.of_string

let first_duplicated_frequency : int Sequence.t -> int =
 fun l ->
  let acc = Set.empty (module Int) in
  let freq = 0 in
  Sequence.fold_until ~init:(acc, freq) l
    ~finish:(fun _ -> failwith "unreachable")
    ~f:(fun (acc, freq) x ->
      match Set.mem acc freq with
      | false ->
          let acc = Set.add acc freq in
          let freq = freq + x in
          Continue (acc, freq)
      | true -> Stop freq)

let () =
  printf "Part 1: %d\n" @@ List.fold ~init:0 ~f:(fun acc x -> acc + x) input;
  let seq = Sequence.cycle_list_exn input in
  printf "Part 2: %d\n" (first_duplicated_frequency seq)
