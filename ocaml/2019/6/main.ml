open Core

let input =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:(fun s ->
         match String.split s ~on:')' with
         | [ a; b ] -> (b, a)
         | _ -> failwith "invalid input")
  |> String.Map.of_alist_exn

let rec count_orbits ?(acc = 0) = function
  | "COM" -> acc
  | s -> count_orbits ~acc:(acc + 1) @@ String.Map.find_exn input s

let rec walk ?(acc = String.Map.empty) key =
  let acc = String.Map.add_exn acc ~key ~data:(String.Map.length acc) in
  match key with "COM" -> acc | s -> walk ~acc @@ String.Map.find_exn input s

let () =
  let total_orbits =
    Map.fold input ~init:0 ~f:(fun ~key ~data:_ acc -> acc + count_orbits key)
  in
  Printf.printf "part 1 result: %d\n" total_orbits;

  let santa_path = walk "SAN" in
  let you_path = walk "YOU" in

  let steps =
    String.Set.inter
      (String.Map.key_set santa_path)
      (String.Map.key_set you_path)
    |> String.Set.to_list
    |> List.map ~f:(fun k ->
           String.Map.find_exn santa_path k + String.Map.find_exn you_path k)
    |> List.min_elt ~compare:Int.compare
  in
  let steps_without_start_positions = Option.value_exn steps - 2 in
  Printf.printf "part 2 result: %d" @@ steps_without_start_positions
