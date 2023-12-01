open Core

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:Int.of_string

let rec find_differences ?(acc = Int.Map.empty) = function
  | x :: y :: tail ->
      let diff = y - x in
      let acc =
        Int.Map.set acc ~key:diff
          ~data:((Option.value ~default:0 @@ Int.Map.find acc diff) + 1)
      in
      find_differences ~acc (y :: tail)
  | _ -> acc

let count_combinations list =
  List.fold (List.rev list) ~init:Int.Map.empty ~f:(fun acc x ->
      let combos = Map.find acc (x + 1) |> Option.value ~default:0 in
      let combos = combos + (Map.find acc (x + 2) |> Option.value ~default:0) in
      let combos = combos + (Map.find acc (x + 3) |> Option.value ~default:0) in
      Int.Map.add_exn acc ~key:x ~data:(if combos > 0 then combos else 1))

let () =
  let sorted =
    0 :: (Option.value_exn (List.max_elt ~compare input) + 3) :: input
    |> List.sort ~compare
  in
  let differences = find_differences sorted in
  Int.Map.iteri differences ~f:(fun ~key ~data ->
      Printf.printf "%d: %d\n" key data);
  Printf.printf "part 1 result: %d\n"
    (Map.find_exn differences 1 * Map.find_exn differences 3);

  Printf.printf "part 2 result: %d\n"
  @@ Int.Map.find_exn (count_combinations sorted) 0
