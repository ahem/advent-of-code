let read_lines : in_channel -> string list =
 fun f ->
  let rec loop acc = try loop (input_line f :: acc) with End_of_file -> acc in
  List.rev @@ loop []

let parse_groups : int option list -> int list =
  List.fold_left
    (fun acc x ->
      match x with
      | Some n -> (List.hd acc + n) :: List.tl acc
      | None -> 0 :: acc)
    [ 0 ]

let () =
  let input = read_lines stdin |> List.map int_of_string_opt in
  let sizes = parse_groups input |> List.sort (Fun.flip Int.compare) in
  Printf.printf "\nPart 1: %d\n" @@ List.hd sizes;

  let part_2_result = Seq.take 3 (List.to_seq sizes) |> Seq.fold_left ( + ) 0 in
  Printf.printf "Part 2: %d\n" @@ part_2_result
