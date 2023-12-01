let read_lines : in_channel -> string list =
 fun f ->
  let rec loop acc = try loop (input_line f :: acc) with End_of_file -> acc in
  List.rev @@ loop []

type op = Forward of int | Down of int | Up of int

let parse_line s =
  match String.split_on_char ' ' s with
  | [ "forward"; n ] -> Forward (int_of_string n)
  | [ "down"; n ] -> Down (int_of_string n)
  | [ "up"; n ] -> Up (int_of_string n)
  | _ -> failwith "cannot parse line"

let move (horizontal, depth) = function
  | Forward n -> (horizontal + n, depth)
  | Down n -> (horizontal, depth + n)
  | Up n -> (horizontal, depth - n)

let move_with_aim (horizontal, depth, aim) = function
  | Forward n -> (horizontal + n, depth + (aim * n), aim)
  | Down n -> (horizontal, depth, aim + n)
  | Up n -> (horizontal, depth, aim - n)

let () =
  let input = read_lines stdin |> List.map parse_line in
  let horizontal, depth = List.fold_left move (0, 0) input in
  Printf.printf "Part 1 final pos: %d, %d (%d)\n" horizontal depth
    (horizontal * depth);
  let horizontal, depth, _ = List.fold_left move_with_aim (0, 0, 0) input in
  Printf.printf "Part 2 final pos: %d, %d (%d)\n" horizontal depth
    (horizontal * depth)
