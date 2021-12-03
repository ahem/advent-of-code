let read_lines : in_channel -> string list =
 fun f ->
  let rec loop acc = try loop (input_line f :: acc) with End_of_file -> acc in
  List.rev @@ loop []

let rec count_increases = function
  | a :: b :: rst ->
      let x = if b > a then 1 else 0 in
      x + count_increases (b :: rst)
  | _ -> 0

let rec count_increases3 = function
  | a :: b :: c :: d :: rst ->
      let x = if b + c + d > a + b + c then 1 else 0 in
      x + count_increases3 (b :: c :: d :: rst)
  | _ -> 0

let () =
  let input = read_lines stdin |> List.map int_of_string in
  Printf.printf "part 1: %d\n" (count_increases input);
  Printf.printf "part 2: %d\n" (count_increases3 input)
