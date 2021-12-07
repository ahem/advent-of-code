let read_input () =
  input_line stdin |> String.split_on_char ',' |> List.map int_of_string

let range start stop =
  Seq.unfold (fun i -> if i < stop then Some (i, i + 1) else None) start

let fac n =
  let rec loop acc = function 0 -> acc | n -> loop (n + acc) (n - 1) in
  loop 0 n

let () =
  let numbers = read_input () in

  let min = List.fold_left Int.min Int.max_int numbers in
  let max = List.fold_left Int.max 0 numbers in

  range min (max + 1)
  |> Seq.map (fun n ->
         List.fold_left (fun acc x -> acc + Int.abs (x - n)) 0 numbers)
  |> Seq.fold_left Int.min Int.max_int
  |> Printf.printf "Part1: %d\n";

  range min (max + 1)
  |> Seq.map (fun n ->
         List.fold_left (fun acc x -> acc + fac (Int.abs (x - n))) 0 numbers)
  |> Seq.fold_left Int.min Int.max_int
  |> Printf.printf "Part2: %d\n"
