open Core

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:Int.of_string

let check : int list -> int -> bool =
 fun list n ->
  let seq = Sequence.of_list list in
  Sequence.cartesian_product seq seq
  |> Sequence.filter ~f:(fun (x, y) -> x <> y)
  |> Sequence.exists ~f:(fun (x, y) -> x + y = n)

let rec find_first_not_valid preamble = function
  | hd :: _ when not (check preamble hd) -> hd
  | hd :: tail ->
      find_first_not_valid (List.append (List.drop preamble 1) [ hd ]) tail
  | [] -> failwith "no more numbers"

let exploit : int -> int list -> int * int =
 fun n list ->
  let numbers =
    Sequence.range 0 (List.length list - 2)
    |> Sequence.concat_map ~f:(fun start ->
           Sequence.range (start + 2) (List.length list + 1)
           |> Sequence.map ~f:(fun stop -> List.slice list start stop))
    |> Sequence.find_exn ~f:(fun candidate ->
           (* List.iter candidate ~f:(Printf.printf "%d "); *)
           (* Printf.printf "\n"; *)
           n = List.fold candidate ~init:0 ~f:( + ))
  in
  let open Int in
  let min = Option.value_exn (List.min_elt ~compare numbers) in
  let max = Option.value_exn (List.max_elt ~compare numbers) in
  (min, max)

let () =
  let size = 25 in
  let invalid_number =
    find_first_not_valid (List.take input size) (List.drop input size)
  in
  Printf.printf "part 1: %d\n" invalid_number;

  let min, max = exploit invalid_number input in
  Printf.printf "part 2: %d + %d = %d\n" min max (min + max)
