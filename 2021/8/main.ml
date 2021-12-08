module CharSet = Set.Make (Char)
module DigitMap = Map.Make (CharSet)

let set_of_string s = CharSet.of_seq @@ String.to_seq s

let read_input () =
  let rec loop acc =
    try
      let line =
        Scanf.scanf "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s\n"
          (fun a b c d e f g h i j r1 r2 r3 r4 ->
            ( List.map set_of_string [ a; b; c; d; e; f; g; h; i; j ],
              List.map set_of_string [ r1; r2; r3; r4 ] ))
      in
      loop (line :: acc)
    with End_of_file -> acc
  in
  List.rev @@ loop []

let missing_one ~from a = CharSet.cardinal @@ CharSet.diff from a = 1

let has_all ~from a = CharSet.cardinal @@ CharSet.diff from a = 0

let len d set = CharSet.cardinal set = d

let both f g x = f x && g x

let find_and_remove :
    CharSet.t list -> (CharSet.t -> bool) -> CharSet.t * CharSet.t list =
 fun lst f ->
  let found, rest = List.partition f lst in
  match found with
  | [ x ] -> (x, rest)
  | _ -> failwith "more than one result found!"

let find_digits : CharSet.t list -> int DigitMap.t =
 fun lst ->
  let one, lst = find_and_remove lst (len 2) in
  let four, lst = find_and_remove lst (len 4) in
  let seven, lst = find_and_remove lst (len 3) in
  let eight, lst = find_and_remove lst (len 7) in
  let six, lst = find_and_remove lst (both (len 6) (missing_one ~from:one)) in
  let three, lst = find_and_remove lst (both (len 5) (has_all ~from:one)) in
  let five, lst = find_and_remove lst (both (len 5) (missing_one ~from:four)) in
  let two, lst = find_and_remove lst (len 5) in
  let zero, lst = find_and_remove lst (both (len 6) (missing_one ~from:four)) in
  let nine = match lst with [ x ] -> x | _ -> failwith "unexpected error" in
  [ zero; one; two; three; four; five; six; seven; eight; nine ]
  |> List.mapi (fun n v -> (v, n))
  |> List.to_seq |> DigitMap.of_seq

let decode_result (lst, result) =
  let digits = find_digits lst in
  match List.map (fun x -> DigitMap.find x digits) result with
  | [ r1; r2; r3; r4 ] -> (r1 * 1000) + (r2 * 100) + (r3 * 10) + r4
  | _ -> failwith "bad result!"

let () =
  let input = read_input () in

  input |> List.map snd
  |> List.map
       (List.filter (fun r ->
            match CharSet.cardinal r with 2 | 4 | 3 | 7 -> true | _ -> false))
  |> List.map List.length |> List.fold_left ( + ) 0
  |> Printf.printf "Part 1: %d\n";

  input |> List.map decode_result |> List.fold_left ( + ) 0
  |> Printf.printf "Part 2: %d\n"
