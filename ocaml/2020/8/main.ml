open Core

let input =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(function
       | [ opcode; n ] -> (opcode, Int.of_string n)
       | _ -> failwith "invalid input")
  |> Array.of_list

let rec walk ?(acc = 0) ?(pos = 0) ?(visited = Int.Set.empty) instructions =
  if pos = Array.length instructions then `Finished acc
  else if pos > Array.length instructions then `Invalid
  else if Int.Set.mem visited pos then `Infinite_loop acc
  else
    let visited = Int.Set.add visited pos in
    match instructions.(pos) with
    | "nop", _ -> walk instructions ~pos:(pos + 1) ~acc ~visited
    | "jmp", n -> walk instructions ~pos:(pos + n) ~acc ~visited
    | "acc", n -> walk instructions ~pos:(pos + 1) ~acc:(acc + n) ~visited
    | _ -> failwith "invalid instruction"

let copy_and_set arr idx value =
  let copy = Array.copy arr in
  copy.(idx) <- value;
  copy

let candidates : (string * int) array -> (string * int) array Sequence.t =
 fun instructions ->
  Sequence.range 0 (Array.length instructions)
  |> Sequence.map ~f:(fun idx ->
         match instructions.(idx) with
         | "nop", n -> Some (copy_and_set instructions idx ("jmp", n))
         | "jmp", n -> Some (copy_and_set instructions idx ("nop", n))
         | _ -> None)
  |> Sequence.filter_opt

let () =
  ( match walk input with
  | `Infinite_loop n -> Printf.printf "part 1 result: %d\n" n
  | _ -> failwith "no result for part 1!" );

  let part2_result =
    Sequence.find_map (candidates input) ~f:(fun candidate ->
        match walk candidate with `Finished n -> Some n | _ -> None)
  in
  match part2_result with
  | Some n -> Printf.printf "part 2 result: %d\n" n
  | None -> failwith "no result for part 2!"
