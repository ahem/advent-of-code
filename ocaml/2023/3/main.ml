open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:String.strip
let digit_regex = Re2.of_string "\\d+"
let non_symbol_regex = Re2.of_string "\\d+|\\."

type digit_string = { value : int; length : int; x : int; y : int }
(* #[@@deriving show] *)

let digit_string_of_match : y:int -> Re2.Match.t -> digit_string =
 fun ~y m ->
  let x = fst @@ Re2.Match.get_pos_exn ~sub:(`Index 0) m in
  let str = Re2.Match.get ~sub:(`Index 0) m |> Option.value_exn in
  let value = Int.of_string str in
  let length = String.length str in
  { value; length; x; y }

let neighbour_symbols : string list -> digit_string -> string =
 fun schematic number ->
  let line_length = List.hd_exn schematic |> String.length in
  let x1 = max 0 (number.x - 1) in
  let y1 = max 0 (number.y - 1) in
  let x2 = min line_length (number.x + number.length + 1) in
  let y2 = min (number.y + 1) (List.length schematic) in
  List.foldi schematic ~init:"" ~f:(fun idx acc line ->
      if idx >= y1 && idx <= y2 then
        String.append acc @@ String.sub line ~pos:x1 ~len:(x2 - x1)
      else acc)
  |> Re2.replace_exn non_symbol_regex ~f:(fun _ -> "")

let find_numbers : string list -> digit_string list =
  List.foldi ~init:[] ~f:(fun y acc line ->
      Re2.get_matches_exn digit_regex line
      |> List.map ~f:(digit_string_of_match ~y)
      |> List.append acc)

let find_symbol : c:char -> string list -> (int * int) list =
 fun ~c ->
  List.foldi ~init:[] ~f:(fun y acc ->
      String.foldi ~init:acc ~f:(fun x acc c' ->
          match Char.equal c c' with true -> (x, y) :: acc | false -> acc))

let neighbor_numbers : digit_string list -> int * int -> digit_string list =
 fun numbers (x, y) ->
  List.filter numbers ~f:(fun o ->
      Int.abs (o.y - y) <= 1 && x >= o.x - 1 && x <= o.x + o.length)

let () =
  let numbers = find_numbers input in
  let part_numbers =
    List.filter numbers ~f:(fun number ->
        not @@ String.is_empty @@ neighbour_symbols input number)
  in
  let part_1_result =
    List.fold part_numbers ~init:0 ~f:(fun acc n -> acc + n.value)
  in
  printf "Part 1: %d\n" part_1_result;

  let gears =
    find_symbol ~c:'*' input
    |> List.map ~f:(neighbor_numbers numbers)
    |> List.filter ~f:(fun lst -> List.length lst = 2)
  in
  let part_2_result =
    List.fold gears ~init:0 ~f:(fun acc -> function
      | [ a; b ] -> acc + (a.value * b.value) | _ -> failwith "bad gear")
  in
  printf "Part 2: %d\n" part_2_result
