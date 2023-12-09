open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin

let parse : string -> int list =
 fun s -> String.strip s |> String.split ~on:' ' |> List.map ~f:Int.of_string

let rec differences : int list -> int list = function
  | a :: b :: rest -> (b - a) :: differences (b :: rest)
  | _ -> []

let show : int list -> string =
 fun lst -> List.map lst ~f:Int.to_string |> String.concat ~sep:" "

let () = ignore show

let rec next_value : int list -> int =
 fun lst ->
  match List.for_all lst ~f:(Int.equal 0) with
  | false -> List.last_exn lst + next_value (differences lst)
  | true -> 0

let rec prev_value : int list -> int =
 fun lst ->
  match List.for_all lst ~f:(Int.equal 0) with
  | false -> List.hd_exn lst - prev_value (differences lst)
  | true -> 0

let () =
  let histories = List.map input ~f:parse in

  let part_1_result =
    List.map histories ~f:next_value
    |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
  in
  printf "Part 1 result: %d\n" part_1_result;

  let part_2_result =
    List.map histories ~f:prev_value
    |> List.fold ~init:0 ~f:(fun acc x -> acc + x)
  in
  printf "Part 2 result: %d\n" part_2_result
