open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:String.strip

let digit_from_str : string -> int Or_error.t = function 
    | "1" | "one" -> Ok(1)
    | "2" | "two" -> Ok(2)
    | "3" | "three" -> Ok(3)
    | "4" | "four" -> Ok(4)
    | "5" | "five" -> Ok(5)
    | "6" | "six" -> Ok(6)
    | "7" | "seven" -> Ok(7)
    | "8" | "eight" -> Ok(8)
    | "9" | "nine" -> Ok(9)
    | _ -> Or_error.error_string "invalid digit"

let rec find_overlapping_matches : Re2.t -> string -> string list = fun regex s ->
    match Re2.first_match regex s with
    | Ok(m) ->
        let (offset, _) = Re2.Match.get_pos_exn m ~sub:(`Index 0) in
        let m = Re2.Match.get_exn m ~sub:(`Index 0) in
        m :: find_overlapping_matches regex (String.subo ~pos:(offset + 1) s)
    | Error(_) -> []

let calibration_value : Re2.t -> string -> int Or_error.t = fun regex s ->
    find_overlapping_matches regex s |> List.map ~f:digit_from_str |> Or_error.all |> function
        | Error(err) -> Error(err)
        | Ok([]) -> Or_error.error_string("no digits")
        | Ok(digits) ->
                let first = List.hd_exn digits in
                let last = List.last_exn digits in
                Ok(first * 10 + last)

let sum_of_calibration_values : Re2.t -> string list -> int = fun regex ->
    List.fold ~init:0 ~f:(fun acc s -> acc + (calibration_value regex s |> Or_error.ok_exn))

let () =
    let part1_regex =Re2.of_string "\\d" in
    printf "Part 1: %d\n" @@ sum_of_calibration_values part1_regex input;
    let part2_regex =Re2.of_string "\\d|one|two|three|four|five|six|seven|eight|nine" in
    printf "Part 2: %d\n" @@ sum_of_calibration_values part2_regex input;

