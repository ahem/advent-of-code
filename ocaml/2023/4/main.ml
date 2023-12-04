open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:String.strip

type game = { id : int; winning_numbers : int list; numbers : int list }
(* [@@deriving show] *)

let parse_game_regex = Re2.of_string "^Card\\s+(\\d+): ([0-9 ]+) \\| ([0-9 ]+)$"

let space_separated_numbers_to_ints : string -> int list =
 fun s -> String.split s ~on:' ' |> List.filter_map ~f:Int.of_string_opt

let parse_game : string -> game Or_error.t =
 fun s ->
  let open Or_error.Let_syntax in
  let%bind m = Re2.first_match parse_game_regex s in
  let id = Re2.Match.get_exn m ~sub:(`Index 1) |> Int.of_string in
  let winning_numbers =
    Re2.Match.get_exn m ~sub:(`Index 2) |> space_separated_numbers_to_ints
  in
  let numbers =
    Re2.Match.get_exn m ~sub:(`Index 3) |> space_separated_numbers_to_ints
  in
  Ok { id; winning_numbers; numbers }

let value : game -> int =
 fun { winning_numbers; numbers; _ } ->
  List.filter numbers ~f:(List.mem ~equal:Int.equal winning_numbers)
  |> List.fold ~init:0 ~f:(fun acc _ ->
         match acc with 0 -> 1 | acc -> 2 * acc)

let winnings : game -> int =
 fun { winning_numbers; numbers; _ } ->
  List.filter numbers ~f:(List.mem ~equal:Int.equal winning_numbers)
  |> List.length

let total_winnings : game list -> int =
 fun games ->
  let initial_amounts =
    List.fold games
      ~init:(Map.empty (module Int))
      ~f:(fun acc game -> Map.set acc ~key:game.id ~data:1)
  in
  List.fold games ~init:initial_amounts ~f:(fun acc game ->
      let current_amount = Map.find_exn acc game.id in
      Sequence.range (game.id + 1) (game.id + 1 + winnings game)
      |> Sequence.fold ~init:acc ~f:(fun acc card_id ->
             Map.update acc card_id ~f:(function
               | Some n -> n + current_amount
               | None -> failwith "unexpected")))
  |> Map.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)

let () =
  let games =
    List.map input ~f:parse_game |> Or_error.combine_errors |> Or_error.ok_exn
  in
  let part_1_result = List.fold games ~init:0 ~f:(fun acc x -> acc + value x) in
  printf "Part 1: %d\n" part_1_result;

  let part_2_result = total_winnings games in
  printf "Part 2: %d\n" part_2_result
