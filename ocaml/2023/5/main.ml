open Base
open Stdio

let input = In_channel.input_all In_channel.stdin

type sequence_map = { dest : int; src : int; length : int } [@@deriving show]

type t = {
  seeds : int list;
  seed_to_soil : sequence_map list;
  soil_to_fertilizer : sequence_map list;
  fertilizer_to_water : sequence_map list;
  water_to_light : sequence_map list;
  light_to_temperature : sequence_map list;
  temperature_to_humidity : sequence_map list;
  humidity_to_location : sequence_map list;
}
[@@deriving show]

let space_regex = Re2.of_string "\\s+"

let parse_numbers : string -> int list =
 fun s ->
  String.strip s |> Re2.split space_regex
  |> List.map ~f:(fun n -> String.strip n |> Int.of_string)

let block_regex =
  Re2.create "([a-z -]+):((?:\\d|\\s|\\n)+)\\n" |> Or_error.ok_exn

let parse_blocks : string -> (string * int list) list =
 fun s ->
  Re2.get_matches_exn block_regex s
  |> List.map ~f:(fun m ->
         let header = Re2.Match.get_exn m ~sub:(`Index 1) in
         let numbers = Re2.Match.get_exn m ~sub:(`Index 2) |> parse_numbers in
         (header, numbers))

let sequence_map_of_block : string * int list -> sequence_map list =
 fun (_, numbers) ->
  List.chunks_of numbers ~length:3
  |> List.map ~f:(function
       | [ dest; src; length ] -> { dest; src; length }
       | _ -> failwith "unexpected sequence mapping")

let parse : string -> t =
 fun s ->
  let blocks = parse_blocks s in
  {
    seeds = List.nth_exn blocks 0 |> snd;
    seed_to_soil = List.nth_exn blocks 1 |> sequence_map_of_block;
    soil_to_fertilizer = List.nth_exn blocks 2 |> sequence_map_of_block;
    fertilizer_to_water = List.nth_exn blocks 3 |> sequence_map_of_block;
    water_to_light = List.nth_exn blocks 4 |> sequence_map_of_block;
    light_to_temperature = List.nth_exn blocks 5 |> sequence_map_of_block;
    temperature_to_humidity = List.nth_exn blocks 6 |> sequence_map_of_block;
    humidity_to_location = List.nth_exn blocks 7 |> sequence_map_of_block;
  }

let convert : sequence_map list -> int -> int =
 fun mappings x ->
  List.find_map mappings ~f:(fun { dest; src; length } ->
      if x >= src && x < src + length then Some (dest + (x - src)) else None)
  |> Option.value ~default:x

let seed_to_location : t -> int -> int =
 fun almanac x ->
  convert almanac.seed_to_soil x
  |> convert almanac.soil_to_fertilizer
  |> convert almanac.fertilizer_to_water
  |> convert almanac.water_to_light
  |> convert almanac.light_to_temperature
  |> convert almanac.temperature_to_humidity
  |> convert almanac.humidity_to_location

let reverse_convert : sequence_map list -> int -> int =
 fun mappings x ->
  List.find_map mappings ~f:(fun { dest; src; length } ->
      if x >= dest && x < dest + length then Some (src + (x - dest)) else None)
  |> Option.value ~default:x

let location_to_seed : t -> int -> int =
 fun almanac x ->
  reverse_convert almanac.humidity_to_location x
  |> reverse_convert almanac.temperature_to_humidity
  |> reverse_convert almanac.light_to_temperature
  |> reverse_convert almanac.water_to_light
  |> reverse_convert almanac.fertilizer_to_water
  |> reverse_convert almanac.soil_to_fertilizer
  |> reverse_convert almanac.seed_to_soil

let is_in_seed_range : t -> int -> bool =
 fun almanac x ->
  List.chunks_of ~length:2 almanac.seeds
  |> List.exists ~f:(function
       | [ start; length ] -> start <= x && x < start + length
       | _ -> false)

let () =
  let o = parse input in
  printf "%s\n" @@ show o;

  let part_1_result =
    List.map o.seeds ~f:(seed_to_location o)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in
  printf "part 1: %d\n" part_1_result;
  Stdio.Out_channel.flush Stdio.stdout;

  let part_2_result_seed =
    Sequence.range 0 Int.max_value
    |> Sequence.map ~f:(location_to_seed o)
    |> Sequence.filter ~f:(is_in_seed_range o)
    |> Sequence.hd_exn
  in
  let part_2_result = seed_to_location o part_2_result_seed in
  printf "part 2: %d\n" part_2_result
