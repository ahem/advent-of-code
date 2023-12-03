open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:String.strip

let parse_handfull : string -> int Map.M(String).t Or_error.t =
 fun s ->
  String.strip s |> String.split ~on:','
  |> List.fold
       ~init:(Ok (Map.empty (module String)))
       ~f:(fun acc s ->
         match (acc, String.strip s |> String.split ~on:' ') with
         | Ok map, [ v; k ] -> (
             let key = String.strip k in
             match Int.of_string_opt v with
             | Some data -> Ok (Map.set map ~key ~data)
             | None ->
                 Or_error.error_string
                 @@ Printf.sprintf "parse error: '%s' is not a int" v)
         | _ -> Or_error.error_string @@ Printf.sprintf "parse error: '%sø" s)

let parse_line : string -> int Map.M(String).t list Or_error.t =
 fun s ->
  String.split ~on:':' s |> List.last_exn |> String.strip
  |> String.split ~on:';' |> List.map ~f:parse_handfull
  |> Or_error.combine_errors

let max_values : int Map.M(String).t list -> int Map.M(String).t =
  List.fold
    ~init:(Map.empty (module String))
    ~f:(fun acc ->
      Map.fold ~init:acc ~f:(fun ~key ~data acc ->
          if data > Option.value ~default:0 @@ Map.find acc key then
            Map.set acc ~key ~data
          else acc))

let power : int Map.M(String).t -> int =
  Map.fold ~init:1 ~f:(fun ~key:_ ~data acc -> acc * data)

let _print_map : int Map.M(String).t -> unit =
 fun map ->
  Map.iteri map ~f:(fun ~key ~data -> printf "%s=%d " key data);
  printf "\n"

let () =
  let games = List.map input ~f:parse_line |> Or_error.all |> Or_error.ok_exn in
  let max_possible = List.map games ~f:max_values in
  let part_1_result =
    List.foldi max_possible ~init:0 ~f:(fun idx acc v ->
        if
          Map.find v "red" |> Option.value ~default:0 <= 12
          && Map.find v "green" |> Option.value ~default:0 <= 13
          && Map.find v "blue" |> Option.value ~default:0 <= 14
        then acc + idx + 1
        else acc)
  in
  printf "Part 1: %d\n" part_1_result;

  let part_2_result =
    List.fold max_possible ~init:0 ~f:(fun acc v -> acc + power v)
  in
  printf "Part 2: %d\n" part_2_result
