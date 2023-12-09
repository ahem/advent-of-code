open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin
let regex = Re2.of_string "[A-Z0-9]+"

type t = (string * string) Map.M(String).t

let parse_map : string list -> t =
  List.fold
    ~init:(Map.empty (module String))
    ~f:(fun acc line ->
      match Re2.find_all regex line with
      | Ok [ key; left; right ] -> Map.add_exn acc ~key ~data:(left, right)
      | _ -> acc)

let parse : string list -> char list * t =
 fun lst ->
  let steps = List.hd_exn lst |> String.strip |> String.to_list in
  let map = parse_map @@ List.sub input ~pos:2 ~len:(List.length lst - 2) in
  (steps, map)

let walk : t -> char Sequence.t -> string -> string Sequence.t =
 fun map steps node ->
  Sequence.folding_map steps ~init:node ~f:(fun node step ->
      match step with
      | 'L' -> Map.find_exn map node |> fst |> fun x -> (x, node)
      | 'R' -> Map.find_exn map node |> snd |> fun x -> (x, node)
      | _ -> failwith "unreachable")

let () =
  let steps, map = parse input in

  let part_1_result =
    walk map (Sequence.cycle_list_exn steps) "AAA"
    |> Sequence.fold_until ~init:0
         ~f:(fun acc -> function "ZZZ" -> Stop acc | _ -> Continue (acc + 1))
         ~finish:(fun x -> x)
  in
  printf "Part 1: %d\n" part_1_result;

  let ghost_walk_lengths =
    Map.keys map
    |> List.filter ~f:(String.is_suffix ~suffix:"A")
    |> List.map ~f:(fun start_node ->
           walk map (Sequence.cycle_list_exn steps) start_node
           |> Sequence.fold_until ~init:0
                ~f:(fun acc s ->
                  match String.is_suffix s ~suffix:"Z" with
                  | true -> Stop acc
                  | false -> Continue (acc + 1))
                ~finish:(fun x -> x))
  in

  printf "%s\n" @@ String.concat ~sep:"; "
  @@ List.map ~f:Int.to_string ghost_walk_lengths;

  let longest_path =
    List.max_elt ~compare:Int.compare ghost_walk_lengths |> Option.value_exn
  in

  (* there is probably a fancy math way to do this better :-) *)
  let part_2_result =
    Sequence.range ~stride:longest_path longest_path Int.max_value
    |> Sequence.find ~f:(fun x ->
           List.for_all ghost_walk_lengths ~f:(fun y -> x % y = 0))
    |> Option.value_exn
  in

  printf "Part 2: %d\n" part_2_result
