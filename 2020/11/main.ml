open Core
module Coord = Tuple.Comparable (Int) (Int)

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:String.strip

let width = String.length @@ List.hd_exn input

let height = List.length input

let initial_state =
  List.foldi input ~init:Coord.Map.empty ~f:(fun y acc row ->
      String.foldi row ~init:acc ~f:(fun x acc -> function
        | '#' -> Coord.Map.add_exn acc ~key:(x, y) ~data:1
        | 'L' -> Coord.Map.add_exn acc ~key:(x, y) ~data:0
        | _ -> acc))

let directions =
  [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

let part1_step state =
  Coord.Map.mapi state ~f:(fun ~key:(x, y) ~data ->
      let cnt =
        List.map directions ~f:(fun (dx, dy) ->
            Coord.Map.find state (x + dx, y + dy) |> Option.value ~default:0)
        |> List.fold ~init:0 ~f:( + )
      in
      if data = 0 && cnt = 0 then 1
      else if data = 1 && cnt >= 4 then 0
      else data)

let part2_step state =
  let rec seat_in_direction (x, y) (dx, dy) =
    if x < 0 || x >= width || y < 0 || y >= height then None
    else
      match Coord.Map.find state (x + dx, y + dy) with
      | Some x -> Some x
      | None -> seat_in_direction (x + dx, y + dy) (dx, dy)
  in

  Coord.Map.mapi state ~f:(fun ~key:(x, y) ~data ->
      let cnt =
        List.map directions ~f:(fun dir ->
            seat_in_direction (x, y) dir |> Option.value ~default:0)
        |> List.fold ~init:0 ~f:( + )
      in
      if data = 0 && cnt = 0 then 1
      else if data = 1 && cnt >= 5 then 0
      else data)

let rec step_until_stable ~step state =
  let next_state = step state in
  if Coord.Map.equal equal state next_state then state
  else step_until_stable ~step next_state

let () =
  step_until_stable ~step:part1_step initial_state
  |> Coord.Map.count ~f:(fun x -> x = 1)
  |> Printf.printf "part 1 result: %d\n";

  step_until_stable ~step:part2_step initial_state
  |> Coord.Map.count ~f:(fun x -> x = 1)
  |> Printf.printf "part 2 result: %d\n"
