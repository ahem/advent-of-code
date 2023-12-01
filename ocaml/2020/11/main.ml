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

let neighbour_seat_in_direction state (x, y) (dx, dy) =
  Coord.Map.find state (x + dx, y + dy)

let rec seat_in_direction state (x, y) (dx, dy) =
  if x < 0 || x >= width || y < 0 || y >= height then None
  else
    match Coord.Map.find state (x + dx, y + dy) with
    | Some x -> Some x
    | None -> seat_in_direction state (x + dx, y + dy) (dx, dy)

let step ~check ~limit state =
  Coord.Map.mapi state ~f:(fun ~key:pos ~data ->
      let seats = List.filter_map directions ~f:(check state pos) in
      let cnt = List.fold seats ~init:0 ~f:( + ) in
      if data = 0 && cnt = 0 then 1
      else if data = 1 && cnt >= limit then 0
      else data)

let rec step_until_stable ~check ~limit state =
  let next_state = step ~check ~limit state in
  let stable = Coord.Map.equal equal state next_state in
  if stable then state else step_until_stable ~check ~limit next_state

let () =
  step_until_stable initial_state ~limit:4 ~check:neighbour_seat_in_direction
  |> Coord.Map.count ~f:(fun x -> x = 1)
  |> Printf.printf "part 1 result: %d\n";

  step_until_stable initial_state ~limit:5 ~check:seat_in_direction
  |> Coord.Map.count ~f:(fun x -> x = 1)
  |> Printf.printf "part 2 result: %d\n"
