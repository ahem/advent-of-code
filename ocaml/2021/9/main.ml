module Grid = Map.Make (struct type t = int * int let compare = compare end) [@@ocamlformat "disable"]

let read_input () =
  let seq_of_line y line =
    String.to_seqi line
    |> Seq.map (fun (x, c) -> (x, y, int_of_char c - Char.code '0'))
  in
  Seq.unfold
    (fun y ->
      try Some (seq_of_line y (input_line stdin), y + 1)
      with End_of_file -> None)
    0
  |> Seq.concat
  |> Seq.fold_left (fun grid (x, y, v) -> Grid.add (x, y) v grid) Grid.empty

let adjacents grid (x, y) =
  List.to_seq [ (-1, 0); (0, 1); (1, 0); (0, -1) ]
  |> Seq.map (fun p -> (x + fst p, y + snd p))
  |> Seq.filter_map (fun p ->
         Grid.find_opt p grid |> Option.map (fun v -> (p, v)))
  |> Grid.of_seq

let find_low_points grid =
  Grid.filter
    (fun p v -> Grid.for_all (fun _ x -> v < x) (adjacents grid p))
    grid

let rec basin grid p pv seen =
  let seen = Grid.add p pv seen in
  let next_points =
    adjacents grid p
    |> Grid.filter (fun q v -> (not @@ Grid.mem q seen) && v != 9)
  in
  Grid.fold (basin grid) next_points seen

let () =
  let grid = read_input () in
  let low_points = find_low_points grid in
  let risk_levels =
    low_points |> Grid.bindings |> List.map (fun (_, v) -> v + 1)
  in
  Printf.printf "Part 1: %d\n" @@ List.fold_left ( + ) 0 risk_levels;

  let basin_sizes =
    low_points
    |> Grid.mapi (fun p pv -> basin grid p pv Grid.empty |> Grid.cardinal)
    |> Grid.bindings |> List.map snd |> List.sort Int.compare |> List.rev
  in
  match basin_sizes with
  | a :: b :: c :: _ -> Printf.printf "Part 2: %d\n" (a * b * c)
  | _ -> failwith "not enough basins!"
