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

let print_grid : int Grid.t -> unit =
 fun grid ->
  let max_x =
    Grid.bindings grid |> List.map fst |> List.map fst |> List.sort Int.compare
    |> List.rev |> List.hd
  in
  let max_y =
    Grid.bindings grid |> List.map fst |> List.map snd |> List.sort Int.compare
    |> List.rev |> List.hd
  in
  Seq.unfold (fun y -> if y <= max_y then Some (y, y + 1) else None) 0
  |> Seq.iter (fun y ->
         Seq.unfold (fun x -> if x <= max_x then Some (x, x + 1) else None) 0
         |> Seq.iter (fun x -> Printf.printf "%d" (Grid.find (x, y) grid));
         Printf.printf "\n");
  Printf.printf "\n"

let directions =
  [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

let rec flash : int Grid.t -> Grid.key -> int Grid.t =
 fun grid (x, y) ->
  if Grid.find (x, y) grid = 0 then grid
  else
    (* reset flashing square to 0 *)
    let grid = Grid.add (x, y) 0 grid in

    (* then process adjacents (increase with 1, flash if necessary) *)
    List.fold_left
      (fun grid (dx, dy) ->
        match Grid.find_opt (x + dx, y + dy) grid with
        | None | Some 0 -> grid
        | Some v when v < 9 -> Grid.add (x + dx, y + dy) (v + 1) grid
        | Some _ -> flash grid (x + dx, y + dy))
      grid directions

let step grid =
  (* increase all *)
  let grid = Grid.map (( + ) 1) grid in

  (* process flashses *)
  let grid =
    Grid.filter (fun _ v -> v > 9) grid
    |> Grid.bindings |> List.map fst |> List.fold_left flash grid
  in

  (* count all flashes this turn *)
  let flash_cnt =
    Grid.filter (fun _ v -> v = 0) grid |> Grid.bindings |> List.length
  in

  (grid, flash_cnt)

let () =
  let grid = read_input () in

  let _, flash_cnt =
    Seq.unfold (fun i -> if i < 100 then Some (i, i + 1) else None) 0
    |> Seq.fold_left
         (fun (grid, flash_cnt) _ ->
           let grid, cnt = step grid in
           (grid, flash_cnt + cnt))
         (grid, 0)
  in
  Printf.printf "Part 1: %d\n" flash_cnt;

  let rec loop grid n =
    let grid, cnt = step grid in
    if Grid.cardinal grid = cnt then n else loop grid (n + 1)
  in

  Printf.printf "Part 2: %d\n" @@ loop grid 1
