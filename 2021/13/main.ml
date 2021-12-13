module Grid = Set.Make (struct type t = int * int let compare = compare end) [@@ocamlformat "disable"]

let read_input () =
  let rec loop acc =
    try loop (Scanf.scanf "%d,%d\n" (fun x y -> (x, y)) :: acc)
    with Scanf.Scan_failure _ -> acc
  in
  let positions = loop [] |> List.to_seq |> Grid.of_seq in

  Scanf.scanf "\n" ignore ();

  let rec loop acc =
    try loop (Scanf.scanf "fold along %c=%d\n" (fun c n -> (c, n)) :: acc)
    with End_of_file -> acc
  in
  let folds = List.rev @@ loop [] in

  (positions, folds)

let fold_grid_over_x grid n =
  Grid.fold
    (fun (x, y) acc ->
      if x > n then Grid.add (n - Int.abs (n - x), y) acc
      else Grid.add (x, y) acc)
    grid Grid.empty

let fold_grid_over_y grid n =
  Grid.fold
    (fun (x, y) acc ->
      let p = if y > n then (x, n - Int.abs (n - y)) else (x, y) in
      Grid.add p acc)
    grid Grid.empty

let fold_grid grid (c, n) =
  match c with
  | 'x' -> fold_grid_over_x grid n
  | 'y' -> fold_grid_over_y grid n
  | _ -> failwith "invalid fold direction"

let print_grid grid =
  let max_x =
    Grid.elements grid |> List.map fst |> List.sort Int.compare |> List.rev
    |> List.hd
  in
  let max_y =
    Grid.elements grid |> List.map snd |> List.sort Int.compare |> List.rev
    |> List.hd
  in
  Seq.unfold (fun y -> if y <= max_y then Some (y, y + 1) else None) 0
  |> Seq.iter (fun y ->
         Seq.unfold (fun x -> if x <= max_x then Some (x, x + 1) else None) 0
         |> Seq.iter (fun x ->
                Printf.printf "%c" (if Grid.mem (x, y) grid then '*' else ' '));
         Printf.printf "\n")

let () =
  let grid, folds = read_input () in
  fold_grid grid (List.hd folds)
  |> Grid.cardinal
  |> Printf.printf "Part 1: %d\n";

  Printf.printf "Part 2:\n";
  print_grid @@ List.fold_left fold_grid grid folds
