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
  let max_x, max_y = Grid.max_elt grid in
  for y = 0 to max_y + 1 do
    for x = 0 to max_x + 1 do
      Printf.printf "%c" (if Grid.mem (x, y) grid then '*' else ' ')
    done;
    Printf.printf "\n"
  done

let () =
  let grid, folds = read_input () in
  fold_grid grid (List.hd folds)
  |> Grid.cardinal
  |> Printf.printf "Part 1: %d\n";

  Printf.printf "Part 2:\n";
  print_grid @@ List.fold_left fold_grid grid folds
