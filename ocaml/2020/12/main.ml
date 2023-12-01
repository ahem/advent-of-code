open Core

let input =
  In_channel.input_lines In_channel.stdin
  |> List.map ~f:(fun s -> (s.[0], Int.of_string @@ String.slice s 1 0))

let move (x, y) (dx, dy) n = (x + (dx * n), y + (dy * n))

let rec turn_left direction n =
  match (direction, n) with
  | (x, y), 0 -> (x, y)
  | (x, y), n when n > 0 && n % 90 = 0 -> turn_left (-y, x) (n - 90)
  | _ -> failwith "invalid turn"

let rec turn_right direction n =
  match (direction, n) with
  | (x, y), 0 -> (x, y)
  | (x, y), n when n > 0 && n % 90 = 0 -> turn_right (y, -x) (n - 90)
  | _ -> failwith "invalid turn"

let part1 pos direction list =
  List.fold list ~init:(pos, direction) ~f:(fun (pos, direction) -> function
    | 'N', n -> (move pos (0, 1) n, direction)
    | 'S', n -> (move pos (0, -1) n, direction)
    | 'E', n -> (move pos (1, 0) n, direction)
    | 'W', n -> (move pos (-1, 0) n, direction)
    | 'R', n -> (pos, turn_right direction n)
    | 'L', n -> (pos, turn_left direction n)
    | 'F', n -> (move pos direction n, direction)
    | c, n -> failwith @@ Printf.sprintf "invalid move: %c%d" c n)
  |> fst

let part2 pos direction list =
  List.fold list ~init:(pos, direction) ~f:(fun (pos, direction) -> function
    | 'N', n -> (pos, move direction (0, 1) n)
    | 'S', n -> (pos, move direction (0, -1) n)
    | 'E', n -> (pos, move direction (1, 0) n)
    | 'W', n -> (pos, move direction (-1, 0) n)
    | 'R', n -> (pos, turn_right direction n)
    | 'L', n -> (pos, turn_left direction n)
    | 'F', n -> (move pos direction n, direction)
    | c, n -> failwith @@ Printf.sprintf "invalid move: %c%d" c n)
  |> fst

let () =
  let x, y = part1 (0, 0) (1, 0) input in
  Printf.printf "part 1 ends at (%d, %d) [distance: %d]\n" x y
    (Int.abs x + Int.abs y);

  let x, y = part2 (0, 0) (10, 1) input in
  Printf.printf "part 2 ends at (%d, %d) [distance: %d]\n" x y
    (Int.abs x + Int.abs y)
