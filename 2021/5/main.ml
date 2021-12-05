module PointMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

module Line = struct
  let is_horiz_or_vert p1 p2 = fst p1 = fst p2 || snd p1 = snd p2

  let draw p1 p2 =
    let dx = fst p2 - fst p1 in
    let dy = snd p2 - snd p1 in
    let s = Int.max (Int.abs dx) (Int.abs dy) in
    let sx = dx / s in
    let sy = dy / s in
    Seq.unfold
      (fun p ->
        if (fst p - sx, snd p - sy) = p2 then None
        else Some (p, (fst p + sx, snd p + sy)))
      p1
end

module Grid = struct
  type t = int PointMap.t

  let empty : t = PointMap.empty

  let get grid p = match PointMap.find_opt p grid with Some x -> x | None -> 0

  let incr grid p = PointMap.add p (get grid p + 1) grid

  let add_line grid line = Seq.fold_left incr grid line

  let count_overlaps grid =
    PointMap.bindings grid |> List.filter (fun (_, x) -> x >= 2) |> List.length
end

let read_data : unit -> ((int * int) * (int * int)) list =
 fun () ->
  let rec loop acc =
    try
      let line =
        Scanf.scanf "%d,%d -> %d,%d\n" (fun a b c d -> ((a, b), (c, d)))
      in
      loop (line :: acc)
    with End_of_file -> acc
  in
  List.rev @@ loop []

let () =
  let input = read_data () in

  let grid =
    input
    |> List.filter (fun (p1, p2) -> Line.is_horiz_or_vert p1 p2)
    |> List.map (fun (p1, p2) -> Line.draw p1 p2)
    |> List.fold_left Grid.add_line Grid.empty
  in
  Printf.printf "%d\n" @@ Grid.count_overlaps grid;

  let grid =
    input
    |> List.map (fun (p1, p2) -> Line.draw p1 p2)
    |> List.fold_left Grid.add_line Grid.empty
  in
  Printf.printf "%d\n" @@ Grid.count_overlaps grid
