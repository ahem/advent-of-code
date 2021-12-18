let read_input () =
  Scanf.scanf "target area: x=%d..%d, y=%d..%d" (fun x1 x2 y1 y2 ->
      ((Int.min x1 x2, Int.max y1 y2), (Int.max x1 x2, Int.min y1 y2)))

let range start stop =
  Seq.unfold (fun i -> if i < stop then Some (i, i + 1) else None) start

let find_possible_x_velocities target_area =
  let (x1, _), (x2, _) = target_area in
  range 1 (x2 + 1)
  |> Seq.filter (fun initial_velocity ->
         let rec test x velocity =
           match x with
           | x when x > x2 || velocity = 0 -> false
           | x when x >= x1 && x <= x2 -> true
           | x -> test (x + velocity) (velocity - 1)
         in
         test 0 initial_velocity)

let find_possible_paths_for_x_velocity x target_area =
  let (x1, y1), (x2, y2) = target_area in
  let max_y = 0 - y2 in
  let min_y = y2 in
  range min_y (max_y + 1)
  |> Seq.filter (fun y ->
         let rec test x y x_velocity y_velocity =
           match y with
           | y when y < y2 -> false
           | y when y <= y1 && y >= y2 && x >= x1 && x <= x2 -> true
           | y ->
               let x = x + x_velocity in
               let y = y + y_velocity in
               let x_velocity = if x_velocity > 0 then x_velocity - 1 else 0 in
               let y_velocity = y_velocity - 1 in
               test x y x_velocity y_velocity
         in
         test 0 0 x y)

let () =
  let target_area = read_input () in
  let (x1, y1), (x2, y2) = target_area in
  Printf.printf "(%d, %d) -> (%d, %d)\n" x1 y1 x2 y2;

  (* the highest possible curve, will come at the highest possible speed downwards, so
     it is going to be a curve hitting the lowest row in the target area. Given this,
     we can start from the bottom line and imagine a curve that goes up from zero, then
     down again, and when it return to zero it must have accumulated a negative
     velocity of exactly ZERO - BOTTOM_OF_TARGET_AREA. That is, it will hit the bottom
     line of the target with the next step.

     With a little bit of reasoning, we can see that the curve must have started with a
     positive velocity of (ZERO - BOTTOM_OF_TARGET_AREA) - ONE.

     The last -1 is because the velocity will also increase with one on the laste step.

     Formula for "1 + 2 + 3 + ... + n" is: (n * (n + 1)) / 2
  *)
  Printf.printf "Part 1: %d\n" ((-1 - y2) * (0 - y2) / 2);

  let possible_initial_velocities =
    find_possible_x_velocities target_area
    |> Seq.flat_map (fun x ->
           find_possible_paths_for_x_velocity x target_area
           |> Seq.map (fun y -> (x, y)))
  in

  Printf.printf "Part 2: %d\n"
  @@ Seq.fold_left (fun acc _ -> acc + 1) 0 possible_initial_velocities;
  ()
