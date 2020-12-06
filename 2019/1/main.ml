open Core

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:Int.of_string

let rec fuel_required ?(acc = 0) x =
  let r = (x / 3) - 2 in
  if r > 0 then fuel_required ~acc:(acc + r) r else acc

let () =
  Printf.printf "%d\n" (fuel_required 100756);

  List.map input ~f:(fun mass -> (mass / 3) - 2)
  |> List.fold ~init:0 ~f:( + )
  |> Printf.printf "part 1: %d\n";

  List.map input ~f:fuel_required
  |> List.fold ~init:0 ~f:( + )
  |> Printf.printf "part 2: %d\n"
