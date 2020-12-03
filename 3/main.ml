open Core

let input = In_channel.input_lines In_channel.stdin |> Array.of_list

let step pos dir = (fst pos + fst dir, snd pos + snd dir)

let is_tree map pos =
  let row = Array.get map @@ snd pos in
  let len = String.length row in
  Char.equal row.[fst pos % len] '#'

let rec toboggan ~dir ?(pos = (0, 0)) ?(cnt = 0) map =
  match step pos dir with
  | _, y when y >= Array.length map -> cnt
  | pos ->
      let d = if is_tree map pos then 1 else 0 in
      toboggan map ~pos ~dir ~cnt + d

let () =
  Printf.printf "Part 1 [3, 1]: %d\n\n" @@ toboggan ~dir:(3, 1) input;
  let candidates = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ] in
  List.fold candidates ~init:1 ~f:(fun acc dir ->
      let trees_hit = toboggan ~dir input in
      Printf.printf "Slope [%d, %d]: %d\n" (fst dir) (snd dir) trees_hit;
      acc * trees_hit)
  |> Printf.printf "Part 2 result: %d\n"
