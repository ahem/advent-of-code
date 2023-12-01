open Core
module Int_tuple = Tuple.Comparable (Int) (Int)

let input = In_channel.input_lines In_channel.stdin

let trace_wirepath p =
  String.split_on_chars p ~on:[ ',' ]
  |> List.map ~f:(fun s ->
         match (s.[0], String.slice s 1 0 |> Int.of_string) with
         | 'R', x -> Sequence.take (Sequence.repeat (1, 0)) x
         | 'L', x -> Sequence.take (Sequence.repeat (-1, 0)) x
         | 'U', x -> Sequence.take (Sequence.repeat (0, 1)) x
         | 'D', x -> Sequence.take (Sequence.repeat (0, -1)) x
         | _ -> failwith "wat?")
  |> Sequence.of_list |> Sequence.concat
  |> Sequence.fold ~init:(Int_tuple.Map.empty, 0, 0, 0)
       ~f:(fun (acc, x, y, cnt) (dx, dy) ->
         let x = x + dx in
         let y = y + dy in
         let cnt = cnt + 1 in
         if not (Map.mem acc (x, y)) then
           (Map.add_exn acc ~key:(x, y) ~data:cnt, x, y, cnt)
         else (acc, x, y, cnt))
  |> fun (acc, _, _, _) -> acc

let () =
  let trace_a = trace_wirepath (List.nth_exn input 0) in
  let trace_b = trace_wirepath (List.nth_exn input 1) in
  let intersections =
    Int_tuple.Set.inter
      (Int_tuple.Map.key_set trace_a)
      (Int_tuple.Map.key_set trace_b)
    |> Int_tuple.Set.to_list
  in

  List.map intersections ~f:(fun (x, y) -> Int.abs x + Int.abs y)
  |> List.min_elt ~compare:Int.compare
  |> fun x ->
  Option.value_exn x |> Printf.printf "part 1 result: %d\n";

  List.map intersections ~f:(fun pos ->
      Int_tuple.Map.find_exn trace_a pos + Int_tuple.Map.find_exn trace_b pos)
  |> List.min_elt ~compare:Int.compare
  |> fun x -> Option.value_exn x |> Printf.printf "part 2 result: %d\n"
