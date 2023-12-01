open Core

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:String.strip

let search schedule nn n =
  match schedule with
  | (ts, diff) :: schedule ->
      let seq =
        Sequence.range ((ts * n) - diff) Int.max_value ~stride:(nn * ts)
      in
      Sequence.find seq ~f:(fun x ->
          List.for_all schedule ~f:(fun (ts, diff) -> (x + diff) % ts = 0))
      |> Option.iter ~f:(fun x ->
             Printf.printf "result [%n]: %d\n\n" n x;
             exit 0)
  | _ -> failwith "wat"

let () =
  let ts = List.hd_exn input |> Int.of_string in

  List.last_exn input |> String.split ~on:','
  |> List.filter_map ~f:int_of_string_opt
  |> List.map ~f:(fun x ->
         let minutes_to_wait = (x * (1 + (ts / x))) - ts in
         (x, minutes_to_wait))
  |> List.min_elt ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> Option.iter ~f:(fun (id, wait) ->
         Printf.printf "part 1 result: %d\n\n" (id * wait));

  Out_channel.flush Out_channel.stdout;

  let schedule =
    List.last_exn input |> String.split ~on:','
    |> List.foldi ~init:[] ~f:(fun idx acc -> function
         | "x" -> acc | s -> (Int.of_string s, idx) :: acc)
  in
  let schedule =
    List.sort schedule ~compare:(fun (a, _) (b, _) -> Int.compare b a)
  in

  List.map schedule ~f:(fun (a, b) -> Printf.sprintf "(x + %d) mod %d" b a)
  |> String.concat ~sep:" = "
  |> Printf.printf "Paste into Wolfram Alpha for part2 result:\n %s = 0\n\n"
