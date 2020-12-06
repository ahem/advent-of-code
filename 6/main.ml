open Core

let input = In_channel.input_lines In_channel.stdin

let parse : string list -> Char.Set.t list list =
  List.fold ~init:[ [] ] ~f:(fun acc s ->
      match (s, acc) with
      | "", acc -> [] :: acc
      | s, hd :: acc ->
          let set = String.to_list s |> Char.Set.of_list in
          (set :: hd) :: acc
      | _ -> failwith "wat")

let all_chars_set =
  String.to_list "abcdefghijklmnopqrstuvwxyz" |> Char.Set.of_list

let () =
  List.map (parse input) ~f:Char.Set.union_list
  |> List.map ~f:Char.Set.length
  |> List.fold ~init:0 ~f:( + )
  |> Printf.printf "part 1: %d\n";

  List.map (parse input) ~f:(fun l ->
      List.fold l ~init:all_chars_set ~f:Char.Set.inter)
  |> List.map ~f:Char.Set.length
  |> List.fold ~init:0 ~f:( + )
  |> Printf.printf "part 1: %d\n"
