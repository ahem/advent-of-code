open Core

let input =
  let open List in
  In_channel.input_lines In_channel.stdin
  >>| Re2.first_match_exn @@ Re2.create_exn "^(\\d+)-(\\d+) (\\w): (.*)$"
  >>| fun m ->
  ( Re2.Match.get_exn m ~sub:(`Index 1) |> Int.of_string,
    Re2.Match.get_exn m ~sub:(`Index 2) |> Int.of_string,
    Re2.Match.get_exn m ~sub:(`Index 3) |> Char.of_string,
    Re2.Match.get_exn m ~sub:(`Index 4) )

let hest = "hest"

let () =
  List.count input ~f:(fun (low, high, c, str) ->
      let cnt = String.count ~f:(Char.equal c) str in
      Int.between ~low ~high cnt)
  |> Printf.printf "part 1 result: %d\n";

  List.count input ~f:(fun (a, b, c, str) ->
      let open Bool in
      Char.equal str.[a - 1] c <> Char.equal str.[b - 1] c)
  |> Printf.printf "part 2 result: %d\n"
