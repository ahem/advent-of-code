open Core

let input = In_channel.input_all In_channel.stdin |> String.strip

let pix layers n =
  Sequence.of_list layers
  |> Sequence.map ~f:(fun layer -> layer.[n])
  |> Sequence.drop_while ~f:(Char.equal '2')
  |> Sequence.hd_exn

let count layer c = String.count layer ~f:(Char.equal c)

let () =
  let width = 25 and height = 6 in
  let layers =
    String.to_list input
    |> List.groupi ~break:(fun i _ _ -> i % (width * height) = 0)
    |> List.map ~f:String.of_char_list
  in
  let layer_with_least_zero =
    List.min_elt layers ~compare:(fun a b ->
        Int.compare (count a '0') (count b '0'))
  in
  Option.iter layer_with_least_zero ~f:(fun layer ->
      Printf.printf "part 1 result: %d\n" @@ (count layer '1' * count layer '2'));

  List.range 0 (width * height)
  |> List.map ~f:(pix layers)
  |> List.groupi ~break:(fun i _ _ -> i % width = 0)
  |> List.map ~f:String.of_char_list
  |> List.map ~f:(String.map ~f:(fun c -> if Char.equal c '0' then ' ' else c))
  |> List.iter ~f:(Printf.printf "%s\n")
