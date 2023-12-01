let read_data () =
  input_line stdin |> String.split_on_char ',' |> List.map int_of_string

let () =
  let input = read_data () in
  let initial =
    Array.init 9 (fun i -> List.length @@ List.filter (Int.equal i) input)
  in
  let rec loop : int array -> int -> int array =
   fun state i ->
    if i = 0 then state
    else
      let next_state =
        Array.mapi
          (fun j _ ->
            match j with
            | 8 -> state.(0)
            | 6 -> state.(0) + state.(7)
            | n -> state.(n + 1))
          state
      in
      loop next_state (i - 1)
  in

  Printf.printf "Part 1: %d\n" @@ Array.fold_left ( + ) 0 @@ loop initial 80;
  Printf.printf "Part 2: %d\n" @@ Array.fold_left ( + ) 0 @@ loop initial 256
