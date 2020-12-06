open Core

let input =
  In_channel.input_all In_channel.stdin
  |> String.strip
  |> String.split_on_chars ~on:[ '-' ]
  |> List.map ~f:Int.of_string
  |> fun l -> (List.hd_exn l, List.last_exn l)

let rec digits_of ?(acc = []) n =
  let d = n % 10 in
  let n = n / 10 in
  let acc = d :: acc in
  if n > 0 then digits_of n ~acc else acc

let rec check ?(found_adjacent = false) = function
  | x :: y :: _ when x > y -> false
  | x :: y :: tail when x = y -> check ~found_adjacent:true (y :: tail)
  | _ :: y :: tail -> check ~found_adjacent (y :: tail)
  | _ -> found_adjacent

let rec strict_adjacent_check : int list -> bool = function
  | hd :: tail ->
      let f = Int.equal hd in
      if 1 = List.length @@ List.take_while tail ~f then true
      else strict_adjacent_check @@ List.drop_while tail ~f
  | _ -> false

let () =
  let seq =
    Sequence.range (fst input) (snd input)
    |> Sequence.map ~f:digits_of |> Sequence.filter ~f:check
  in

  Printf.printf "part 1: %d\n" @@ Sequence.length seq;
  Printf.printf "part 2: %d\n" @@ Sequence.count seq ~f:strict_adjacent_check
