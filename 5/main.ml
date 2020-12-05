open Core

let input = In_channel.input_lines In_channel.stdin

let map_specifier ~low ~high s =
  String.fold s ~init:[] ~f:(fun acc c ->
      let open Char in
      if c = low then `Low :: acc
      else if c = high then `High :: acc
      else failwith "unexpected specifier")
  |> List.rev

let parse x =
  let row_specifier = String.slice x 0 7 |> map_specifier ~low:'F' ~high:'B' in
  let seat_specifier =
    String.slice x 7 10 |> map_specifier ~low:'L' ~high:'R'
  in
  (row_specifier, seat_specifier)

let rec walk ~min ~max specifier =
  if min = max then min
  else
    let d = min + ((max - min) / 2) in
    match specifier with
    | [] -> failwith "wat!"
    | `Low :: tail -> walk ~min ~max:d tail
    | `High :: tail -> walk ~min:(d + 1) ~max tail

type t = { row : int; col : int; id : int }

let decode_specifier spec =
  let row = walk ~min:0 ~max:127 (fst spec) in
  let col = walk ~min:0 ~max:7 (snd spec) in
  let id = (row * 8) + col in
  { row; col; id }

let rec find_seat = function
  | x :: next :: _ when next - x = 2 -> x + 1
  | _ :: tail -> find_seat tail
  | [] -> failwith "cannot find seat!!"

let () =
  let seats = List.map input ~f:parse |> List.map ~f:decode_specifier in
  let sorted_ids =
    List.map seats ~f:(fun { id; _ } -> id) |> List.sort ~compare:Int.compare
  in
  Printf.printf "part 1 result: %d\n" @@ List.last_exn sorted_ids;
  Printf.printf "part 2 result: %d\n" @@ find_seat sorted_ids
