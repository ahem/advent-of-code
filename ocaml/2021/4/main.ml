module Bingo = struct
  type t = (int * bool) list list

  let init : int list list -> t = List.map (List.map (fun x -> (x, false)))

  let apply_number : t -> int -> t =
   fun board n ->
    board
    |> List.map @@ List.map (fun (x, v) -> if x = n then (x, true) else (x, v))

  let column : t -> int -> (int * bool) list =
   fun board n -> List.map (fun row -> List.nth row n) board

  let check_rows : t -> bool =
   fun board -> List.find_opt (List.for_all snd) board |> Option.is_some

  let check_columns : t -> bool =
   fun board ->
    Seq.unfold (fun i -> if i < 5 then Some (column board i, i + 1) else None) 0
    |> Seq.filter (List.for_all snd)
    |> fun seq -> seq () != Nil

  let check : t -> bool = fun board -> check_rows board || check_columns board

  let unmarked_numbers : t -> int list =
   fun board -> List.flatten board |> List.filter snd |> List.map fst
end

let read_numbers : unit -> int list =
 fun () ->
  input_line stdin |> String.split_on_char ',' |> List.map int_of_string

let read_boards : unit -> Bingo.t list =
 fun () ->
  let pack_as_list a b c d e = [ a; b; c; d; e ] in
  let rec loop acc =
    try
      let board =
        Bingo.init
          [
            Scanf.scanf " %d %d %d %d %d " pack_as_list;
            Scanf.scanf " %d %d %d %d %d " pack_as_list;
            Scanf.scanf " %d %d %d %d %d " pack_as_list;
            Scanf.scanf " %d %d %d %d %d " pack_as_list;
            Scanf.scanf " %d %d %d %d %d " pack_as_list;
          ]
      in
      loop (board :: acc)
    with End_of_file -> acc
  in
  List.rev @@ loop []

let play_round : Bingo.t list -> int -> Bingo.t list * Bingo.t list =
 fun boards n ->
  let boards = List.map (fun board -> Bingo.apply_number board n) boards in
  let winners, rest = List.partition Bingo.check boards in
  (winners, rest)

let rec find_first_winner : Bingo.t list -> int list -> Bingo.t * int =
 fun boards -> function
  | n :: remaining_numbers -> (
      match play_round boards n with
      | [ winner ], _ -> (winner, n)
      | [], boards -> find_first_winner boards remaining_numbers
      | _ -> failwith "draw")
  | _ -> failwith "no winner found"

let rec find_last_winner : Bingo.t list -> int list -> Bingo.t * int =
 fun boards -> function
  | n :: remaining_numbers -> (
      match play_round boards n with
      | [ bingo ], [] -> (bingo, n)
      | _, boards -> find_last_winner boards remaining_numbers)
  | _ -> failwith "no single last winner found"

let score : Bingo.t -> int -> int =
 fun board last_number ->
  last_number * List.fold_left ( + ) 0 (Bingo.unmarked_numbers board)

let () =
  let numbers = read_numbers () in
  let boards = read_boards () in
  Printf.printf "numbers: %d, boards: %d\n" (List.length numbers)
    (List.length boards);

  let winner_board, last_number = find_first_winner boards numbers in
  Printf.printf "Part 1 score: %d\n" @@ score winner_board last_number;

  let loser_board, last_number = find_last_winner boards numbers in
  Printf.printf "Part 2 score: %d" @@ score loser_board last_number
