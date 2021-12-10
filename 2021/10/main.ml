let read_input () =
  let rec loop acc =
    try loop (input_line stdin :: acc) with End_of_file -> acc
  in
  List.rev @@ loop []

let push c stack = c :: stack

let pop = function c :: tail -> Some (c, tail) | _ -> None

let middle_of_list lst = List.nth lst @@ (List.length lst / 2)

let is_opener = function
  | '(' | '[' | '{' | '<' -> true
  | ')' | ']' | '}' | '>' -> false
  | c -> failwith @@ Printf.sprintf "invalid char: %c" c

let is_closer_for c = function
  | ')' when c = '(' -> true
  | ']' when c = '[' -> true
  | '}' when c = '{' -> true
  | '>' when c = '<' -> true
  | _ -> false

let parse s =
  let rec loop stack = function
    | Seq.Cons (c, next) when is_opener c -> loop (push c stack) (next ())
    | Seq.Cons (c, next) -> (
        match pop stack with
        | Some (c', stack) when is_closer_for c' c -> loop stack (next ())
        | _ -> `Corrupted c)
    | Nil -> ( match stack with [] -> `Complete | stack -> `Missing stack)
  in
  loop [] ((String.to_seq s) ())

let score_invalid = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> failwith "invalid char"

let score_missing =
  List.fold_left
    (fun acc -> function
      | '(' -> (5 * acc) + 1
      | '[' -> (5 * acc) + 2
      | '{' -> (5 * acc) + 3
      | '<' -> (5 * acc) + 4
      | _ -> failwith "error")
    0

let () =
  let input = read_input () in

  input
  |> List.filter_map (fun s ->
         match parse s with `Corrupted c -> Some (score_invalid c) | _ -> None)
  |> List.fold_left ( + ) 0
  |> Printf.printf "Part 1: %d\n";

  input
  |> List.filter_map (fun s ->
         match parse s with
         | `Missing lst -> Some (score_missing lst)
         | _ -> None)
  |> List.sort Int.compare |> middle_of_list
  |> Printf.printf "Part 2: %d\n"
