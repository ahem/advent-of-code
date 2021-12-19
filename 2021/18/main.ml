let read_lines () =
  let rec loop acc =
    try loop (input_line stdin :: acc) with End_of_file -> acc
  in
  List.rev @@ loop []

type node = { open_braces : int; value : int; closing_braces : int }

let parse s =
  let empty_node = { open_braces = 0; value = 0; closing_braces = 0 } in
  String.to_seq s
  |> Seq.fold_left
       (fun (acc, n) -> function
         | '[' -> (acc, { n with open_braces = n.open_braces + 1 })
         | ']' -> (acc, { n with closing_braces = n.closing_braces + 1 })
         | ',' -> (n :: acc, empty_node)
         | c -> (acc, { n with value = int_of_string @@ Printf.sprintf "%c" c }))
       ([], empty_node)
  |> fun (acc, n) -> n :: acc |> List.rev

let repeat_char c n =
  let rec loop s = function
    | 0 -> s
    | n -> loop (Printf.sprintf "%s%c" s c) (n - 1)
  in
  loop "" n

let string_of_node n =
  String.concat ""
    [
      repeat_char '[' n.open_braces;
      string_of_int n.value;
      repeat_char ']' n.closing_braces;
    ]

let print_node_list lst =
  List.map string_of_node lst |> String.concat "," |> Printf.printf "%s\n"

let () =
  let input = read_lines () |> List.map parse in
  Printf.printf "lines: %d\n" @@ List.length input;

  List.iter print_node_list input;
  (* Printf.printf "%s\n" (reduce "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"); *)
  ()
