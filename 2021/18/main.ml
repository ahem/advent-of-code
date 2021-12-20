let read_lines () =
  let rec loop acc =
    try loop (input_line stdin :: acc) with End_of_file -> acc
  in
  List.rev @@ loop []

let repeat_char c n =
  let rec loop s = function
    | 0 -> s
    | n -> loop (Printf.sprintf "%s%c" s c) (n - 1)
  in
  loop "" n

let permutations lst =
  let rec loop acc = function
    | hd :: rest ->
        let acc = List.map (fun x -> (hd, x)) rest :: acc in
        loop acc rest
    | [] -> acc
  in
  loop [] lst |> List.concat

module SnailFishNumber = struct
  type node = { open_cnt : int; value : int; close_cnt : int }

  let empty_node = { open_cnt = 0; value = 0; close_cnt = 0 }

  let of_string s =
    let empty_node = { open_cnt = 0; value = 0; close_cnt = 0 } in
    String.to_seq s
    |> Seq.fold_left
         (fun (acc, n) -> function
           | '[' -> (acc, { n with open_cnt = n.open_cnt + 1 })
           | ']' -> (acc, { n with close_cnt = n.close_cnt + 1 })
           | ',' -> (n :: acc, empty_node)
           | c ->
               (acc, { n with value = int_of_string @@ Printf.sprintf "%c" c }))
         ([], empty_node)
    |> fun (acc, n) -> n :: acc |> List.rev

  let to_string lst =
    let node_to_string n =
      String.concat ""
        [
          repeat_char '[' n.open_cnt;
          string_of_int n.value;
          repeat_char ']' n.close_cnt;
        ]
    in
    List.map node_to_string lst |> String.concat "," |> Printf.sprintf "%s\n"

  let reduce node_list =
    let rec explode depth acc = function
      | left :: right :: rst when depth + left.open_cnt > 4 ->
          let node =
            {
              value = 0;
              open_cnt = left.open_cnt - 1;
              close_cnt = right.close_cnt - 1;
            }
          in
          let prev, acc =
            match acc with
            | n :: acc -> (Some { n with value = n.value + left.value }, acc)
            | [] -> (None, [])
          in
          let next, rst =
            match rst with
            | n :: rst -> (Some { n with value = n.value + right.value }, rst)
            | [] -> (None, [])
          in
          let exploded =
            List.filter_map (fun x -> x) [ prev; Some node; next ]
          in
          let lst = List.concat [ List.rev acc; exploded; rst ] in
          explode 0 [] lst
      | node :: rst ->
          let depth = depth + node.open_cnt - node.close_cnt in
          explode depth (node :: acc) rst
      | [] -> split [] @@ List.rev acc
    and split acc = function
      | { value; open_cnt; close_cnt } :: rst when value > 9 ->
          let left = value / 2 in
          let right = value - left in
          let left_node =
            { empty_node with open_cnt = open_cnt + 1; value = left }
          in
          let right_node =
            { empty_node with close_cnt = close_cnt + 1; value = right }
          in
          let lst =
            List.concat [ List.rev acc; left_node :: right_node :: rst ]
          in
          explode 0 [] lst
      | node :: rst -> split (node :: acc) rst
      | [] -> List.rev acc
    in
    explode 0 [] node_list

  let add left right =
    let left =
      match left with
      | n :: rst -> { n with open_cnt = n.open_cnt + 1 } :: rst
      | _ -> failwith "invalid add"
    in
    let right =
      match List.rev right with
      | n :: rst -> List.rev @@ ({ n with close_cnt = n.close_cnt + 1 } :: rst)
      | _ -> failwith "invalid add"
    in
    reduce @@ List.concat [ left; right ]

  let magnitude node_list =
    let rec loop acc = function
      | left :: right :: rst when left.close_cnt = 0 && right.open_cnt = 0 ->
          let node =
            {
              value = (3 * left.value) + (2 * right.value);
              open_cnt = left.open_cnt - 1;
              close_cnt = right.close_cnt - 1;
            }
          in
          loop [] @@ List.concat [ List.rev acc; node :: rst ]
      | [ node ] -> node.value
      | node :: rst -> loop (node :: acc) rst
      | [] -> failwith "invalid magnitude"
    in
    loop [] node_list
end

let () =
  let input = read_lines () |> List.map SnailFishNumber.of_string in
  Printf.printf "lines: %d\n" @@ List.length input;

  Printf.printf "Part 1:\n  Sum: ";
  let sum =
    List.fold_left SnailFishNumber.add (List.hd input) (List.tl input)
  in
  Printf.printf "%s" @@ SnailFishNumber.to_string sum;
  Printf.printf "  Magnitude: %d\n" @@ SnailFishNumber.magnitude sum;

  let part_2_result =
    permutations input
    |> List.fold_left
         (fun acc (a, b) ->
           let ab = SnailFishNumber.magnitude @@ SnailFishNumber.add a b in
           let ba = SnailFishNumber.magnitude @@ SnailFishNumber.add b a in
           Int.max acc (Int.max ab ba))
         0
  in
  Printf.printf "Part 2: %d\n" part_2_result
