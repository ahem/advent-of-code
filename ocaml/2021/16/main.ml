Printexc.record_backtrace true

let read_input () =
  input_line stdin |> String.to_seq |> List.of_seq
  |> List.map (function
       | '0' -> [ '0'; '0'; '0'; '0' ]
       | '1' -> [ '0'; '0'; '0'; '1' ]
       | '2' -> [ '0'; '0'; '1'; '0' ]
       | '3' -> [ '0'; '0'; '1'; '1' ]
       | '4' -> [ '0'; '1'; '0'; '0' ]
       | '5' -> [ '0'; '1'; '0'; '1' ]
       | '6' -> [ '0'; '1'; '1'; '0' ]
       | '7' -> [ '0'; '1'; '1'; '1' ]
       | '8' -> [ '1'; '0'; '0'; '0' ]
       | '9' -> [ '1'; '0'; '0'; '1' ]
       | 'A' -> [ '1'; '0'; '1'; '0' ]
       | 'B' -> [ '1'; '0'; '1'; '1' ]
       | 'C' -> [ '1'; '1'; '0'; '0' ]
       | 'D' -> [ '1'; '1'; '0'; '1' ]
       | 'E' -> [ '1'; '1'; '1'; '0' ]
       | 'F' -> [ '1'; '1'; '1'; '1' ]
       | _ -> failwith "invalid char")
  |> List.flatten

module List = struct
  include List

  let rec skip n = function
    | lst when n = 0 -> lst
    | _ :: tail -> skip (n - 1) tail
    | [] -> failwith "cannot skip from empty list"

  let take n lst =
    let rec loop acc n = function
      | lst when n = 0 -> (acc, lst)
      | hd :: tail -> loop (hd :: acc) (n - 1) tail
      | [] -> failwith @@ Printf.sprintf "cannot take %d from empty list" n
    in
    let taken, lst = loop [] n lst in
    (List.rev taken, lst)
end

module BitString = struct
  type t = char list

  let to_int bits =
    List.concat [ [ '0'; 'b' ]; bits ]
    |> List.to_seq |> String.of_seq |> int_of_string

  let take_int number_of_bits lst =
    let bits, rest = List.take number_of_bits lst in
    (to_int bits, rest)
end

type operator = Fold of (int -> int -> int) | Compare of (int -> int -> bool)

type node = LiteralPacket of int | OperatorPacket of operator * packet list

and packet = int * node

let operator_of_type_id = function
  | 0 -> Fold ( + )
  | 1 -> Fold ( * )
  | 2 -> Fold Int.min
  | 3 -> Fold Int.max
  | 5 -> Compare ( > )
  | 6 -> Compare ( < )
  | 7 -> Compare ( = )
  | _ -> failwith "invalid operator"

let rec read_packet : BitString.t -> packet * BitString.t =
 fun lst ->
  let packet_version, lst = BitString.take_int 3 lst in
  let packet_type_id, lst = BitString.take_int 3 lst in
  match packet_type_id with
  | 4 ->
      let n, lst = read_literal lst in
      ((packet_version, LiteralPacket n), lst)
  | n ->
      let operator = operator_of_type_id n in
      let packages, lst = read_operator lst in
      ((packet_version, OperatorPacket (operator, packages)), lst)

and read_literal lst =
  let rec loop acc lst =
    let group, lst = List.take 5 lst in
    match group with
    | '0' :: group ->
        let bits = List.concat @@ List.rev (group :: acc) in
        let n = BitString.to_int bits in
        (n, lst)
    | '1' :: group -> loop (group :: acc) lst
    | _ -> failwith "invalid literal packet"
  in
  loop [] lst

and read_operator = function
  | '0' :: lst ->
      let len, lst = BitString.take_int 15 lst in
      let bits, lst = List.take len lst in
      let rec loop acc bits =
        let packet, bits = read_packet bits in
        if List.length bits < 4 then List.rev (packet :: acc)
        else loop (packet :: acc) bits
      in
      (loop [] bits, lst)
  | '1' :: lst ->
      let len, lst = BitString.take_int 11 lst in
      let rec loop n acc lst =
        let packet, lst = read_packet lst in
        if n = 0 then (List.rev (packet :: acc), lst)
        else loop (n - 1) (packet :: acc) lst
      in
      loop (len - 1) [] lst
  | _ -> failwith "invalid operator packet"

let sum_versions : packet -> int =
  let rec loop acc = function
    | version, LiteralPacket _ -> acc + version
    | version, OperatorPacket (_, packages) ->
        List.fold_left loop (acc + version) packages
  in
  loop 0

let rec eval_packet : packet -> int = function
  | _, LiteralPacket n -> n
  | _, OperatorPacket (Fold op, packages) -> (
      match packages with
      | hd :: tail ->
          List.fold_left
            (fun acc p -> op acc @@ eval_packet p)
            (eval_packet hd) tail
      | _ -> failwith "invalid product packet")
  | _, OperatorPacket (Compare op, packages) -> (
      match packages with
      | [ a; b ] -> if op (eval_packet a) (eval_packet b) then 1 else 0
      | _ -> failwith "invalid equal packet")

let () =
  let lst = read_input () in
  let packet, _ = read_packet lst in

  Printf.printf "Part 1: %d\n" @@ sum_versions packet;
  Printf.printf "Part 2: %d\n" @@ eval_packet packet
