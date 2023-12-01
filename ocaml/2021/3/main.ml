let range start stop =
  Seq.unfold (fun i -> if i < stop then Some (i, i + 1) else None) start

module Bits = struct
  type t = string

  let to_int : t -> int =
   fun bits -> Printf.sprintf "0b%s" bits |> int_of_string

  let of_list : bool list -> t =
   fun seq ->
    List.map (function true -> '1' | false -> '0') seq
    |> List.to_seq |> String.of_seq

  let pos : t -> int -> bool =
   fun bits n ->
    match bits.[n] with
    | '1' -> true
    | '0' -> false
    | _ -> failwith "invalid bit"

  let count_ones : t -> int =
   fun bits ->
    range 0 (String.length bits)
    |> Seq.map (pos bits)
    |> Seq.map Bool.to_int |> Seq.fold_left ( + ) 0

  let most_common_bit : t -> [> `One | `Zero | `Equal ] =
   fun bits ->
    let cnt = count_ones bits |> float_of_int in
    let len = String.length bits in
    let mid = float_of_int len /. 2.0 in
    if cnt > mid then `One else if cnt < mid then `Zero else `Equal
end

module BitGrid = struct
  type t = string list

  let column : t -> int -> Bits.t =
   fun grid n -> List.map (fun bits -> Bits.pos bits n) grid |> Bits.of_list

  let colmuns : t -> Bits.t Seq.t =
   fun grid ->
    let len = String.length @@ List.hd grid in
    range 0 len |> Seq.map (fun n -> column grid n)

  let rec apply_bit_criteria ?(col = 0) ~f bits =
    if List.length bits = 1 then List.hd bits
    else
      let most_common = column bits col |> Bits.most_common_bit in
      List.filter (fun row -> f row col most_common) bits
      |> apply_bit_criteria ~f ~col:(col + 1)
end

let read_lines f =
  let rec loop acc = try loop (input_line f :: acc) with End_of_file -> acc in
  List.rev @@ loop []

let () =
  let bit_grid = read_lines stdin in

  let gamma =
    BitGrid.colmuns bit_grid
    |> Seq.map Bits.most_common_bit
    |> Seq.map (function `One -> '1' | _ -> '0')
    |> String.of_seq |> Bits.to_int
  in

  let epsilon =
    BitGrid.colmuns bit_grid
    |> Seq.map Bits.most_common_bit
    |> Seq.map (function `Zero -> '1' | _ -> '0')
    |> String.of_seq |> Bits.to_int
  in

  Printf.printf "Part 1: gamma: %d, episilon: %d, result: %d\n" gamma epsilon
    (gamma * epsilon);

  let oxygen_generator =
    BitGrid.apply_bit_criteria
      ~f:
        (fun row n -> function
          | `One | `Equal -> Bits.pos row n = true
          | `Zero -> Bits.pos row n = false)
      bit_grid
    |> Bits.to_int
  in

  let co2_scrubber =
    BitGrid.apply_bit_criteria
      ~f:
        (fun row n -> function
          | `One | `Equal -> Bits.pos row n = false
          | `Zero -> Bits.pos row n = true)
      bit_grid
    |> Bits.to_int
  in

  Printf.printf "Part 2: oxygen_generator: %d, co2_scrubber: %d, result: %d\n"
    oxygen_generator co2_scrubber
    (oxygen_generator * co2_scrubber)
