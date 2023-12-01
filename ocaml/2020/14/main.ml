open Core

let mem_pattern = Re2.create_exn "mem\\[(\\d+)\\] = (\\d+)"

let mask_pattern = Re2.create_exn "mask = ([X10]+)"

type t = Mem of { addr : int; value : int } | Mask of string

let parse_input_line s =
  match Re2.first_match mem_pattern s with
  | Ok m ->
      let addr = Re2.Match.get_exn ~sub:(`Index 1) m |> Int.of_string in
      let value = Re2.Match.get_exn ~sub:(`Index 2) m |> Int.of_string in
      Mem { addr; value }
  | _ ->
      Mask
        (Re2.first_match_exn mask_pattern s |> Re2.Match.get_exn ~sub:(`Index 1))

let input =
  In_channel.input_lines In_channel.stdin |> List.map ~f:parse_input_line

let parse_mask s =
  let and_mask =
    String.substr_replace_all s ~pattern:"X" ~with_:"1"
    |> Printf.sprintf "0b%s" |> Int.of_string
  in
  let or_mask =
    String.substr_replace_all s ~pattern:"X" ~with_:"0"
    |> Printf.sprintf "0b%s" |> Int.of_string
  in
  (and_mask, or_mask)

let unwind_floating : string -> int Sequence.t =
 fun s ->
  let rec walk = function
    | '1' :: tail ->
        Sequence.map (walk tail) ~f:(fun s -> String.concat [ "1"; s ])
    | '0' :: tail ->
        Sequence.map (walk tail) ~f:(fun s -> String.concat [ "0"; s ])
    | 'X' :: tail ->
        Sequence.concat
        @@ Sequence.of_list
             [
               Sequence.map (walk tail) ~f:(fun s -> String.concat [ "0"; s ]);
               Sequence.map (walk tail) ~f:(fun s -> String.concat [ "1"; s ]);
             ]
    | [] -> Sequence.of_list [ "" ]
    | _ -> failwith "wat"
  in
  walk (String.to_list s)
  |> Sequence.map ~f:(fun s ->
         String.concat [ "0b"; s ] |> fun s -> Int.of_string s)

let () =
  let heap, _, _ =
    List.fold input ~init:(Int.Map.empty, 68719476735, 0)
      ~f:(fun (heap, and_mask, or_mask) -> function
      | Mask mask ->
          let and_mask, or_mask = parse_mask mask in
          (heap, and_mask, or_mask)
      | Mem { addr; value } ->
          let value = value land and_mask in
          let value = value lor or_mask in
          let heap = Map.set heap ~key:addr ~data:value in
          (heap, and_mask, or_mask))
  in
  Printf.printf "part 1 result: %d\n"
  @@ List.fold (Int.Map.data heap) ~init:0 ~f:( + );

  let heap, _ =
    List.fold input ~init:(Int.Map.empty, "") ~f:(fun (heap, mask) -> function
      | Mask mask -> (heap, mask)
      | Mem { addr; value } ->
          let inverted_and_mask = lnot @@ fst @@ parse_mask mask in
          let heap =
            Sequence.fold (unwind_floating mask) ~init:heap ~f:(fun heap mask ->
                let addr = mask lor (addr land inverted_and_mask) in
                Int.Map.set heap ~key:addr ~data:value)
          in
          (heap, mask))
  in
  Printf.printf "part 2 result: %d\n"
  @@ List.fold (Int.Map.data heap) ~init:0 ~f:( + )
