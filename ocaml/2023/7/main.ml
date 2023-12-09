open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin

let parse : string -> char list * int =
 fun s ->
  match String.split ~on:' ' (String.strip s) with
  | [ hand; bid ] -> (String.to_list hand, Int.of_string bid)
  | _ -> failwith @@ Printf.sprintf "error parsing '%s'" s

let value : order:char list -> char -> int =
 fun ~order c -> List.findi_exn order ~f:(fun _ -> Char.equal c) |> fst

let compare_hands : order:char list -> char list -> char list -> int =
 fun ~order a b ->
  List.zip_exn a b
  |> List.find_map ~f:(fun (a, b) ->
         match Int.compare (value ~order a) (value ~order b) with
         | 0 -> None
         | r -> Some r)
  |> Option.value ~default:0

let strength : char list -> int =
 fun hand ->
  let groups =
    List.sort_and_group ~compare:Char.compare hand
    |> List.sort ~compare:(fun a b ->
           Int.compare (List.length a) (List.length b))
  in
  match groups with
  | [ [ _; _; _; _; _ ] ] -> 6
  | [ [ _ ]; [ _; _; _; _ ] ] -> 5
  | [ [ _; _ ]; [ _; _; _ ] ] -> 4
  | [ [ _ ]; [ _ ]; [ _; _; _ ] ] -> 3
  | [ [ _ ]; [ _; _ ]; [ _; _ ] ] -> 2
  | [ [ _ ]; [ _ ]; [ _ ]; [ _; _ ] ] -> 1
  | [ [ _ ]; [ _ ]; [ _ ]; [ _ ]; [ _ ] ] -> 0
  | _ -> failwith "unreachable"

let unroll_jokers : char list -> char list list =
  List.fold ~init:[] ~f:(fun acc card ->
      let possibilities =
        if Char.equal card 'J' then
          [ 'A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2' ]
        else [ card ]
      in
      if List.is_empty acc then List.map possibilities ~f:(fun x -> [ x ])
      else
        List.fold acc ~init:[] ~f:(fun acc hand ->
            List.map possibilities ~f:(fun p -> List.append hand [ p ])
            |> List.append acc))

let strength_with_joker : char list -> int =
 fun hand ->
  if List.count hand ~f:(Char.equal 'J') > 3 then 6
  else
    unroll_jokers hand |> List.map ~f:strength
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn

let show_with_strength : (int * char list) * int -> string =
 fun ((strength, hand), bid) ->
  Printf.sprintf "%d %s: %d" strength (String.of_char_list hand) bid

let compare_with_strength :
    order:char list -> int * char list -> int * char list -> int =
 fun ~order (strength_a, hand_a) (strength_b, hand_b) ->
  match Int.compare strength_a strength_b with
  | 0 -> compare_hands ~order hand_a hand_b
  | r -> r

let () =
  let parsed_input = List.map input ~f:parse in

  let order =
    [ '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A' ]
  in

  let sorted_with_strength =
    List.map parsed_input ~f:(fun (hand, bid) -> ((strength hand, hand), bid))
    |> List.sort ~compare:(fun (a, _) (b, _) ->
           compare_with_strength ~order a b)
  in

  List.iter sorted_with_strength ~f:(fun x ->
      printf "%s\n" @@ show_with_strength x);

  let part_1_result =
    List.foldi sorted_with_strength ~init:0 ~f:(fun i acc (_, bid) ->
        acc + ((i + 1) * bid))
  in
  printf "part 1: %d\n" part_1_result;

  (* PART 2 *)
  let order =
    [ 'J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A' ]
  in

  let sorted_with_strength =
    List.map parsed_input ~f:(fun (hand, bid) ->
        ((strength_with_joker hand, hand), bid))
    |> List.sort ~compare:(fun (a, _) (b, _) ->
           compare_with_strength ~order a b)
  in

  List.iter sorted_with_strength ~f:(fun x ->
      printf "%s\n" @@ show_with_strength x);

  let part_2_result =
    List.foldi sorted_with_strength ~init:0 ~f:(fun i acc (_, bid) ->
        acc + ((i + 1) * bid))
  in
  printf "part 2: %d\n" part_2_result
