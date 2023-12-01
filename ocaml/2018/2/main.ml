open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:String.strip

module CharMap = Map.M (Char)

let count_chars : string -> int Map.M(Char).t =
 fun s ->
  String.fold s
    ~init:(Map.empty (module Char))
    ~f:(Map.update ~f:(fun v -> Option.value ~default:0 v + 1))

let diff_char_count : string -> string -> int =
 fun a b ->
  String.foldi a ~init:0 ~f:(fun i acc c ->
      match Char.equal c b.[i] with false -> acc + 1 | true -> acc)

let rec find_boxes : string list -> (string * string) option = function
  | hd :: tail -> (
      match List.find tail ~f:(fun s -> diff_char_count hd s = 1) with
      | Some s -> Some (hd, s)
      | None -> find_boxes tail)
  | _ -> None

let common_letters : string -> string -> string =
 fun a b ->
  String.foldi a ~init:"" ~f:(fun i acc c ->
      match Char.equal c b.[i] with
      | true -> String.append acc @@ String.of_char c
      | false -> acc)

let () =
  let character_counts = List.map ~f:count_chars input in
  let two_of_any_letter_cnt =
    List.count character_counts ~f:(Map.exists ~f:(fun x -> x = 2))
  in
  let three_of_any_letter_cnt =
    List.count character_counts ~f:(Map.exists ~f:(fun x -> x = 3))
  in
  printf "Lines with two of any letter: %d\n" two_of_any_letter_cnt;
  printf "Lines with three of any letter: %d\n" three_of_any_letter_cnt;
  printf "Part 1: %d\n" @@ (two_of_any_letter_cnt * three_of_any_letter_cnt);

  let a, b = find_boxes input |> Option.value_exn in
  printf "%s %s\n" a b;
  printf "Part 2: %s\n" @@ common_letters a b
