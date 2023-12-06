open Base
open Stdio

let input = In_channel.input_lines In_channel.stdin

type race = { time : int; distance : int } [@@deriving show]

let digit_regex = Re2.of_string "\\d+"

let parse_numbers_from_str : string -> int list =
 fun s ->
  Re2.find_all digit_regex s |> Or_error.ok_exn |> List.map ~f:Int.of_string

let parse : string list -> race list = function
  | [ time_str; distance_str ] ->
      let times = parse_numbers_from_str time_str in
      let distances = parse_numbers_from_str distance_str in
      List.zip_exn times distances
      |> List.map ~f:(fun (time, distance) -> { time; distance })
  | _ -> failwith "parse error"

(*
    distance = (time - n) * n    =>   (n * time) - (n * n) = distance    =>   -1 * (n * n) + time * n - distance = 0

    quadratic equation forumla:
    ax^2 + bx + c = 0, roots: (-b ± sqrt(b^2 - 4ac)) / 2a

    a = 1, b = time, c = -distance

    roots = (-time ± sqrt(time * time - 4 * (-1) * (-distance))) / 2 * -1
          = (-time ± sqrt(time * time - (4 * distance))) / -2

*)
let solve : race -> float * float =
 fun { time; distance } ->
  let time = Float.of_int time in
  let distance = Float.of_int distance in
  let open Float in
  ( (-time + sqrt ((time * time) - (4.0 * distance))) / -2.0,
    (-time - sqrt ((time * time) - (4.0 * distance))) / -2.0 )

let () =
  let races = parse input in
  List.map races ~f:show_race |> List.iter ~f:(printf "%s\n");
  List.map races ~f:solve |> List.iter ~f:(fun (a, b) -> printf "%f %f\n" a b);
  List.map races ~f:solve
  |> List.map ~f:(fun (a, b) ->
         let a = Int.of_float (Float.round ~dir:`Down a) in
         let b = Int.of_float (Float.round ~dir:`Up b) in
         printf "  %d %d\n" a b;
         (a, b))
  |> List.fold ~init:1 ~f:(fun acc (a, b) -> acc * (b - a - 1))
  |> printf "Part 1: %d\n";

  let races =
    List.map input ~f:(Re2.replace_exn (Re2.of_string " ") ~f:(fun _ -> ""))
    |> parse
  in
  List.map races ~f:show_race |> List.iter ~f:(printf "%s\n");
  List.map races ~f:solve |> List.iter ~f:(fun (a, b) -> printf "%f %f\n" a b);
  List.map races ~f:solve
  |> List.map ~f:(fun (a, b) ->
         let a = Int.of_float (Float.round ~dir:`Down a) in
         let b = Int.of_float (Float.round ~dir:`Up b) in
         printf "  %d %d\n" a b;
         (a, b))
  |> List.fold ~init:1 ~f:(fun acc (a, b) -> acc * (b - a - 1))
  |> printf "Part 2: %d\n"
