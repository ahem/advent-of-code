open Core

let input = In_channel.input_lines In_channel.stdin

let line_pattern = Re2.create_exn "^(\\w+ \\w+) bags? contain(.*)\\."

let bag_pattern = Re2.create_exn "(\\d+) (\\w+ \\w+) bags?(?:, )?"

let parse : string -> string * (int * string) list =
 fun s ->
  let m = Re2.first_match_exn line_pattern s in
  let color = Re2.Match.get_exn m ~sub:(`Index 1) in
  let contents =
    Re2.Match.get_exn m ~sub:(`Index 2)
    |> Re2.get_matches_exn bag_pattern
    |> List.map ~f:(fun m ->
           let cnt = Re2.Match.get_exn m ~sub:(`Index 1) |> Int.of_string in
           let color = Re2.Match.get_exn m ~sub:(`Index 2) in
           (cnt, color))
  in
  (color, contents)

let all_rules = List.map input ~f:parse |> String.Map.of_alist_exn

let rec can_contain : string -> (int * string) list -> bool =
 fun color rule ->
  List.exists rule ~f:(function
    | _, c when String.equal c color -> true
    | _, c -> can_contain color (String.Map.find_exn all_rules c))

let rec count_contents : (int * string) list -> int =
  List.fold ~init:0 ~f:(fun acc (x, col) ->
      match String.Map.find_exn all_rules col with
      | [] -> acc + x
      | rule -> acc + x + (x * count_contents rule))

let () =
  Printf.printf "part 1 result: %d\n"
  @@ String.Map.count all_rules ~f:(can_contain "shiny gold");

  Printf.printf "part 2 result: %d\n"
  @@ count_contents (String.Map.find_exn all_rules "shiny gold")
