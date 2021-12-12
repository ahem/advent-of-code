module StrMap = Map.Make (String)

type cave_size = Big | Small

type cave = { size : cave_size; exits : string list }

let add_cave_to_map : string -> string -> cave StrMap.t -> cave StrMap.t =
 fun name exit map ->
  match StrMap.find_opt name map with
  | Some cave -> StrMap.add name { cave with exits = exit :: cave.exits } map
  | None ->
      StrMap.add name
        {
          size = (if name = String.lowercase_ascii name then Small else Big);
          exits = [ exit ];
        }
        map

let read_input () =
  let rec loop acc =
    try loop (Scanf.scanf "%[^-]-%s\n" (fun a b -> (a, b)) :: acc)
    with End_of_file -> acc
  in
  loop []
  |> List.fold_left
       (fun acc (a, b) ->
         let acc = add_cave_to_map a b acc in
         let acc = add_cave_to_map b a acc in
         acc)
       StrMap.empty

let print_map : cave StrMap.t -> unit =
 fun map ->
  StrMap.iter
    (fun name { exits; _ } ->
      Printf.printf "%s: " name;
      List.iter (Printf.printf "%s ") exits;
      Printf.printf "\n")
    map

let rec part1_walk : cave StrMap.t -> string list -> string -> int -> int =
 fun map visited name acc ->
  if name = "end" then acc + 1
  else
    let cave = StrMap.find name map in
    let visited = name :: visited in
    let exits =
      List.filter
        (fun s ->
          match StrMap.find s map with
          | { size = Small; _ } -> not (List.mem s visited)
          | _ -> true)
        cave.exits
    in
    List.fold_left (fun acc s -> part1_walk map visited s acc) acc exits

let () =
  let map = read_input () in
  part1_walk map [] "start" 0 |> Printf.printf "Part 1: %d"
