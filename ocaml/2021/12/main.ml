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

let visit_small_cave visited cave name =
  if cave.size = Small then
    StrMap.update name
      (function Some v -> Some (v + 1) | None -> Some 1)
      visited
  else visited

let rec walk ~check_small map visited name acc =
  if name = "end" then acc + 1
  else
    let cave = StrMap.find name map in
    let visited = visit_small_cave visited cave name in
    let exits =
      List.filter
        (function
          | "start" -> false
          | s when (StrMap.find s map).size = Big -> true
          | s -> check_small visited s)
        cave.exits
    in
    List.fold_left (fun acc s -> walk ~check_small map visited s acc) acc exits

let () =
  let map = read_input () in

  Printf.printf "Part 1: %d\n"
  @@ walk map StrMap.empty "start" 0 ~check_small:(fun visited s ->
         not (StrMap.mem s visited));

  Printf.printf "Part 2: %d\n"
  @@ walk map StrMap.empty "start" 0 ~check_small:(fun visited s ->
         if StrMap.exists (fun _ v -> v > 1) visited then
           not (StrMap.mem s visited)
         else true)
