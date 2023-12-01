module Point = struct type t = int * int let compare = compare end [@@ocamlformat "disable"]

module PointSet = Set.Make (Point)

module PointMap = struct
  include Map.Make (Point)

  let adjacents : Point.t -> 'a t -> (Point.t * 'a) list =
   fun (x, y) map ->
    [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
    |> List.filter_map (fun (dx, dy) ->
           let k = (x + dx, y + dy) in
           find_opt k map |> Option.map (fun v -> (k, v)))
end

module AStar : sig
  (* find_path map start dst -> (score, path) *)
  val find_path : int PointMap.t -> Point.t -> Point.t -> int * Point.t list
end = struct
  let trace : Point.t PointMap.t -> Point.t -> Point.t list =
   fun map p ->
    let rec loop path p =
      let parent = PointMap.find p map in
      if parent = p then List.rev path else loop (p :: path) parent
    in
    List.rev @@ loop [] p

  let find_most_promising_path ~h ~open_fields =
    PointMap.bindings open_fields
    |> List.sort (fun (a, ag) (b, bg) -> Int.compare (ag + h a) (bg + h b))
    |> List.hd

  let get_successors ~map ~closed_fields p =
    PointMap.adjacents p map
    |> List.filter (fun (q, _) -> not (PointSet.mem q closed_fields))

  let rec search :
      h:(Point.t -> int) ->
      map:int PointMap.t ->
      closed_fields:PointSet.t ->
      open_fields:int PointMap.t ->
      parent_fields:Point.t PointMap.t ->
      dst:Point.t ->
      int * Point.t list =
   fun ~h ~map ~closed_fields ~open_fields ~parent_fields ~dst ->
    (* A* Algoritm:

       maintain three maps:
          closed_fields: fields that have been looked and processed
          open_fields: fields that might lead to final destination. Key is point, and
             value is the accumulated score following the best know path to this field
          parent_fields: keep track of parent fields so we can trace back
             the final path

        for each iteration, find the open field closest to destination by score + h(p). The 'h'
        function is a guess of how far there is to destination.

        with the found open field, get any adjacents fields that have not been visited (as per
        closed_fields list) and add them to the path's collected in open_fields. If the field
        is already in the open_fields list, then compare the accumlated path score and replace
        if the currently considered adjacent point's path is lower
    *)
    let p, score = find_most_promising_path ~h ~open_fields in
    let open_fields = PointMap.remove p open_fields in
    let successors = get_successors ~map ~closed_fields p in

    (* update all successors parent fields to be P *)
    let parent_fields =
      List.fold_left
        (fun acc (succ, _) -> PointMap.add succ p acc)
        parent_fields successors
    in

    (* for each successor, update open fields *)
    let open_fields =
      successors
      |> List.fold_left
           (fun open_fields (successor, successor_score) ->
             let score = successor_score + score in
             PointMap.update successor
               (function
                 | Some path_score ->
                     if score < path_score then Some score else Some path_score
                 | None -> Some score)
               open_fields)
           open_fields
    in

    match PointMap.find_opt dst open_fields with
    | Some score ->
        (* reached destination, return score and trace path back *)
        (score, trace parent_fields dst)
    | None ->
        (* destination not reached, add P to closed list, and keep searching *)
        let closed_fields = PointSet.add p closed_fields in
        search ~h ~map ~closed_fields ~open_fields ~parent_fields ~dst

  let find_path map start dst =
    let open_fields = PointMap.add start 0 PointMap.empty in
    let parent_fields = PointMap.add start start PointMap.empty in
    let closed_fields = PointSet.empty in
    search
      ~h:(fun (x, y) -> x + y)
      ~map ~closed_fields ~parent_fields ~open_fields ~dst
end

let read_input () =
  let seq_of_line y line =
    String.to_seqi line
    |> Seq.map (fun (x, c) -> (x, y, int_of_char c - Char.code '0'))
  in
  Seq.unfold
    (fun y ->
      try Some (seq_of_line y (input_line stdin), y + 1)
      with End_of_file -> None)
    0
  |> Seq.concat
  |> Seq.fold_left
       (fun grid (x, y, v) -> PointMap.add (x, y) v grid)
       PointMap.empty

let range n = Seq.unfold (fun i -> if i < n then Some (i, i + 1) else None) 0

let extend_map map =
  let (max_x, max_y), _ = PointMap.max_binding map in
  let max_x, max_y = (max_x + 1, max_y + 1) in
  range (max_y * 5)
  |> Seq.fold_left
       (fun acc y ->
         range (max_x * 5)
         |> Seq.fold_left
              (fun acc x ->
                let d = (x / max_x) + (y / max_y) in
                let v = d + PointMap.find (x mod max_x, y mod max_y) map in
                let v = if v < 10 then v else v - 9 in
                PointMap.add (x, y) v acc)
              acc)
       PointMap.empty

let print_map map =
  let (max_x, max_y), _ = PointMap.max_binding map in
  for y = 0 to max_y do
    for x = 0 to max_x do
      Printf.printf "%d" @@ PointMap.find (x, y) map
    done;
    Printf.printf "\n"
  done

let () =
  Printexc.record_backtrace true;

  let map = read_input () in
  let score, path =
    AStar.find_path map (0, 0) (fst @@ PointMap.max_binding map)
  in

  Printf.printf "Part 1: Score: %d (path length: %d)\n" score (List.length path);
  flush stdout;

  let map = extend_map map in

  let score, path =
    AStar.find_path map (0, 0) (fst @@ PointMap.max_binding map)
  in
  Printf.printf "Part 2: Score: %d, (path length: %d)\n" score
    (List.length path)
