module PairMap = Map.Make (struct type t = char * char let compare = compare end) [@@ocamlformat "disable"]

module CharMap = Map.Make (Char)

let read_input () =
  let template = Scanf.scanf "%s\n\n" (fun s -> s) in
  let rec loop acc =
    try
      loop
      @@ Scanf.scanf "%c%c -> %c\n" (fun a b c ->
             let key = (a, b) in
             let value = ((a, c), (c, b)) in
             (key, value) :: acc)
    with End_of_file -> acc
  in
  (template, PairMap.of_seq @@ List.to_seq @@ loop [])

let incr n = function Some v -> Some (v + n) | None -> Some n

let polymer_of_string s =
  let rec loop acc = function
    | a :: b :: tail -> loop (PairMap.update (a, b) (incr 1) acc) (b :: tail)
    | [ _ ] -> acc
    | [] -> failwith "reached end of list!"
  in
  loop PairMap.empty (List.of_seq @@ String.to_seq s)

let step pairs polymer =
  PairMap.fold
    (fun key n acc ->
      let pair_a, pair_b = PairMap.find key pairs in
      let acc = PairMap.update pair_a (incr n) acc in
      let acc = PairMap.update pair_b (incr n) acc in
      acc)
    polymer PairMap.empty

let last_char s = s.[String.length s - 1]

let count_chars : string -> int PairMap.t -> int CharMap.t =
 fun template polymer ->
  let counts =
    PairMap.fold
      (fun (a, b) cnt acc ->
        let acc = CharMap.update a (incr cnt) acc in
        let acc = CharMap.update b (incr cnt) acc in
        acc)
      polymer CharMap.empty
  in
  (* since pairs are overlapping, each letter is counted twice *)
  let counts = CharMap.map (fun n -> n / 2) counts in
  (* except the first and last in the template *)
  let counts = CharMap.update template.[0] (incr 1) counts in
  let counts = CharMap.update (last_char template) (incr 1) counts in
  counts

let range n = Seq.unfold (fun i -> if i < n then Some (i, i + 1) else None) 0

let score template polymer =
  let quantities = count_chars template polymer in
  let sorted =
    quantities |> CharMap.bindings |> List.map snd |> List.sort_uniq Int.compare
  in
  let min = List.hd sorted in
  let max = List.hd @@ List.rev sorted in
  max - min

let () =
  let template, pairs = read_input () in

  let polymer =
    range 10
    |> Seq.fold_left (fun acc _ -> step pairs acc) (polymer_of_string template)
  in
  Printf.printf "Part 1: %d\n" @@ score template polymer;

  let polymer =
    range 40
    |> Seq.fold_left (fun acc _ -> step pairs acc) (polymer_of_string template)
  in
  Printf.printf "Part 2: %d\n" @@ score template polymer
