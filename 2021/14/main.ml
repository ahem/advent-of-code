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

let step :
    ((char * char) * (char * char)) PairMap.t -> int PairMap.t -> int PairMap.t
    =
 fun pairs polymer ->
  PairMap.fold
    (fun key n acc ->
      let pair_a, pair_b = PairMap.find key pairs in
      let acc = PairMap.update pair_a (incr n) acc in
      let acc = PairMap.update pair_b (incr n) acc in
      acc)
    polymer polymer

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
  let counts = CharMap.update template.[0] (incr 1) counts in
  let counts =
    CharMap.update template.[String.length template - 1] (incr 1) counts
  in
  counts

let () =
  let template, pairs = read_input () in
  let polymer = polymer_of_string template in
  let polymer =
    Seq.unfold (fun i -> if i < 10 then Some (i, i + 1) else None) 0
    |> Seq.fold_left (fun acc _ -> step pairs acc) polymer
  in
  let quantities = count_chars template polymer in
  CharMap.iter (Printf.printf "  %c: %n\n") quantities

(*
  B: 321208
  C: 259212
  F: 104292
  H: 114194
  K: 361654
  N: 295284
  O: 213592
  P: 139008
  S: 353823
  V: 81597
  *)
