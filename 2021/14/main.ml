module PairMap = Map.Make (struct type t = char * char let compare = compare end) [@@ocamlformat "disable"]

module CharMap = Map.Make (Char)

let read_input () =
  let template =
    Scanf.scanf "%s\n\n" (fun s -> String.to_seq s |> List.of_seq)
  in
  let rec loop acc =
    try loop @@ (Scanf.scanf "%c%c -> %c\n" (fun a b c -> ((a, b), c)) :: acc)
    with End_of_file -> acc
  in
  (template, PairMap.of_seq @@ List.to_seq @@ loop [])

let step : char PairMap.t -> char list -> char list =
 fun pairs polymer ->
  let rec loop acc = function
    | a :: b :: tail ->
        let x = PairMap.find (a, b) pairs in
        loop (x :: a :: acc) (b :: tail)
    | [ b ] -> b :: acc
    | [] -> failwith "unexpected empty list!"
  in
  List.rev @@ loop [] polymer

let count_chars lst =
  let rec loop acc = function
    | c :: tail ->
        let acc =
          CharMap.update c
            (function Some n -> Some (n + 1) | None -> Some 1)
            acc
        in
        loop acc tail
    | [] -> acc
  in
  loop CharMap.empty lst

let () =
  let template, pairs = read_input () in

  let polymer =
    Seq.unfold (fun i -> if i < 10 then Some (i, i + 1) else None) 0
    |> Seq.fold_left (fun acc _ -> step pairs acc) template
  in

  (* Printf.printf "%s\n" (String.of_seq @@ List.to_seq polymer); *)
  let quantities = count_chars polymer in
  CharMap.iter (Printf.printf "  %c: %n\n") quantities;

  let min =
    CharMap.bindings quantities
    |> List.map snd |> List.sort_uniq Int.compare |> List.hd
  in
  let max =
    CharMap.bindings quantities
    |> List.map snd |> List.sort_uniq Int.compare |> List.rev |> List.hd
  in
  Printf.printf "Part 1: %d\n" (max - min)
