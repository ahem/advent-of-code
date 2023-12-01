open Core

let input =
  In_channel.input_all In_channel.stdin
  |> String.strip |> String.split ~on:',' |> List.map ~f:Int.of_string

type t = { turn : int; value : int; prev : int Int.Map.t }

let init =
  List.foldi ~init:(0, Int.Map.empty) ~f:(fun turn (_, acc) x ->
      match Int.Map.find acc x with
      | None -> (0, Int.Map.set acc ~key:x ~data:turn)
      | Some last -> (turn - last, Int.Map.set acc ~key:x ~data:turn))

let game initial =
  let open Sequence.Generator in
  let rec step ~turn state x =
    match Int.Map.find state x with
    | None ->
        yield 0 >>= fun () ->
        step (Int.Map.set state ~key:x ~data:turn) ~turn:(turn + 1) 0
    | Some last ->
        let n = turn - last in
        yield n >>= fun () ->
        step (Int.Map.set state ~key:x ~data:turn) ~turn:(turn + 1) n
  in

  let turn = List.length initial in
  let value, state = init initial in

  let initial_seq = Sequence.of_list initial in
  let seq = run @@ step state ~turn value in
  Sequence.concat
  @@ Sequence.of_list [ initial_seq; Sequence.of_list [ value ]; seq ]

let () =
  Printf.printf "part 1 result: %d\n" @@ Sequence.nth_exn (game input) (2020 - 1);
  Out_channel.flush Out_channel.stdout;
  Printf.printf "part 2 result: %d\n"
  @@ Sequence.nth_exn (game input) (30000000 - 1)
