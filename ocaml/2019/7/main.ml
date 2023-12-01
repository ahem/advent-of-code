open Core
open Async
open Deferred.Let_syntax

let input =
  In_channel.input_all In_channel.stdin
  |> String.strip
  |> String.split_on_chars ~on:[ ',' ]
  |> List.map ~f:Int.of_string |> Array.of_list

let rec permutations l =
  let n = List.length l in
  if n = 1 then [ l ]
  else
    let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t
    in
    let rec aux k =
      let e = List.nth_exn l k in
      let subperms = permutations (sub e l) in
      let t = List.map ~f:(fun a -> e :: a) subperms in
      if k < n - 1 then List.rev_append t (aux (k + 1)) else t
    in
    aux 0

let main () =
  let make_amp = Amp.init input in

  let%bind () =
    permutations [ 0; 1; 2; 3; 4 ]
    |> Deferred.List.map ~f:(fun phases -> make_amp phases >>= Amp.run)
    >>| List.max_elt ~compare
    >>| Option.iter ~f:(Print.printf "part 1 result: %d\n")
  in

  let%bind () =
    permutations [ 5; 6; 7; 8; 9 ]
    |> Deferred.List.map ~f:(fun phases -> make_amp phases >>= Amp.feedback)
    >>| List.max_elt ~compare
    >>| Option.iter ~f:(Print.printf "part 2 result: %d\n")
  in

  exit 0

let () =
  don't_wait_for @@ main ();
  never_returns (Scheduler.go ())
