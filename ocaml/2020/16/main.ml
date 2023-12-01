open Core

module Rule = struct
  type t = { name : string; low : int * int; high : int * int }

  let pattern = Re2.create_exn "^(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)$"

  let show { name; low; high } =
    Printf.sprintf "%s: %d-%d or %d-%d" name (fst low) (snd low) (fst high)
      (snd high)

  let parse s =
    let m = Re2.first_match_exn pattern s in
    let name = Re2.Match.get_exn m ~sub:(`Index 1) in
    let low =
      ( Re2.Match.get_exn m ~sub:(`Index 2) |> Int.of_string,
        Re2.Match.get_exn m ~sub:(`Index 3) |> Int.of_string )
    in
    let high =
      ( Re2.Match.get_exn m ~sub:(`Index 4) |> Int.of_string,
        Re2.Match.get_exn m ~sub:(`Index 5) |> Int.of_string )
    in
    { name; low; high }

  let matches rule n =
    Int.between n ~low:(fst rule.low) ~high:(snd rule.low)
    || Int.between n ~low:(fst rule.high) ~high:(snd rule.high)

  let matches_all rule = List.for_all ~f:(matches rule)
end

module Ticket = struct
  type t = int list

  let parse s = String.split s ~on:',' |> List.map ~f:Int.of_string

  let non_valid_fields ~rules =
    List.filter ~f:(fun field ->
        not @@ List.exists rules ~f:(fun rule -> Rule.matches rule field))

  let is_valid ~rules t = non_valid_fields ~rules t |> List.is_empty
end

module TicketList = struct
  type t = Ticket.t list

  let parse = List.map ~f:Ticket.parse

  (** fold over the tickets in the lists one field at at time. ~f is called
      repeatedly with (f idx acc lst) where idx is the field number and lst is
      the list of that field from all tickets in the TicketList *)
  let field_foldi : init:'a -> f:(int -> 'a -> int list -> 'a) -> t -> 'a =
   fun ~init ~f t ->
    List.fold t ~init:Int.Map.empty ~f:(fun acc ticket ->
        List.foldi ticket ~init:acc ~f:(fun idx acc x ->
            let data =
              match Int.Map.find acc idx with
              | Some lst -> x :: lst
              | None -> [ x ]
            in
            Int.Map.set acc ~key:idx ~data))
    |> Int.Map.fold ~init ~f:(fun ~key ~data acc -> f key acc data)

  let field_iteri ~f = field_foldi ~init:() ~f:(fun idx () heads -> f idx heads)
end

let is_not_empty s = not @@ String.is_empty s

type t = {
  rules : Rule.t list;
  ticket : int list;
  nearby_tickets : int list list;
}

let parse lines =
  let rule_lines, lines = List.split_while lines ~f:is_not_empty in
  let rules = List.map rule_lines ~f:Rule.parse in
  let ticket, lines =
    match lines with
    | "" :: "your ticket:" :: s :: lines -> (Ticket.parse s, lines)
    | _ -> failwith "cannot parse ticket"
  in
  let nearby_tickets =
    match lines with
    | "" :: "nearby tickets:" :: lines -> TicketList.parse lines
    | _ -> failwith "cannot parse nearby tickets"
  in
  { rules; ticket; nearby_tickets }

let find_fields_names : TicketList.t -> Rule.t list -> (int * string) list =
  let get_valid_rules_for_fields nearby_tickets rules =
    TicketList.field_foldi nearby_tickets ~init:Int.Map.empty
      ~f:(fun field_number acc fields ->
        let matching_rules =
          List.filter rules ~f:(fun rule -> Rule.matches_all rule fields)
          |> List.map ~f:(fun Rule.{ name; _ } -> name)
        in
        Int.Map.set acc ~key:field_number ~data:matching_rules)
  in

  fun nearby_tickets rules ->
    let rec walk acc candidates =
      match
        Int.Map.keys
        @@ Int.Map.filter candidates ~f:(fun lst -> List.length lst = 1)
      with
      | [] -> acc
      | singles ->
          let rules_names =
            List.concat_map singles ~f:(fun field_number ->
                Int.Map.find_exn candidates field_number)
          in
          let candidates =
            Int.Map.map candidates ~f:(fun lst ->
                List.filter lst ~f:(fun n ->
                    List.mem rules_names n ~equal:String.equal |> not))
          in
          let acc = List.concat [ acc; List.zip_exn singles rules_names ] in
          walk acc candidates
    in

    walk [] (get_valid_rules_for_fields nearby_tickets rules)
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)

let () =
  let { nearby_tickets; rules; ticket } =
    In_channel.input_lines In_channel.stdin |> List.map ~f:String.strip |> parse
  in

  Printf.printf "Part 1:\n";
  List.fold nearby_tickets ~init:0 ~f:(fun acc ticket ->
      List.fold (Ticket.non_valid_fields ~rules ticket) ~init:acc ~f:( + ))
  |> Printf.printf "ticket scanning error rate: %d\n\n";

  let nearby_tickets = List.filter nearby_tickets ~f:(Ticket.is_valid ~rules) in

  let fields = find_fields_names nearby_tickets rules in
  List.iter fields ~f:(fun (field_no, name) ->
      Printf.printf "%d: %s\n" field_no name);

  let departure_fields =
    List.filter_map fields ~f:(fun (idx, s) ->
        if String.is_prefix ~prefix:"departure" s then Some idx else None)
  in
  List.foldi ticket ~init:1 ~f:(fun idx acc value ->
      if List.mem ~equal departure_fields idx then acc * value else acc)
  |> Printf.printf "part 2 result: %d\n"
