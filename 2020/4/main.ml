open Core

let input = In_channel.input_lines In_channel.stdin

let parse list =
  List.fold list ~init:[] ~f:(fun acc s ->
      let open String in
      match (acc, s) with
      | [], s -> [ s ]
      | head :: tail, s when s = "" -> "" :: head :: tail
      | head :: tail, s ->
          (String.concat [ head; " "; s ] |> String.strip) :: tail)
  |> List.map ~f:(fun s ->
         String.split s ~on:' '
         |> List.map ~f:(String.split ~on:':')
         |> List.map ~f:(fun x -> (List.hd_exn x, List.last_exn x))
         |> String.Map.of_alist_exn)

let validate_part1 : string String.Map.t -> bool =
 fun item ->
  List.for_all ~f:(String.Map.mem item)
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

let height_regex = Re2.create_exn "([0-9]+)(cm|in)"

let color_regex = Re2.create_exn "^#[0-9a-f]{6}$"

let eye_color_regex = Re2.create_exn "^(amb|blu|brn|gry|grn|hzl|oth)$"

let password_id_regex = Re2.create_exn "^[0-9]{9}$"

let parse_height s =
  match Re2.first_match height_regex s with
  | Ok m -> (
      let n = Int.of_string @@ Re2.Match.get_exn ~sub:(`Index 1) m in
      match Re2.Match.get_exn m ~sub:(`Index 2) with
      | "cm" -> Some (`Cm n)
      | _ -> Some (`Inch n) )
  | _ -> None

let validate_part2 item =
  (let open Option.Let_syntax in
  let open Re2.Infix in
  let%bind birth_year = String.Map.find item "byr" >>| Int.of_string in
  let%bind issue_year = String.Map.find item "iyr" >>| Int.of_string in
  let%bind expiration_year = String.Map.find item "eyr" >>| Int.of_string in
  let%bind height = String.Map.find item "hgt" >>= parse_height in
  let%bind hair_color = String.Map.find item "hcl" in
  let%bind eye_color = String.Map.find item "ecl" in
  let%bind password_id = String.Map.find item "pid" in
  let valid =
    Int.between birth_year ~low:1920 ~high:2002
    && Int.between issue_year ~low:2010 ~high:2020
    && Int.between expiration_year ~low:2020 ~high:2030
    && ( match height with
       | `Cm h -> Int.between h ~low:150 ~high:193
       | `Inch h -> Int.between h ~low:59 ~high:76 )
    && hair_color =~ color_regex
    && eye_color =~ eye_color_regex
    && password_id =~ password_id_regex
  in
  return valid)
  |> Option.value ~default:false

let () =
  let data = parse input in
  Printf.printf "part 1: %d\n" @@ List.count data ~f:validate_part1;
  Printf.printf "part 2: %d\n" @@ List.count data ~f:validate_part2
