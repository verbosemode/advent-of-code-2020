module String_map = Map.Make (String)

let required_fields_present m =
  List.for_all
    (fun e -> String_map.exists (fun k _ -> String.compare e k = 0) m)
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

let is_digit = function '0' .. '9' -> true | _ -> false
let is_hex = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false

let validate_string_int_range lower upper s =
  match int_of_string_opt s with
  | Some i when i >= lower && i <= upper -> true
  | _ -> false

let is_valid_byr = validate_string_int_range 1920 2002
let is_valid_iyr = validate_string_int_range 2010 2020
let is_valid_eyr = validate_string_int_range 2020 2030

let is_valid_hgt s =
  if String.length s >= 4 then
    let digits = String.sub s 0 (String.length s - 2) in
    let last_char1 = String.get s (String.length s - 2) in
    let last_char2 = String.get s (String.length s - 1) in
    match (last_char1, last_char2, String.for_all is_digit digits) with
    | 'c', 'm', true -> validate_string_int_range 150 193 digits
    | 'i', 'n', true -> validate_string_int_range 59 76 digits
    | _ -> false
  else false

let is_valid_hcl s =
  String.length s = 7
  && Char.equal (String.get s 0) '#'
  && String.for_all is_hex (String.sub s 1 6)

let is_valid_ecl s =
  String.length s = 3
  && List.exists
       (fun e -> String.equal e s)
       [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

let is_valid_pid s = String.length s = 9 && String.for_all is_digit s

let validate_field_exn m k v =
  match String_map.find_opt k m with
  | Some f -> f v
  | None ->
      failwith
        "ensure all required fields do exist in a passport prior to coming here"

let field_validators =
  String_map.empty
  |> String_map.add "byr" is_valid_byr
  |> String_map.add "iyr" is_valid_iyr
  |> String_map.add "eyr" is_valid_eyr
  |> String_map.add "hgt" is_valid_hgt
  |> String_map.add "hcl" is_valid_hcl
  |> String_map.add "ecl" is_valid_ecl
  |> String_map.add "pid" is_valid_pid
  |> String_map.add "cid" (fun _ -> true)

let validate_field_data validators m =
  String_map.for_all (fun k v -> validate_field_exn validators k v) m

let parse_data l =
  let rec aux current a l =
    match l with
    | [] -> current :: a
    | h :: t ->
        if String.length h = 0 then aux String_map.empty (current :: a) t
        else
          aux
            (List.fold_left
               (fun a e ->
                 let l = String.split_on_char ':' e in
                 String_map.add (List.nth l 0) (List.nth l 1) a)
               current
               (String.split_on_char ' ' h))
            a t
  in
  aux String_map.empty [] l

let () =
  let filename = Sys.argv.(1) in
  let part = Sys.argv.(2) in
  match part with
  | part when part = "part1" ->
      CCIO.with_in filename CCIO.read_lines_l
      |> parse_data
      |> List.filter required_fields_present
      |> List.length
      |> Printf.printf "Valid passports: %i\n"
  | part when part = "part2" ->
      CCIO.with_in filename CCIO.read_lines_l
      |> parse_data
      |> List.filter required_fields_present
      |> List.filter (fun e -> validate_field_data field_validators e)
      |> List.length
      |> Printf.printf "Valid passports: %i\n"
  | _ -> invalid_arg "usage: ./04.exe <filename> <part1|part2>"
