module String_map = Map.Make (String)

let validate_passport m =
  List.for_all
    (fun e -> String_map.exists (fun k _ -> String.compare e k = 0) m)
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

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
  CCIO.with_in filename CCIO.read_lines_l
  |> parse_data
  |> List.filter validate_passport
  |> List.length
  |> Printf.printf "Valid passports: %i\n"
