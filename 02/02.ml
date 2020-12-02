exception Shit_hit_the_fan

module Password_file = struct
  open Angstrom
  open Angstrom.Let_syntax

  type t = {
    digit1: int;
    digit2: int;
    tchar: char;
    password: string;
  }

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

  let is_alpha = function
    | 'a' .. 'z' -> true
    | _ -> false

  let p_digit = take_while1 is_digit
  let p_alpha = satisfy is_alpha
  let p_password = take_while1 is_alpha

  let p =
    let* digit1 = p_digit <* char '-' in
    let* digit2 = p_digit <* char ' ' in
    let* tchar = p_alpha <* string ": " in
    let* password = p_password in
    return (digit1, digit2, tchar, password)

  let parse_line s =
    match Angstrom.parse_string ~consume:Consume.All p s with
    | Ok (digit1, digit2, tchar, password) ->
        (match CCInt.of_string digit1, CCInt.of_string digit2 with
         | None, _ | _, None -> Error "Int conversion of digit1 / digit2 failed"
         | Some digit1, Some digit2 -> Ok {digit1; digit2; tchar; password}
        )
    | Error e -> Error e
end

module Passwords = struct
  let verify_line f s =
    match Password_file.parse_line s with
    | Error _ -> raise Shit_hit_the_fan
    | Ok password_entry -> f password_entry

  let count_valid_passwords f filename =
    CCList.map (verify_line f) (CCIO.with_in filename CCIO.read_lines_l)
    |> CCList.filter (fun e -> e = true)
    |> CCList.length
    |> Printf.printf "%i\n"
end

module Part1 = struct
  let verify_password (p : Password_file.t) =
    let l = CCString.to_list p.password |> CCList.filter (fun c -> c = p.tchar) in
    let min_len = p.digit1 in
    let max_len = p.digit2 in
    CCList.length l >= min_len && CCList.length l <= max_len
end

module Part2 = struct
  let verify_password (p : Password_file.t) =
    let l = CCString.to_list p.password in
    if CCList.length l < p.digit2 then false else
    let char1 = CCList.nth l (p.digit1 - 1) in
    let char2 = CCList.nth l (p.digit2 - 1) in
    char1 = p.tchar && char2 != p.tchar ||
    char1 != p.tchar && char2 = p.tchar
end

let usage () = Printf.printf "usage: %s (part1|part2) filename" Sys.argv.(0)

let () =
  if Array.length Sys.argv != 3 then usage () else
  let filename = Sys.argv.(2) in
  match Sys.argv.(1) with
  | "part1" -> Passwords.count_valid_passwords Part1.verify_password filename
  | "part2" -> Passwords.count_valid_passwords Part2.verify_password filename
  | _ -> Printf.printf "usage: %s (part1|part2) filename\n" Sys.argv.(0)
