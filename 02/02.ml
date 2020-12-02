exception Shit_hit_the_fan

module Part1 = struct
  module Password_file = struct
    open Angstrom
    open Angstrom.Let_syntax

    type t = {
      min_len: int;
      max_len: int;
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
      let* min_len = p_digit <* char '-' in
      let* max_len = p_digit <* char ' ' in
      let* tchar = p_alpha <* string ": " in
      let* password = p_password in
      return (min_len, max_len, tchar, password)

    let parse_line s =
      match Angstrom.parse_string ~consume:Consume.All p s with
      | Ok (min_len, max_len, tchar, password) ->
          (match CCInt.of_string min_len, CCInt.of_string max_len with
           | None, _ | _, None -> Error "Int conversion of min_len / max_len failed"
           | Some min_len, Some max_len -> Ok {min_len; max_len; tchar; password}
          )
      | Error e -> Error e
  end

  let verify_password (p : Password_file.t) =
    let l = CCString.to_list p.password |> CCList.filter (fun c -> c = p.tchar) in
    CCList.length l >= p.min_len && CCList.length l <= p.max_len

  let verify_line s =
    match Password_file.parse_line s with
    | Error _ -> raise Shit_hit_the_fan
    | Ok password_entry -> verify_password password_entry

  let start filename =
    CCList.map verify_line (CCIO.with_in filename CCIO.read_lines_l)
    |> CCList.filter (fun e -> e = true)
    |> CCList.length
    |> Printf.printf "%i\n"
end

module Part2 = struct
  module Password_file = struct
    open Angstrom
    open Angstrom.Let_syntax

    type t = {
      pos1: int;
      pos2: int;
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
      let* pos1 = p_digit <* char '-' in
      let* pos2 = p_digit <* char ' ' in
      let* tchar = p_alpha <* string ": " in
      let* password = p_password in
      return (pos1, pos2, tchar, password)

    let parse_line s =
      match Angstrom.parse_string ~consume:Consume.All p s with
      | Ok (pos1, pos2, tchar, password) ->
          (match CCInt.of_string pos1, CCInt.of_string pos2 with
           | None, _ | _, None -> Error "Int conversion of min_len / max_len failed"
           | Some pos1, Some pos2 -> Ok {pos1; pos2; tchar; password}
          )
      | Error e -> Error e
  end

  let verify_password (p : Password_file.t) =
    let l = CCString.to_list p.password in
    if CCList.length l < p.pos2 then false
    else
    (CCList.nth l (p.pos1 - 1) = p.tchar &&
    CCList.nth l (p.pos2 - 1) != p.tchar) ||
    (CCList.nth l (p.pos1 - 1) != p.tchar &&
    CCList.nth l (p.pos2 - 1) = p.tchar)

  let verify_line s =
    match Password_file.parse_line s with
    | Error _ -> raise Shit_hit_the_fan
    | Ok password_entry -> verify_password password_entry

  let start filename =
    CCList.map verify_line (CCIO.with_in filename CCIO.read_lines_l)
    |> CCList.filter (fun e -> e = true)
    |> CCList.length
    |> Printf.printf "%i\n"
end

let usage () = Printf.printf "usage: %s (part1|part2) filename" Sys.argv.(0)

let () =
  if Array.length Sys.argv != 3 then usage () else
  let filename = Sys.argv.(2) in
  match Sys.argv.(1) with
  | "part1" -> Part1.start filename
  | "part2" -> Part2.start filename
  | _ -> Printf.printf "usage: %s (part1|part2) filename\n" Sys.argv.(0)
