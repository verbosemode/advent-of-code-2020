type row = F | B
type column = L | R

let parse_rows_exn s =
  String.sub s 0 7
  |> String.fold_left
       (fun a c ->
         match c with
         | 'F' -> F :: a
         | 'B' -> B :: a
         | _ -> failwith (Printf.sprintf "invalid row value: %c\n%!" c))
       []
  |> List.rev

let find_row l =
  let row, _, _ =
    List.fold_left
      (fun (_, start', end') e ->
        match e with
        | F ->
            let new_end = end' - ((end' - start' + 1) / 2) in
            (new_end, start', new_end)
        | B ->
            let new_start = start' + ((end' - start' + 1) / 2) in
            (new_start, new_start, end'))
      (0, 0, 127) l
  in
  row

let parse_columns_exn s =
  String.sub s 7 3
  |> String.fold_left
       (fun a c ->
         match c with
         | 'L' -> L :: a
         | 'R' -> R :: a
         | _ -> failwith (Printf.sprintf "invalid row value: %c\n%!" c))
       []
  |> List.rev

let find_column l =
  let row, _, _ =
    List.fold_left
      (fun (_, start', end') e ->
        match e with
        | L ->
            let new_end = end' - ((end' - start' + 1) / 2) in
            (new_end, start', new_end)
        | R ->
            let new_start = start' + ((end' - start' + 1) / 2) in
            (new_start, new_start, end'))
      (0, 0, 7) l
  in
  row

let seat_id s =
  let rows = parse_rows_exn s in
  let columns = parse_columns_exn s in
  (find_row rows * 8) + find_column columns

let () =
  let filename = Sys.argv.(1) in
  let part = Sys.argv.(2) in
  match part with
  | part when part = "part1" ->
      CCIO.with_in filename CCIO.read_lines_l
      |> List.fold_left (fun a e -> max (seat_id e) a) 0
      |> Printf.printf "highest seat id: %i\n"
  | _ -> invalid_arg "usage: ./04.exe <filename> <part1|part2>"
