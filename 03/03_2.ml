let move steps pos s =
  let max_pos = (String.length s) - 1 in
  let pos = pos + steps in
  if pos > max_pos then pos - max_pos - 1
  else pos

let process_line steps skip_f (i : int) (a : int) (pos : int) (s : string) : (int * int) =
  if skip_f i then (a, pos)
  else
    let pos = move steps pos s in
    if Char.equal (String.get s pos) '#'
    then (a + 1, pos)
    else (a, pos)

let process_lines ic skip_f steps =
  CCIO.read_lines_seq ic
  |> OSeq.foldi (fun i (a, pos) s -> process_line steps skip_f i a pos s) (0, 0)
  |> fst

let run_slope filename steps skip_f =
  CCIO.with_in filename (fun ic -> process_lines ic skip_f steps)

let () =
  let slopes = [
    1, (fun i -> i = 0);
    3, (fun i -> i = 0);
    5, (fun i -> i = 0);
    7, (fun i -> i = 0);
    1, (fun i -> i = 0 || i mod 2 <> 0);
  ] in
  let filename = Sys.argv.(1) in
  let trees =
    slopes
    |> List.map (fun (steps, skip_f) -> run_slope filename steps skip_f)
    |> List.fold_left ( * ) 1
  in
  Printf.printf "found %i trees\n%!" trees
