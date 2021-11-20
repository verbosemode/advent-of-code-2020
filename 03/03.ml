let move steps pos s =
  let max_pos = (String.length s) - 1 in
  let pos = pos + steps in
  if pos > max_pos then pos - max_pos - 1
  else pos

let process_line (i : int) (a : int) (pos : int) (s : string) : (int * int) =
  if i = 0 then (a, pos)
  else
    let pos = move 3 pos s in
    if (String.get s pos) = '#' then (a + 1, pos)
    else (a, pos)

let process_lines ic =
  CCIO.read_lines_seq ic
  |> OSeq.foldi (fun i (a, pos) s -> process_line i a pos s) (0, 0)
  |> fst

let () =
  let filename = Sys.argv.(1) in
  let trees = CCIO.with_in filename process_lines in
  Printf.printf "found %i trees\n%!" trees
