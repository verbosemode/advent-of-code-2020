(*

1. fold string into one line


*)



let find_width filename =
  CCIO.with_in filename CCIO.read_line |> Option.get |> CCString.length

let calc_max_rows steps width =
  let steps = Float.of_int steps in
  let width = Float.of_int (width + 1) in
  width /. steps
  |> CCFloat.round
  |> CCInt.of_float

let step_indices steps width max_len =
  let rec aux a i =
    if i >= max_len then a
    else aux (i :: a) (i + steps + width)
  in
  aux [] 0

let () =
  let filename = Sys.argv.(1) in
  let steps = 3 in
  let width = find_width filename in
  let max_rows = calc_max_rows steps width in
  let max_len = width * max_rows in
  let indices = step_indices steps width max_len in
  let s = CCIO.with_in filename CCIO.read_all in
  let s = CCString.replace ~sub:"\n" ~by:"" s in
  (* let s = CCList.fold_left (fun a i -> CCString.of_char (CCString.get s i) ^ a) "" indices in *)
  CCList.iter (fun i -> Printf.printf "%c\n" (CCString.get s i)) indices
  (* CCString.find_all_l ~sub:"#" s *)
  (* |> CCList.length *)
  (* |> print_int *)
