(*
move step if row is max_row 3
else move step + line_width 34
*)

let find_width filename =
  CCIO.with_in filename CCIO.read_line |> Option.get |> CCString.length

let string_of_string_list = CCList.fold_left (fun a e -> a ^ e) ""

let step_fold_list steps row_len max_rows s =
  let s_len = CCString.length s in
  let rec aux a i row =
    if i >= s_len then a
    else
      let step_width, row =
        if row = max_rows then steps, 1 else (steps + row_len), (row + 1)
      in
      Printf.printf "step_width: %i row: %i\n" step_width row;
      aux (CCString.get s i :: a) (i + step_width) row
  in
  aux [] 0 0
 
let calc_max_rows steps width =
  let steps = Float.of_int steps in
  let width = Float.of_int width in
  width /. steps
  |> CCFloat.round
  |> CCInt.of_float
 
(* let step_indices steps width max_len = *)
(*   let rec aux a i = *)
(*     if i > max_len then a *)
(*     (1* else aux ([i; i + 1; i + 3] @ a) (i + steps + width) *1) *)
(*     else aux (i :: a) (i + steps + width) *)
(*   in *)
(*   aux [] 0 *)
(* let step_indices steps width max_len = *)
(*   let rec aux a i = *)
(*     if i > max_len then a *)
(*     (1* else aux ([i; i + 1; i + 3] @ a) (i + steps + width) *1) *)
(*     else aux (i :: a) (i + steps + width) *)
(*   in *)
(*   aux [] 0 *)

(* let set_rev i rows rev = *)
(*   let change_rev = i mod rows = 0 in *)
(*   match change_rev, rev with *)
(*   | true, false -> true *)
(*   | true, true -> false *)
(*   | false, _ -> rev *)

(* let rec reverse_fold i rows rev a l = *)
(*   let rev = set_rev i rows rev in *)
(*   match l with *)
(*   | [] -> a *)
(*   | h::[] -> *)
(*       if rev then (CCString.rev h) :: a else h :: a *)
(*   | h::t -> if rev then reverse_fold (i + 1) rows rev (CCString.rev h :: a) t *)
(*             else reverse_fold (i + 1) rows rev (h :: a) t *)

let () =
  let filename = Sys.argv.(1) in
  let steps = 3 in
  let row_width = find_width filename in
  let max_rows = calc_max_rows steps row_width in
  Printf.printf "max rows %i\n" max_rows;
  let lines = CCIO.with_in filename CCIO.read_lines_l in
  let s = string_of_string_list lines in
  let l = step_fold_list steps row_width max_rows s in
  List.iter (fun i -> Printf.printf "%c\n" i) l;
  let trees = CCList.filter (fun e -> e = '#') l in
  print_int (CCList.length trees)
