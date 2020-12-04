(*

1. Move forward
2. At bounds?
  - yes -> Reverse string
  - no


2. fold string into one line


*)

let find_width filename =
  CCIO.with_in filename CCIO.read_line |> Option.get |> CCString.length

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


let set_rev i rows rev =
  let change_rev = i mod rows = 0 in
  match change_rev, rev with
  | true, false -> true
  | true, true -> false
  | false, _ -> rev

let rec reverse_fold i rows rev a l =
  let rev = set_rev i rows rev in
  match l with
  | [] -> a
  | h::[] ->
      if rev then (CCString.rev h) :: a else h :: a
  | h::t -> if rev then reverse_fold (i + 1) rows rev (CCString.rev h :: a) t
            else reverse_fold (i + 1) rows rev (h :: a) t

let string_of_string_list = CCList.fold_left (fun a e -> a ^ e) ""

let () =
  let filename = Sys.argv.(1) in
  let steps = 3 in
  let width = find_width filename in
  let max_rows = calc_max_rows steps width in
  let lines = CCIO.with_in filename CCIO.read_lines_l in
  let lines = reverse_fold 1 max_rows false [] lines in
  let lines = CCList.rev lines in
  let s = string_of_string_list lines in
  print_string s

  (* let max_len = width * max_rows in *)
  (* let indices = step_indices steps width max_len in *)
  (* let s = CCString.replace ~sub:"\n" ~by:"" s in *)
  (* Printf.printf "%s\n" s; *)
  (* List.iter (fun i -> Printf.printf "%i\n" i) indices *)
  (* CCString.find_all_l ~sub:"#" s *)
  (* |> CCList.length *)
  (* |> print_int *)
