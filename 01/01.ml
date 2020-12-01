let combine f x l = List.fold_left (fun a e -> f x e :: a) [] l
let tuple (e : int) (l : int list) : (int * int) list =
  combine (fun x e' -> (x, e')) e l

let triple (e : int) (l : (int * int) list) : (int * int * int) list =
  combine (fun x (e1, e2) -> (x, e1, e2)) e l

let build_pairs (l : int list) : (int * int) list =
  CCList.fold_left (fun a e -> tuple e l @ a) [] l

let build_triple (l : int list) : (int * int * int) list =
  CCList.fold_left (fun a e -> triple e (build_pairs l) @ a) [] l

let part1 data =
  data
  |> build_pairs
  |> CCList.find_all (fun (x, y) -> x + y = 2020)
  |> CCList.take 1
  |> CCList.iter (fun (x, y) -> Printf.printf "%i\n" (x * y))

let part2 data =
  data
  |> build_triple
  |> CCList.find_all (fun (x, y, z) -> x + y + z = 2020)
  |> CCList.take 1
  |> CCList.iter (fun (x, y, z) -> Printf.printf "%i\n" (x * y * z))

let usage () = Printf.printf "usage: %s (part1|part2) filename" Sys.argv.(0)

let () =
  if Array.length Sys.argv != 3 then usage () else
  let filename = Sys.argv.(2) in
  let data =
    CCList.map int_of_string (CCIO.with_in filename CCIO.read_lines_l)
  in
  match Sys.argv.(1) with
  | "part1" -> part1 data
  | "part2" -> part2 data 
  | _ -> Printf.printf "usage: %s (part1|part2) filename\n" Sys.argv.(0)
