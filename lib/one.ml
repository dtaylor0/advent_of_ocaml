let ic = open_in "./data/one.txt"

let rec read_lines acc ic =
  try
    let line = input_line ic in
    read_lines (line :: acc) ic
  with End_of_file -> List.rev acc

let lines = read_lines [] ic

let get_nums line =
  line
  |> Re.Str.split (Re.Str.regexp "[ \\t]+")
  |> List.filter (fun s -> s <> "")
  |> List.map (fun d -> String.trim d |> Int64.of_string)

let rec insert_sorted element l =
  match l with
  | [] -> [ element ]
  | hd :: tl ->
      if hd >= element then element :: hd :: tl
      else hd :: insert_sorted element tl

let a, b =
  List.fold_left
    (fun acc line ->
      let a, b = acc in
      match get_nums line with
      | [ num_a; num_b ] -> (insert_sorted num_a a, insert_sorted num_b b)
      | _ -> acc)
    ([], []) lines

let magnitude (x : int64) (y : int64) = Int64.sub x y |> Int64.abs

let result =
  List.fold_left2
    (fun acc x y -> magnitude x y |> Int64.add acc)
    (Int64.of_int 0) a b

let solution = Int64.to_string result
