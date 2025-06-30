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
  |> List.map (fun d -> String.trim d |> int_of_string)

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

let magnitude (x : int) (y : int) = Int.sub x y |> Int.abs

let result_part_1 =
  List.fold_left2 (fun acc x y -> magnitude x y |> Int.add acc) 0 a b

let solution_part_1 = Int.to_string result_part_1

let count_occurrences element lst =
  List.fold_left
    (fun acc n -> if Int.equal n element then Int.add acc 1 else acc)
    0 lst

let result_part_2 =
  List.fold_left
    (fun acc x -> Int.mul x (count_occurrences x b) |> Int.add acc)
    0 a

let solution_part_2 = Int.to_string result_part_2
