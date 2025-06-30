let read_lines ic =
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file -> acc
  in
  loop []

let lines = read_lines (open_in "./data/two.txt")

let reports =
  lines
  |> List.map (fun s -> String.split_on_char ' ' s |> List.map int_of_string)

let rec increasing prev lst =
  match lst with
  | [] -> true
  | hd :: tl -> if hd < prev then false else increasing hd tl

let rec decreasing prev lst =
  match lst with
  | [] -> true
  | hd :: tl -> if hd > prev then false else decreasing hd tl

let rec safe_deltas prev lst =
  match lst with
  | [] -> true
  | hd :: tl ->
      if
        let delta = Int.abs (hd - prev) in
        delta < 1 || delta > 3
      then false
      else safe_deltas hd tl

let safe_report report =
  match report with
  | [] -> true
  | hd :: tl -> (increasing hd tl || decreasing hd tl) && safe_deltas hd tl

let result_part_1 =
  List.fold_left
    (fun acc report -> if safe_report report then acc + 1 else acc)
    0 reports

let solution_part_1 = string_of_int result_part_1
