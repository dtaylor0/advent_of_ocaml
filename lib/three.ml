let read_lines ic =
  let rec read acc =
    try
      let line = input_line ic in
      read (line :: acc)
    with End_of_file -> List.rev acc
  in
  read []

let ic = open_in "./data/three.txt"
let lines = read_lines ic
let text = List.fold_left (fun acc line -> acc ^ line) "" lines
let re_instruction = Re.Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"

let rec result acc pos text =
  try
    let _ = Re.Str.search_forward re_instruction text pos in
    let next_pos = Re.Str.match_end () in
    let a = Re.Str.matched_group 1 text in
    let b = Re.Str.matched_group 2 text in
    let next_acc = (int_of_string a * int_of_string b) + acc in
    result next_acc next_pos text
  with Not_found -> acc

let solution_part_1 = result 0 0 text |> string_of_int
