(* Part one *)
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
let text = String.concat "" lines
let re_instruction = Re.Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"

let rec result acc pos text =
  try
    let _ = Re.Str.search_forward re_instruction text pos in
    let next_pos = Re.Str.match_end () in
    let a = text |> Re.Str.matched_group 1 |> int_of_string in
    let b = text |> Re.Str.matched_group 2 |> int_of_string in
    let next_acc = (a * b) + acc in
    result next_acc next_pos text
  with Not_found -> acc

let solution_part_1 = result 0 0 text |> string_of_int

(* Part two *)
let re_do = Re.Str.regexp "do()"
let re_dont = Re.Str.regexp "don't()"

let valid_sections =
  let rec find_valid_sections pos is_enabled =
    match is_enabled with
    | true -> (
        try
          let next_pos = Re.Str.search_forward re_dont text pos in
          let len = next_pos - pos in
          let valid_section = String.sub text pos len in
          valid_section ^ find_valid_sections next_pos (not is_enabled)
        with Not_found ->
          let len = String.length text - pos in
          let valid_section = String.sub text pos len in
          valid_section)
    | false -> (
        try
          let next_pos = Re.Str.search_forward re_do text pos in
          find_valid_sections next_pos (not is_enabled)
        with Not_found -> "")
  in
  find_valid_sections 0 true

let solution_part_2 = result 0 0 valid_sections |> string_of_int
