let rec read_lines ic =
  try input_line ic :: read_lines ic with End_of_file -> List.rev []

let lines = read_lines (open_in "./data/two.txt")
