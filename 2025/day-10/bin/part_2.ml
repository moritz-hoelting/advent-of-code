let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let () =
  let input = read_file "input.txt" in
  let res = Day_10.part2 input in
  Printf.printf "Part 2: %d\n" res
