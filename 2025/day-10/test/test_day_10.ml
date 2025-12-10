let assert_equal expected actual =
  if expected <> actual then begin
    Printf.eprintf "Test failed!\nExpected: %d\nGot: %d\n" expected actual;
    exit 1
  end

let sample =
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
   [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
   [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

let () =
  assert_equal 7 (Day_10.part1 (String.split_on_char '\n' sample));
  assert_equal 33 (Day_10.part2 (String.split_on_char '\n' sample));
  print_endline "All tests passed!"
