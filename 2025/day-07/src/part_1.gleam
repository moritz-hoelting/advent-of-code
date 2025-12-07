import day_07
import gleam/int
import gleam/io
import gleam/result
import simplifile

pub fn main() -> Nil {
  let input =
    simplifile.read("input.txt")
    |> result.unwrap("")
  let part_1_result = day_07.part_1(input)
  io.println(int.to_string(part_1_result))
}
