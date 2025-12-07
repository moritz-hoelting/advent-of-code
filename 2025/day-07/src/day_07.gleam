import gleam/dict
import gleam/list
import gleam/int
import gleam/io
import gleam/result
import gleam/set
import gleam/string
import simplifile

pub fn main() -> Nil {
  let input =
    simplifile.read("input.txt")
    |> result.unwrap("")

  let part_1_result = part_1(input)
  io.println(int.to_string(part_1_result))

  let part_2_result = part_2(input)
  io.println(int.to_string(part_2_result))
}

fn get_starting_position(line: String) -> Int {
  string.to_graphemes(line)
  |> list.index_map(fn(a, i) { #(a, i) })
  |> list.find_map(fn(x) {
    let #(a, i) = x
    case a {
      "S" -> Ok(i)
      _ -> Error(Nil)
    }
  })
  |> result.unwrap(-1)
}

pub fn part_1(input: String) -> Int {
  let lines = string.split(input, "\n")
  let beams =
    lines
    |> list.first
    |> result.unwrap("")
    |> get_starting_position
    |> list.wrap
    |> set.from_list

  let #(_, count) =
    lines
    |> list.rest
    |> result.unwrap([])
    |> list.fold(#(beams, 0), fn(acc, line) {
      let #(beams, count) = acc
      let splitters =
        string.to_graphemes(line)
        |> list.index_map(fn(a, i) { #(a, i) })
        |> list.filter_map(fn(x) {
          let #(a, i) = x
          let is_contained = set.contains(beams, i)
          case a {
            "^" if is_contained -> Ok(i)
            _ -> Error(Nil)
          }
        })
        |> set.from_list

      let new_beams =
        set.to_list(splitters)
        |> list.flat_map(fn(s) { [s - 1, s + 1] })
        |> list.filter(fn(s) { s >= 0 })
        |> set.from_list

      let merged = beams |> set.difference(splitters) |> set.union(new_beams)

      #(merged, count + set.size(splitters))
    })
  count
}

pub fn part_2(input: String) -> Int {
  let lines = string.split(input, "\n")
  let beams =
    lines
    |> list.first
    |> result.unwrap("")
    |> get_starting_position
    |> list.wrap
    |> list.map(fn(i) { #(i, 1) })
    |> dict.from_list

  let after_splits =
    lines
    |> list.rest
    |> result.unwrap([])
    |> list.fold(beams, fn(beams, line) {
      let splitters =
        string.to_graphemes(line)
        |> list.index_map(fn(a, i) { #(a, i) })
        |> list.filter_map(fn(x) {
          let #(a, i) = x
          let count = dict.get(beams, i)
          case a, count {
            "^", Ok(c) ->
              Ok(dict.from_list([#(i, -c), #(i - 1, c), #(i + 1, c)]))
            _, _ -> Error(Nil)
          }
        })
        |> list.reduce(fn(a, b) { dict.combine(a, b, fn(av, bv) { av + bv }) })
        |> result.unwrap(dict.new())

      dict.combine(beams, splitters, fn(a, b) { a + b })
    })

  after_splits
  |> dict.values
  |> list.reduce(fn(a, b) { a + b })
  |> result.unwrap(0)
}

pub fn part_2_(input: String) -> Int {
  let lines = string.split(input, "\n")
  let starting_pos =
    lines
    |> list.first
    |> result.unwrap("")
    |> get_starting_position

  part_2_rec(lines, starting_pos)
}

fn part_2_rec(lines: List(String), beam: Int) -> Int {
  let first = list.first(lines)
  case first {
    Error(_) -> 1
    Ok(line) -> {
      case string.slice(line, beam, 1) {
        "^" ->
          part_2_rec(list.rest(lines) |> result.unwrap([]), beam - 1)
          + part_2_rec(list.rest(lines) |> result.unwrap([]), beam + 1)
        _ -> part_2_rec(list.rest(lines) |> result.unwrap([]), beam)
      }
    }
  }
}
