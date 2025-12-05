defmodule Day05 do
  use Application

  def start(_type, _args) do
    Supervisor.start_link([], strategy: :one_for_one)
  end

  def run_part1() do
    IO.puts("Part 1: " <> to_string(part1(File.read!("./input.txt"))))
  end

  def run_part2() do
    IO.puts("Part 2: " <> to_string(part2(File.read!("./input.txt"))))
  end

  def part1(input) do
    [ranges, ids] = input |> String.split("\n\n")
    ranges = process_ranges(ranges)

    ids
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.to_integer/1)
    |> Enum.count(fn id -> Enum.any?(ranges, fn {a, b} -> id >= a and id <= b end) end)
  end

  def part2(input) do
    [ranges | _] = input |> String.split("\n\n")

    process_ranges(ranges)
    |> Enum.sort(&(elem(&1, 0) <= elem(&2, 0)))
    |> Enum.reduce([], fn {a, b}, acc ->
      case acc do
        [] ->
          [{a, b}]

        [{prev_a, prev_b} | tail] ->
          if a <= prev_b and b >= prev_a do
            [merge_ranges({a, b}, {prev_a, prev_b}) | tail]
          else
            [{a, b} | acc]
          end
      end
    end)
    |> Enum.reverse()
    |> Enum.map(fn {a, b} -> b - a + 1 end)
    |> Enum.sum()
  end

  def process_ranges(input) do
    input
    |> String.split("\n")
    |> Enum.map(fn line ->
      [a, b] = line |> String.split("-") |> Enum.map(&String.to_integer/1)
      {a, b}
    end)
  end

  def merge_ranges({a1, a2}, {b1, b2}) do
    {min(a1, b1), max(a2, b2)}
  end
end
