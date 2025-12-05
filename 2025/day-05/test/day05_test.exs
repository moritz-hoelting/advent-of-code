defmodule Day05Test do
  use ExUnit.Case
  doctest Day05

  @input """
  3-5
  10-14
  16-20
  12-18

  1
  5
  8
  11
  17
  32
  """

  test "part1" do
    assert Day05.part1(@input) == 3
  end

  test "part2" do
    assert Day05.part2(@input) == 14
  end
end
