from part_1 import part1
from part_2 import part2

sample_1 = """
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
"""

part1_expected = 5
part1_result = part1(sample_1)
assert part1_result == part1_expected, f"part1: expected {part1_expected}, got {part1_result}"

sample_2 = """
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
"""

part2_expected = 2
part2_result = part2(sample_2)
assert part2_result == part2_expected, f"part2: expected {part2_expected}, got {part2_result}"


print("All tests passed.")