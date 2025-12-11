from functools import cache
from part_1 import parse_line

def part2(input: str) -> int:
    input_lines = dict(map(parse_line, input.strip().splitlines()))
    
    @cache
    def get_count(key: str, dac: bool, fft: bool) -> int:
        if key == 'dac':
            dac = True
        if key == 'fft':
            fft = True
        if input_lines[key] == ['out']:
            return 1 if dac and fft else 0
        return sum(get_count(child, dac, fft) for child in input_lines[key])
        
    return get_count('svr', dac=False, fft=False)


if __name__ == "__main__":
    with open("input.txt") as f:
        input_data = f.read()
    result = part2(input_data)
    print("Part 2:", result)