from functools import cache

def parse_line(line: str) -> tuple[str, list[str]]:
    key, values = line.split(": ")
    return key, values.split()

def part1(input: str) -> int:
    input_lines = dict(map(parse_line, input.strip().splitlines()))
    
    @cache
    def get_count(key: str) -> int:
        if input_lines[key] == ['out']:
            return 1
        return sum(get_count(child) for child in input_lines[key])
        
    return get_count('you')

if __name__ == "__main__":
    with open("input.txt") as f:
        input_data = f.read()
    result = part1(input_data)
    print("Part 1:", result)