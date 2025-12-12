use day_12::{Shape, parse_input};
use itertools::Itertools;

fn main() {
    let input = include_str!("../input.txt");
    println!("Part 1: {}", part1(input));
}

fn part1(input: &str) -> usize {
    let (shapes, regions) = parse_input(input).unwrap();

    regions
        .into_iter()
        .filter(|((x, y), counts)| can_fit((*x, *y), counts, &shapes))
        .count()
}

fn placement_masks(shape: &Shape, (width, height): (u8, u8)) -> Vec<u64> {
    shape
        .all_orientations()
        .into_iter()
        .flat_map(|shape| {
            let max_x = *shape.cells.iter().map(|(x, _)| x).max().unwrap();
            let max_y = *shape.cells.iter().map(|(_, y)| y).max().unwrap();

            let cells = shape.cells;

            (0..(height - max_y)).flat_map(move |dy| {
                (0..(width - max_x))
                    .map(|dx| {
                        let mut mask = 0;

                        for (x, y) in cells.iter().copied() {
                            let bit_index = ((y + dy) as u64) * (width as u64) + ((x + dx) as u64);
                            mask |= 1 << bit_index;
                        }

                        mask
                    })
                    .collect::<Vec<_>>()
            })
        })
        .collect()
}

fn can_fit((width, height): (u8, u8), counts: &[u8], shapes: &[Shape]) -> bool {
    let region_area: usize = (width as usize) * (height as usize);

    let total_area = counts
        .iter()
        .enumerate()
        .map(|(shape_id, &count)| {
            let shape = &shapes[shape_id];
            shape.cells.len() * (count as usize)
        })
        .sum::<usize>();

    if total_area > region_area {
        return false;
    }

    let simple_fit_area = counts
        .iter()
        .enumerate()
        .map(|(shape_id, &count)| {
            let shape = &shapes[shape_id];
            let shape_width = shape
                .cells
                .iter()
                .map(|(x, _)| *x)
                .max()
                .map(|x| x + 1)
                .unwrap_or(0);
            let shape_height = shape
                .cells
                .iter()
                .map(|(_, y)| *y)
                .max()
                .map(|y| y + 1)
                .unwrap_or(0);

            (shape_width as usize) * (shape_height as usize) * (count as usize)
        })
        .sum::<usize>();

    if simple_fit_area <= region_area {
        return true;
    }

    let pieces = counts
        .iter()
        .enumerate()
        .flat_map(|(shape_id, &count)| {
            let masks = placement_masks(&shapes[shape_id], (width, height));

            (0..count).map(move |_| masks.clone())
        })
        .sorted_by(|a, b| a.len().cmp(&b.len()))
        .collect::<Vec<_>>();

    backtrack(&pieces, 0, 0)
}

fn backtrack(pieces: &[Vec<u64>], idx: usize, occupied: u64) -> bool {
    if idx == pieces.len() {
        return true;
    }

    for &mask in &pieces[idx] {
        if occupied & mask == 0 && backtrack(pieces, idx + 1, occupied | mask) {
            return true;
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::part1;
    use day_12::SAMPLE;
    #[test]
    fn test_part1() {
        let result = part1(SAMPLE);
        assert_eq!(result, 2);
    }
}
