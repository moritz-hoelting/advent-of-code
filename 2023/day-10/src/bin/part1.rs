use std::iter;

use itertools::Itertools;

fn main() {
    println!("{}", part1(include_str!("./input.txt")));
}

fn part1(input: &str) -> usize {
    let pipes = input
        .lines()
        .enumerate()
        .map(|(y, line)| {
            line.char_indices()
                .map(|(x, c)| Pipe::new(x, y, c.try_into().expect("invalid pipe character")))
                .collect()
        })
        .collect::<Vec<Vec<_>>>();

    let starting_pipe = pipes
        .iter()
        .flatten()
        .find(|p| p.pipe_type == PipeType::Starting)
        .expect("no starting pipe found");

    let mut starting = Vec::with_capacity(2);
    if starting_pipe.pipe_type.has_pos_x() {
        if let Some(pipe) = starting_pipe.get_pos_x(&pipes) {
            starting.push((Direction::Left, pipe));
        }
    }
    if starting_pipe.pipe_type.has_neg_x() {
        if let Some(pipe) = starting_pipe.get_neg_x(&pipes) {
            starting.push((Direction::Right, pipe));
        }
    }
    if starting_pipe.pipe_type.has_pos_y() {
        if let Some(pipe) = starting_pipe.get_pos_y(&pipes) {
            starting.push((Direction::Up, pipe));
        }
    }
    if starting_pipe.pipe_type.has_neg_y() {
        if let Some(pipe) = starting_pipe.get_neg_y(&pipes) {
            starting.push((Direction::Down, pipe));
        }
    }

    let (path_a, path_b) = starting
        .into_iter()
        .map(|p| iter::successors(Some(p), |(d, p)| p.successor(*d, &pipes)))
        .collect_tuple()
        .expect("more than 2 paths");

    path_a
        .zip(path_b)
        .take_while(|((_, pipe_a), (_, pipe_b))| pipe_a != pipe_b)
        .count()
        + 1
}

#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
enum Direction {
    Left,
    Right,
    Up,
    Down,
}

#[derive(Debug, PartialEq, Clone, Eq, Hash, Copy)]
struct Pipe {
    x: usize,
    y: usize,
    pipe_type: PipeType,
}
impl Pipe {
    fn new(x: usize, y: usize, pipe_type: PipeType) -> Self {
        Self { x, y, pipe_type }
    }

    fn successor<'a>(
        &self,
        entry_direction: Direction,
        pipes: &'a [Vec<Pipe>],
    ) -> Option<(Direction, &'a Self)> {
        if entry_direction != Direction::Right && self.pipe_type.has_pos_x() {
            self.get_pos_x(pipes).map(|p| (Direction::Left, p))
        } else if entry_direction != Direction::Left && self.pipe_type.has_neg_x() {
            self.get_neg_x(pipes).map(|p| (Direction::Right, p))
        } else if entry_direction != Direction::Up && self.pipe_type.has_neg_y() {
            self.get_neg_y(pipes).map(|p| (Direction::Down, p))
        } else if entry_direction != Direction::Down && self.pipe_type.has_pos_y() {
            self.get_pos_y(pipes).map(|p| (Direction::Up, p))
        } else {
            None
        }
    }

    fn get_pos_x<'a>(&self, pipes: &'a [Vec<Pipe>]) -> Option<&'a Pipe> {
        if self.pipe_type.has_pos_x() {
            pipes
                .get(self.y)?
                .get(self.x + 1)
                .filter(|p| p.pipe_type.has_neg_x())
        } else {
            None
        }
    }
    fn get_neg_x<'a>(&self, pipes: &'a [Vec<Pipe>]) -> Option<&'a Pipe> {
        if self.pipe_type.has_neg_x() {
            pipes
                .get(self.y)?
                .get(self.x.checked_sub(1)?)
                .filter(|p| p.pipe_type.has_pos_x())
        } else {
            None
        }
    }
    fn get_pos_y<'a>(&self, pipes: &'a [Vec<Pipe>]) -> Option<&'a Pipe> {
        if self.pipe_type.has_pos_y() {
            pipes
                .get(self.y + 1)?
                .get(self.x)
                .filter(|p| p.pipe_type.has_neg_y())
        } else {
            None
        }
    }
    fn get_neg_y<'a>(&self, pipes: &'a [Vec<Pipe>]) -> Option<&'a Pipe> {
        if self.pipe_type.has_neg_y() {
            pipes
                .get(self.y.checked_sub(1)?)?
                .get(self.x)
                .filter(|p| p.pipe_type.has_pos_y())
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
enum PipeType {
    Empty,
    Starting,
    Horizontal,
    Vertical,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
}

impl PipeType {
    fn has_pos_x(&self) -> bool {
        matches!(
            self,
            Self::Horizontal | Self::NorthEast | Self::SouthEast | Self::Starting
        )
    }
    fn has_neg_x(&self) -> bool {
        matches!(
            self,
            Self::Horizontal | Self::NorthWest | Self::SouthWest | Self::Starting
        )
    }
    fn has_pos_y(&self) -> bool {
        matches!(
            self,
            Self::Vertical | Self::SouthEast | Self::SouthWest | Self::Starting
        )
    }
    fn has_neg_y(&self) -> bool {
        matches!(
            self,
            Self::Vertical | Self::NorthEast | Self::NorthWest | Self::Starting
        )
    }
}

impl TryFrom<char> for PipeType {
    type Error = ();

    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '.' => Ok(Self::Empty),
            'S' => Ok(Self::Starting),
            '-' => Ok(Self::Horizontal),
            '|' => Ok(Self::Vertical),
            'L' => Ok(Self::NorthEast),
            'J' => Ok(Self::NorthWest),
            '7' => Ok(Self::SouthWest),
            'F' => Ok(Self::SouthEast),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_part1() {
        assert_eq!(
            part1(indoc!(
                "
                ..F7.
                .FJ|.
                SJ.L7
                |F--J
                LJ...
                "
            )),
            8
        );
    }
}
