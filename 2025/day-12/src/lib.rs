use std::collections::BTreeSet;

use nom::{
    Parser,
    bytes::complete::{is_a, tag},
    character::complete::{self, line_ending},
    multi::{many1, separated_list1},
    sequence::{pair, separated_pair, terminated},
};

type GridSize = (u8, u8);
type Counts = Vec<u8>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Shape {
    pub cells: BTreeSet<(u8, u8)>,
}

impl Shape {
    pub fn rotate_right(&self) -> Self {
        let max_x = self.cells.iter().map(|(x, _)| *x).max().unwrap();
        let rotated_cells = self
            .cells
            .iter()
            .map(|(x, y)| (max_x - *y, *x))
            .collect::<BTreeSet<_>>();

        Shape {
            cells: rotated_cells,
        }
    }

    pub fn flip_horizontal(&self) -> Self {
        let max_x = self.cells.iter().map(|(x, _)| *x).max().unwrap();
        let flipped_cells = self
            .cells
            .iter()
            .map(|(x, y)| (max_x - *x, *y))
            .collect::<BTreeSet<_>>();

        Shape {
            cells: flipped_cells,
        }
    }

    pub fn all_orientations(&self) -> BTreeSet<Shape> {
        let mut orientations = BTreeSet::new();
        let mut current_shape = self.clone();

        for _ in 0..4 {
            orientations.insert(current_shape.clone());
            orientations.insert(current_shape.flip_horizontal());
            current_shape = current_shape.rotate_right();
        }

        orientations
    }
}

impl From<BTreeSet<(u8, u8)>> for Shape {
    fn from(cells: BTreeSet<(u8, u8)>) -> Self {
        Shape { cells }
    }
}

type NomError<'a> = nom::Err<nom::error::Error<&'a str>>;
type ParseResult<'a, T> = Result<T, NomError<'a>>;
type ParseOutput = (Vec<Shape>, Vec<(GridSize, Counts)>);

pub fn parse_input<'a>(input: &'a str) -> ParseResult<'a, ParseOutput> {
    let (_, res) =
        separated_pair(shapes_parser, line_ending, regions_parser).parse_complete(input)?;

    Ok(res)
}

fn shapes_parser(input: &str) -> nom::IResult<&str, Vec<Shape>> {
    separated_list1(
        line_ending::<&str, nom::error::Error<&str>>,
        separated_pair(
            complete::u8,
            pair(complete::char(':'), line_ending),
            many1(terminated(is_a::<&str, &str, _>("#."), line_ending)),
        ),
    )
    .map(|x| {
        x.into_iter()
            .map(|(_, lines)| {
                lines
                    .into_iter()
                    .enumerate()
                    .flat_map(|(y, line)| {
                        line.char_indices().filter_map(move |(x, c)| {
                            if c == '#' {
                                Some((x as u8, y as u8))
                            } else {
                                None
                            }
                        })
                    })
                    .collect::<BTreeSet<_>>()
                    .into()
            })
            .collect()
    })
    .parse(input)
}

fn regions_parser(input: &str) -> nom::IResult<&str, Vec<(GridSize, Counts)>> {
    separated_list1(
        line_ending,
        separated_pair(
            separated_pair(complete::u8, complete::char('x'), complete::u8),
            tag(": "),
            separated_list1(complete::char(' '), complete::u8),
        ),
    )
    .parse(input)
}

pub const SAMPLE: &str = indoc::indoc!(
    "0:
    ###
    ##.
    ##.
    
    1:
    ###
    ##.
    .##
    
    2:
    .##
    ###
    ##.
    
    3:
    ##.
    ###
    ##.
    
    4:
    ###
    #..
    ###
    
    5:
    ###
    .#.
    ###
    
    4x4: 0 0 0 0 2 0
    12x5: 1 0 1 0 2 2
    12x5: 1 0 1 0 3 2"
);
