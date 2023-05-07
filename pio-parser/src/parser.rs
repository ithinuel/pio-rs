use nom::{
    self,
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{alpha1, alphanumeric1, one_of},
    combinator::{map, recognize},
    multi::{fold_many1, many0_count, many1_count},
    sequence::{pair, preceded},
    IResult,
};
use pio::ProgramWithDefines;
use std::collections::HashMap;

pub type Span<'i> = nom_locate::LocatedSpan<&'i str>;

fn blank(input: Span) -> IResult<Span, char> {
    one_of(" \t\r")(input)
}
fn whitesp(input: Span) -> IResult<Span, ()> {
    fold_many1(blank, || (), |_, _| ())(input)
}
fn comment(input: Span) -> IResult<Span, Span> {
    preceded(alt((tag(";"), tag("//"))), take_until("\n"))(input)
}

fn id(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn binary(input: Span) -> IResult<Span, (Span, i64)> {
    preceded(
        tag("0b"),
        map(recognize(many1_count(one_of("01"))), |v: Span| {
            (
                v,
                v.fragment()
                    .chars()
                    .fold(0, |acc, c| acc << 1 | if c == '0' { 0 } else { 1 }),
            )
        }),
    )(input)
}

fn int(input: Span) -> IResult<Span, (Span, i64)> {
    map(recognize(nom::character::complete::digit1), |v: Span| {
        (
            v,
            v.fragment().chars().fold(0, |acc, c| {
                acc * 10 + i64::from(c.to_digit(10).expect("garanteed to be a digit"))
            }),
        )
    })(input)
}

fn hex(input: Span) -> IResult<Span, (Span, i64)> {
    preceded(
        tag("0x"),
        map(
            recognize(many1_count(one_of("0123456789abcdefABCDEF"))),
            |v: Span| {
                (
                    v,
                    v.fragment().chars().fold(0, |acc, c| {
                        acc * 16 + i64::from(c.to_digit(16).expect("guaranteed to be an hex digit"))
                    }),
                )
            },
        ),
    )(input)
}

pub fn program_parser<const PROGRAM_SIZE: usize>(
    _input: &str,
) -> IResult<&str, ProgramWithDefines<HashMap<String, i32>, PROGRAM_SIZE>> {
    todo!()
}

pub fn file(input: &str) -> IResult<&str, (), ()> {
    //
    todo!()
}
