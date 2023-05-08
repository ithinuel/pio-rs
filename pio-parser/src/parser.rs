use nom::{
    self,
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::complete::{alpha1, alphanumeric1, anychar, one_of},
    combinator::{map, recognize},
    multi::{fold_many1, many0_count, many1_count},
    sequence::{pair, preceded, terminated, tuple},
    IResult,
};
use pio::ProgramWithDefines;
use std::collections::HashMap;

pub type Span<'i> = nom_locate::LocatedSpan<&'i str, nom_tracable::TracableInfo>;

fn blank(input: Span) -> IResult<Span, char> {
    one_of(" \t")(input)
}
#[nom_tracable::tracable_parser]
fn whitesp(input: Span) -> IResult<Span, ()> {
    fold_many1(blank, || (), |_, _| ())(input)
}
#[nom_tracable::tracable_parser]
fn comment(input: Span) -> IResult<Span, Span> {
    preceded(alt((tag(";"), tag("//"))), take_until("\n"))(input)
}

#[nom_tracable::tracable_parser]
fn id(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

#[nom_tracable::tracable_parser]
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

#[nom_tracable::tracable_parser]
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

#[nom_tracable::tracable_parser]
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

#[nom_tracable::tracable_parser]
fn directive(input: Span) -> IResult<Span, Span> {
    preceded(tag("."), id)(input)
}

#[nom_tracable::tracable_parser]
fn newlines(input: Span) -> IResult<Span, Span> {
    recognize(many1_count(tag("\n")))(input)
}

#[nom_tracable::tracable_parser]
fn code_block(input: Span) -> IResult<Span, (Span, Span)> {
    let blank = || many0_count(one_of(" \t"));
    tuple((
        tuple((tag("%"), blank(), is_not("{ \t"), blank(), tag("{"))),
        terminated(
            recognize(many1_count(alt((
                terminated(is_not("\n"), tag("\n")),
                tag("\n"),
            )))),
            tag("%}"),
        ),
    ))(input)
    .map(|(s, (name, block))| (s, (name.2, block)))
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

#[cfg(test)]
mod tests {
    #[test]
    fn code_block() {
        let (_, (name, block)) = super::code_block(super::Span::new_extra(
            r"% c-sdk {
%}",
            nom_tracable::TracableInfo::new().parser_width(64),
        ))
        .expect("Successfuly parsed");

        assert_eq!(name.fragment(), &"c-sdk");
        assert_eq!(block.fragment(), &"\n");

        let (_, (name, block)) = super::code_block(super::Span::new_extra(
            r"% c-sdk { whatever
            fits
            here
%}",
            nom_tracable::TracableInfo::new().parser_width(64),
        ))
        .expect("Successfuly parsed");

        assert_eq!(name.fragment(), &"c-sdk");
        assert_eq!(block.fragment(), &r" whatever
            fits
            here
");

        super::code_block(super::Span::new_extra(
            r"% c-sdk { %}",
            nom_tracable::TracableInfo::new().parser_width(64),
        ))
        .expect_err("Invalid empty code block");
        super::code_block(super::Span::new_extra(
            r"%c-sdk{
%}",
            nom_tracable::TracableInfo::new().parser_width(64),
        ))
        .expect("Successfuly parsed");
    }
}
