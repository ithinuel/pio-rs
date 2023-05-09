use nom::{
    self,
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case},
    character::complete::{alpha1, alphanumeric1, one_of},
    combinator::{map, map_res, opt, recognize},
    multi::{many0_count, many1_count},
    sequence::{pair, preceded, terminated, tuple},
    Err::Failure,
    IResult,
};
use nom_tracable::tracable_parser;

use crate::ast::{Expression, Symbol, Value};

pub type Span<'i> = nom_locate::LocatedSpan<&'i str, nom_tracable::TracableInfo>;

use super::ParseError;

fn blank(input: Span) -> IResult<Span, char> {
    one_of(" \t")(input)
}

#[tracable_parser]
fn id(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

#[tracable_parser]
fn binary(input: Span) -> IResult<Span, (Span, u64)> {
    preceded(
        tag("0b"),
        map_res(recognize(many1_count(one_of("01"))), |v: Span| {
            if v.fragment().chars().count() > 64 {
                return Err(Failure(ParseError::IntegerOutOfBound));
            }
            let value = v
                .fragment()
                .chars()
                .fold(0, |acc, c| acc << 1 | if c == '0' { 0 } else { 1 });
            Ok((v, value))
        }),
    )(input)
}

#[tracable_parser]
fn int(input: Span) -> IResult<Span, (Span, u64)> {
    map_res(recognize(nom::character::complete::digit1), |v: Span| {
        v.fragment().chars().try_fold(0, |acc, c| {
            let digit = u64::from(c.to_digit(10).expect("garanteed to be a digit"));
            if (u64::MAX / acc) < 10 || (u64::MAX - 10 * acc) < digit {
                return Err(Failure(ParseError::IntegerOutOfBound));
            }
            Ok(acc * 10 + digit)
        }).map(|value| (v, value))
    })(input)
}

#[tracable_parser]
fn hex(input: Span) -> IResult<Span, (Span, u64)> {
    preceded(
        tag("0x"),
        map_res(
            recognize(many1_count(one_of("0123456789abcdefABCDEF"))),
            |v: Span| {
                let value = v.fragment().chars().try_fold(0, |acc, c| {
                    let digit = u64::from(c.to_digit(16).expect("guaranteed to be an hex digit"));
                    Ok(acc * 16 + digit)
                })?;
                Ok::<_, ()>((v, value))
            },
        ),
    )(input)
}

#[tracable_parser]
fn directive(input: Span) -> IResult<Span, Span> {
    preceded(tag("."), id)(input)
}

#[tracable_parser]
fn newlines(input: Span) -> IResult<Span, Span> {
    recognize(many1_count(tag("\n")))(input)
}

#[tracable_parser]
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

#[tracable_parser]
fn value(input: Span) -> IResult<Span, Value> {
    alt((
        map(alt((hex, binary, int)), |(s, v)| Value::Integer(v, s)),
        map(id, |id| Value::Identifier(id)),
    ))(input)
}

#[tracable_parser]
fn expression(input: Span) -> IResult<Span, Expression> {
    alt((map(value, |v| Expression::Value(v)),))(input)
}

#[tracable_parser]
fn define(input: Span) -> IResult<Span, (Symbol, Expression)> {
    preceded(
        tag(".define"),
        tuple((
            map(opt(alt((tag_no_case("public"), tag("*")))), |v| v.is_some()),
            id,
            expression,
        )),
    )(input)
    .map(|(sp, (public, id, e))| (sp, (Symbol { public, span: id }, e)))
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Value};

    use super::Span;

    #[test]
    fn code_block() {
        let (_, (name, block)) = super::code_block(Span::new_extra(
            r"% c-sdk {
%}",
            nom_tracable::TracableInfo::new().parser_width(64),
        ))
        .expect("Successfuly parsed");

        assert_eq!(name.fragment(), &"c-sdk");
        assert_eq!(block.fragment(), &"\n");

        let (_, (name, block)) = super::code_block(Span::new_extra(
            r"% c-sdk { whatever
            fits
            here
%}",
            nom_tracable::TracableInfo::new().parser_width(64),
        ))
        .expect("Successfuly parsed");

        assert_eq!(name.fragment(), &"c-sdk");
        assert_eq!(
            block.fragment(),
            &r" whatever
            fits
            here
"
        );

        super::code_block(Span::new_extra(
            r"% c-sdk { %}",
            nom_tracable::TracableInfo::new().parser_width(64),
        ))
        .expect_err("Invalid empty code block");
        super::code_block(Span::new_extra(
            r"%c-sdk{
%}",
            nom_tracable::TracableInfo::new().parser_width(64),
        ))
        .expect("Successfuly parsed");
    }

    #[test]
    fn expression() {
        let cases = [("32", 32, 2, 1), ("0b100000", 32, 8, 1), ("0x20", 32, 4, 1)];

        for case in cases {
            let span = Span::new_extra(case.0, nom_tracable::TracableInfo::new());
            let (rest, v) = super::expression(span).expect("Successfuly parsed");
            assert_eq!(
                unsafe {
                    super::Span::new_from_raw_offset(
                        case.2,
                        case.3,
                        "",
                        nom_tracable::TracableInfo::new(),
                    )
                },
                rest
            );
            assert_eq!(Expression::Value(Value::Integer(case.1, span)), v);
        }
    }
}
