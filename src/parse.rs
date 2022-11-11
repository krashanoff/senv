//! parsers

use std::convert::Infallible;

use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_while, take_while_m_n},
    character::{
        complete::{alpha0, alpha1, alphanumeric1, anychar, newline, space0},
        is_alphabetic, is_alphanumeric, is_space,
    },
    combinator::{map_res, recognize},
    multi::{many0, many_till},
    sequence::{delimited, pair, tuple},
    IResult,
};

#[derive(Debug, Clone)]
struct Assignment<'a> {
    key: &'a str,
    value: &'a str,
}

fn ident(input: &str) -> IResult<&str, &str> {
    let first_char = alt((alpha1, tag("_")));
    let rest = many0(alt((alphanumeric1, tag("_"))));
    let ident = pair(first_char, rest);
    recognize(ident)(input)
}

/// the part before the equals
fn assignment(input: &str) -> IResult<&str, &str> {
    recognize(tuple((ident, delimited(space0, tag("="), space0), ident)))(input)
}

fn interpolate(input: &str) -> IResult<&str, &str> {
    recognize(tuple((tag("$"), delimited(tag("{"), ident, tag("}")))))(input)
}

fn comment(input: &str) -> IResult<&str, &str> {
    let end = newline;
    recognize(tuple((tag("#"), many_till(anychar, end))))(input)
}

fn unquoted(input: &str) -> IResult<&str, &str> {
    recognize(tuple((space0, comment)))(input)
}

/// no interpolation
fn single_quoted(input: &str) -> IResult<&str, &str> {
    recognize(delimited(tag("'"), many0(is_not("\"\n")), tag("'")))(input)
}

/// interpolates
fn double_quoted(input: &str) -> IResult<&str, &str> {
    recognize(delimited(tag("\""), many0(is_not("\"\n")), tag("\"")))(input)
}

/// multidouble quote
fn multidquote(input: &str) -> IResult<&str, &str> {
    recognize(delimited(tag("\"\"\""), alpha0, tag("\"\"\"")))(input)
}

/// multisingle quote
fn multisquote(input: &str) -> IResult<&str, &str> {
    recognize(delimited(tag("'''"), alpha0, tag("'''")))(input)
}

fn value(input: &str) -> IResult<&str, &str> {
    recognize(alt((
        multidquote,
        multisquote,
        double_quoted,
        single_quoted,
        unquoted,
    )))(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Error;

    #[test]
    fn test_ident() {
        assert_eq!(ident("_VAL1D"), Ok(("", "_VAL1D")));
        assert_eq!(ident("1nvalid").map_err(|_| Error), Err(Error));
    }

    #[test]
    fn test_interpolate() {
        assert_eq!(interpolate("${VARNAME}"), Ok(("", "${VARNAME}")))
    }
}
