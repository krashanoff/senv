//! parsers

use std::convert::Infallible;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, anychar, newline, space0},
    combinator::{eof, map_res, opt, recognize, rest},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone)]
pub struct Statement<'a> {
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
    recognize(delimited(space0, tag("="), space0))(input)
}

fn interpolate(input: &str) -> IResult<&str, &str> {
    recognize(tuple((tag("$"), delimited(tag("{"), ident, tag("}")))))(input)
}

fn comment(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        space0,
        tag("#"),
        many0(anychar),
        opt(newline),
        opt(eof),
    )))(input)
}

fn unquoted(input: &str) -> IResult<&str, &str> {
    recognize(many0(is_not("\n")))(input)
}

/// no interpolation
fn single_quoted(input: &str) -> IResult<&str, &str> {
    delimited(tag("'"), recognize(many0(is_not("'\n"))), tag("'"))(input)
}

/// interpolates
fn double_quoted(input: &str) -> IResult<&str, &str> {
    delimited(tag("\""), recognize(many0(is_not("\"\n"))), tag("\""))(input)
}

/// multidouble quote
fn multidquote(input: &str) -> IResult<&str, &str> {
    delimited(
        tuple((opt(tag("\"\"\"")), newline)),
        recognize(rest),
        tuple((newline, opt(tag("\"\"\"")))),
    )(input)
}

/// multisingle quote
fn multisquote(input: &str) -> IResult<&str, &str> {
    delimited(tag("'''\n"), recognize(rest), tag("\n'''"))(input)
}

fn terminate_multiline(input: &str) -> IResult<&str, &str> {
    recognize(tuple((newline, opt(tag("'''")))))(input)
}

fn multi_other(input: &str) -> IResult<&str, &str> {
    delimited(
        tuple((newline, opt(tag("'''")))),
        recognize(rest),
        tuple((newline, opt(tag("'''")))),
    )(input)
}

fn env_value<'a>(input: &str) -> IResult<&str, &str> {
    alt((
        multidquote,
        multisquote,
        double_quoted,
        single_quoted,
        unquoted,
    ))(input)
}

/// A statement assigning some value to a key.
fn statement<'a>(input: &'a str) -> IResult<&str, Statement<'a>> {
    map_res(
        tuple((
            opt(tag("export ")),
            ident,
            assignment,
            opt(env_value),
            opt(comment),
        )),
        |(_export, key, _equals, value, _comment): (
            Option<&str>,
            &str,
            &str,
            Option<&str>,
            Option<&str>,
        )|
         -> Result<Statement<'a>, Infallible> {
            Ok(Statement {
                key,
                value: value.unwrap_or_else(|| ""),
            })
        },
    )(input)
}

pub fn envfile<'a>(input: &'a str) -> IResult<&str, Vec<Statement<'a>>> {
    separated_list0(newline, statement)(input)
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
        assert_eq!(ident("_VAL1D="), Ok(("=", "_VAL1D")));
    }

    #[test]
    fn test_comment() {
        assert_eq!(comment(" # comment"), Ok(("", " # comment")));
    }

    #[test]
    fn test_singles() {
        assert_eq!(single_quoted("\'some value\'"), Ok(("", "some value")));
    }

    #[test]
    fn test_doubles() {
        assert_eq!(double_quoted("\"some value\""), Ok(("", "some value")));
        assert_eq!(
            double_quoted("\"some\nmultiline\nval\"").map_err(|_| Error),
            Err(Error)
        );
    }

    #[test]
    fn test_unquoted() {
        assert_eq!(unquoted("value"), Ok(("", "value")));
    }

    #[test]
    fn test_multi() {
        println!("{:?}", multi_other("'''\ntest\n'''").expect("work"));
        assert!(false);

        multidquote(
            r"'''
    test
    '''",
        )
        .expect("thing");
        assert_eq!(
            multidquote(
                "\"\"\"\n \
              some value!\n \
              \"\"\""
            ),
            Ok(("", "\nsome value!\n"))
        );
        assert_eq!(
            multisquote(
                r"'''
        some value!
        '''"
            ),
            Ok(("", "\nsome value\n"))
        );
    }

    #[test]
    fn test_statement() {
        let test = "export _VAL1D_IDENT=\"some value\" # with a comment at the end, optionally.";
        let r = statement(test);
        match r {
            Ok((r, Statement { key, value })) => {
                assert_eq!(key, "_VAL1D_IDENT");
                assert_eq!(value, "some value");
            }
            Err(_) => assert!(false),
        }
    }
}
