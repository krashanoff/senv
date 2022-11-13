//! parsers
//!
//! TODO:
//! * escaped characters
//! * interpolation
//!

use std::convert::Infallible;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::complete::{alpha1, alphanumeric1, anychar, newline, space0},
    combinator::{eof, map, map_res, opt, recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, tuple},
    IResult,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    let first_tag = value((), tag("\"\"\""));
    let last_tag = value((), tag("\"\"\""));
    let r = tuple((first_tag, recognize(take_until("\"\"\"")), last_tag));
    let mut next = map(r, |(_b, inner, _a): ((), &str, ())| inner);
    next(input)
}

/// multi single quote
fn multi_squote(input: &str) -> IResult<&str, &str> {
    let first_tag = value((), tag("'''"));
    let last_tag = value((), tag("'''"));
    let r = tuple((first_tag, recognize(take_until("'''")), last_tag));
    let mut next = map(r, |(_b, inner, _a): ((), &str, ())| inner);
    next(input)
}

fn env_value<'a>(input: &str) -> IResult<&str, &str> {
    alt((
        multidquote,
        multi_squote,
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
    many0(delimited(many0(newline), statement, many0(newline)))(input)
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
        assert_eq!(multi_squote("'''test\n'''"), Ok(("", "test\n")));
        assert_eq!(
            multi_squote(
                r"'''
test
'''",
            ),
            Ok(("", "\ntest\n"))
        );
        assert_eq!(
            multidquote(
                "\"\"\"\n\
              some value!\n\
                \"\"\""
            ),
            Ok(("", "\nsome value!\n"))
        );
        assert_eq!(
            multi_squote(
                "'''\n\
        some value!!\n\
'''"
            ),
            Ok(("", "\nsome value!!\n"))
        );
    }

    #[test]
    fn test_statement() {
        assert_eq!(
            statement(
                "export _VAL1D_IDENT=\"some value\" # with a comment at the end, optionally."
            ),
            Ok((
                "",
                Statement {
                    key: "_VAL1D_IDENT",
                    value: "some value",
                }
            ))
        );
        assert_eq!(
            statement("VALI4='''\nsome value\n''' # with a comment"),
            Ok((
                "",
                Statement {
                    key: "VALI4",
                    value: "\nsome value\n",
                }
            ))
        );
    }

    #[test]
    fn test_envfile() {
        let (r, ss) = envfile(
            r"
TEST=value
export SOME_VAL= # no value",
        )
        .expect("thing");
        assert_eq!(r, "");
        assert_eq!(ss.len(), 2);
        assert!(ss
            .iter()
            .find(|a| a.key == "TEST" && a.value == "value")
            .is_some());
    }
}
