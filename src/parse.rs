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
    character::complete::{alpha1, alphanumeric1, char, line_ending, multispace0, newline, space0},
    combinator::{eof, map, map_res, not, opt, recognize, rest, value},
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

/// equals
fn assignment(input: &str) -> IResult<&str, ()> {
    value((), char('='))(input)
}

fn comment(input: &str) -> IResult<&str, ()> {
    value(
        (),
        tuple((
            space0,
            char('#'),
            alt((take_until("\n"), take_until("\r\n"), rest)),
        )),
    )(input)
}

/// Some unquoted value. Just read to the first occurrence of a space
/// or a newline or EOF or a comment.
fn unquoted(input: &str) -> IResult<&str, &str> {
    let until_whitespace = alt((is_not(" "), is_not("\n\r")));
    map(
        tuple((until_whitespace, value((), comment))),
        |(value, _other)| value,
    )(input)
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
fn multi_dquote(input: &str) -> IResult<&str, &str> {
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
    let core = alt((
        multi_dquote,
        multi_squote,
        double_quoted,
        single_quoted,
        unquoted,
    ));
    map_res(core, |s| -> Result<&str, Infallible> { Ok(s) })(input)
}

/// A statement assigning some value to a key. Doesn't include a newline.
fn statement<'a>(input: &'a str) -> IResult<&str, Statement<'a>> {
    map_res(
        tuple((
            opt(tag("export ")),
            ident,
            assignment,
            env_value,
            opt(comment),
        )),
        |(_export, key, _equals, value, _comment): (
            Option<&str>,
            &str,
            (),
            &str,
            Option<()>,
        )|
         -> Result<Statement<'a>, Infallible> { Ok(Statement { key, value }) },
    )(input)
}

/// Any valid statement in the grammar. Includes empty lines and comments.
fn valid_statement<'a>(input: &'a str) -> IResult<&str, Option<Statement<'a>>> {
    let legal_line = alt((
        map(space0, |_| None),
        map(comment, |_| None),
        map(statement, |r| Some(r)),
    ));

    map(
        tuple((opt(legal_line), space0, alt((line_ending, eof)))),
        |(a, _, _)| a.unwrap(),
    )(input)
}

pub fn envfile<'a>(input: &'a str) -> IResult<&str, Vec<Statement<'a>>> {
    let file = separated_list0(newline, valid_statement);
    map_res(file, |v| -> Result<Vec<Statement>, Infallible> {
        Ok(v.iter()
            .filter_map(|v| match v {
                Some(s) => Some(s.clone()),
                _ => None,
            })
            .collect())
    })(input)
}

#[cfg(test)]
mod test {
    use super::*;

    const EXAMPLE_ENV: &'static str = include_str!("../example.env");

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
        assert_eq!(comment(" # comment"), Ok(("", ())));
        assert_eq!(comment("####### many #"), Ok(("", ())));
    }

    #[test]
    fn test_singles() {
        assert_eq!(single_quoted("\'some value\'"), Ok(("", "some value")));
        assert_ne!(single_quoted("\'uneven"), Ok(("", "")));
        assert_eq!(
            single_quoted("'raw text without variable interpolation'"),
            Ok(("", "raw text without variable interpolation"))
        );
    }

    #[test]
    fn test_doubles() {
        assert_eq!(double_quoted("\"some value\""), Ok(("", "some value")));
        assert_eq!(
            double_quoted("\"some\nmultiline\nval\"").map_err(|_| Error),
            Err(Error)
        );
        assert_eq!(
            double_quoted("\"MultipleLines and variable substitution: ${SIMPLE}\""),
            Ok(("", "MultipleLines and variable substitution: ${SIMPLE}"))
        );
    }

    #[test]
    fn test_unquoted() {
        assert_eq!(unquoted("value"), Ok(("", "value")));
        assert_eq!(unquoted("value # with comment"), Ok(("", "value")));
    }

    #[test]
    fn test_multi_quote() {
        assert_eq!(multi_squote("'''test\n'''"), Ok(("", "test\n")));
        assert_eq!(
            multi_squote(
                r"'''
test
''' # comment at end",
            ),
            Ok((" # comment at end", "\ntest\n"))
        );
        assert_eq!(
            multi_dquote(
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
    fn test_env_value() {
        assert_eq!(env_value("unquoted no"), Ok((" no", "unquoted")));
        assert_eq!(env_value("\"unquoted no\""), Ok(("", "unquoted no")));
        assert_eq!(
            env_value("\'unquoted no\"\"another value\'"),
            Ok(("", "unquoted no\"\"another value"))
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
        assert_eq!(
            statement("TEST=value # with comment at end"),
            Ok((
                "",
                Statement {
                    key: "TEST",
                    value: "value",
                }
            ))
        );
        assert_eq!(
            statement("export SOME_VAL= # no value"),
            Ok((
                "",
                Statement {
                    key: "SOME_VAL",
                    value: "",
                }
            ))
        );
        assert_eq!(
            statement("INTERPOLATED=\"MultipleLines and variable substitution: ${SIMPLE}\""),
            Ok((
                "",
                Statement {
                    key: "INTERPOLATED",
                    value: "MultipleLines and variable substitution: ${SIMPLE}",
                }
            ))
        );
    }

    #[test]
    fn test_multiple_statement() {
        assert_eq!(
            statement("export TEST_THING=hi\nANOTHER_KEY=\"EQUALS A THING\"\n"),
            Ok((
                "",
                Statement {
                    key: "TEST_THING",
                    value: "hi",
                }
            ))
        )
    }

    #[test]
    fn test_envfile() {
        let (r, ss) = envfile(EXAMPLE_ENV).expect("parse");
        assert_eq!(r, "");
        assert_eq!(ss.len(), 2);
        assert_eq!(
            ss[0],
            Statement {
                key: "TEST",
                value: "value",
            }
        );
        assert!(ss
            .iter()
            .find(|a| a.key == "TEST" && a.value == "value")
            .is_some());
        let (r, ss) = envfile(EXAMPLE_ENV).expect("parse");
        assert_eq!(r, "");
    }
}
