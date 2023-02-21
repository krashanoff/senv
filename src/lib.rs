//! For parsing .env files

use std::fmt::Display;

use parse::Statement;

mod parse;

#[derive(Debug, Clone)]
pub enum Error {
    Nom(String),
    FailedToConsume(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nom(s) => write!(f, "error parsing: {}", s),
            Self::FailedToConsume(s) => write!(f, "failed to consume entire input: {}", s),
        }
    }
}

// pub fn parse<'a>(input: &'a str) -> Result<Vec<Statement<'a>>, Error> {
//     match parse::envfile(input).map_err(|e| Error::Nom(e.to_string()))? {
//         ("", s) => Ok(s),
//         (s, _) => Err(Error::FailedToConsume(s.to_string())),
//     }
// }
