use std::{
    collections::HashMap, error, fmt::Display, fs::OpenOptions, io::Read, path::PathBuf,
    process::exit, str::FromStr,
};

use clap::Parser as ClapParser;
use either::Either;
use pest::{iterators::Pair, Parser};

#[derive(clap::Parser)]
#[clap(about, version)]
struct Args {
    /// Shell to output a conversion for.
    #[clap(value_enum, index = 1)]
    shell_name: Shell,

    /// Input environment file.
    #[clap(index = 2)]
    input: PathBuf,
}

#[derive(pest_derive::Parser)]
#[grammar = "../senv.pest"]
struct EnvParser;

fn main() -> Result<(), Box<dyn error::Error>> {
    let args = Args::parse();

    let mut file_content = String::new();
    let mut file = OpenOptions::new().read(true).open(args.input)?;
    file.read_to_string(&mut file_content)?;

    let file = EnvParser::parse(Rule::file, &file_content)?
        .next()
        .expect("file");

    let mut bindings: HashMap<InterpolationKey, Value> = Default::default();
    for mut line in file
        .into_inner()
        .filter(|line| line.as_rule() == Rule::line)
        .filter_map(|line| match line.into_inner().next() {
            Some(assignment) => {
                if assignment.as_rule() == Rule::assignment {
                    return Some(assignment.into_inner());
                }
                None
            }
            None => None,
        })
    {
        let key = line.next().expect("key is mandatory");
        let value = line.next().expect("value");
        bindings.insert(key.into(), value.into());
    }

    for (key, val) in bindings.iter() {
        match args.shell_name {
            Shell::Sh | Shell::Ksh | Shell::Bash | Shell::Zsh => {
                println!("export {}=\"\"\"{}\"\"\"", key, val.into_string(&bindings));
            }
            Shell::Fish => {
                println!("set -x {} \"{}\"", key, val.into_string(&bindings));
            }
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct InterpolationKey<'a>(&'a str);

impl<'a: 'b, 'b> From<Pair<'a, Rule>> for InterpolationKey<'a> {
    fn from(p: Pair<'a, Rule>) -> Self {
        Self(p.as_str())
    }
}

impl Display for InterpolationKey<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Value<'a>(Vec<Either<&'a str, InterpolationKey<'a>>>);

impl<'a: 'b, 'b> From<Pair<'a, Rule>> for Value<'b> {
    fn from(p: Pair<'a, Rule>) -> Self {
        let mut contents = vec![];
        for character in p.into_inner() {
            match character.as_rule() {
                Rule::quoted_character => contents.push(Either::Left(character.as_str())),
                Rule::unquoted_character => contents.push(Either::Left(character.as_str())),
                Rule::interpolation => {
                    let key = character.into_inner().next().expect("key");
                    contents.push(Either::Right(InterpolationKey(key.as_str())));
                }
                r => {
                    panic!("bad interpolation: {:?}", r);
                }
            }
        }
        Self(contents)
    }
}

impl Value<'_> {
    fn into_string(&self, bindings: &HashMap<InterpolationKey, Value>) -> String {
        let mut value = String::new();
        for char_or_key in &self.0 {
            match char_or_key {
                Either::Left(s) => {
                    value.push_str(s);
                }
                Either::Right(key) => {
                    let interpolated_value = bindings.get(&key);
                    value.push_str(
                        &interpolated_value
                            .expect("interpolated value")
                            .into_string(bindings)
                            .as_str(),
                    );
                }
            }
        }
        value
    }
}

/// Some shell supported by `senv`.
#[derive(Debug, Clone, Copy)]
enum Shell {
    Fish,
    Zsh,
    Bash,
    Sh,
    Ksh,
}

impl FromStr for Shell {
    type Err = clap::error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "fish" => Ok(Self::Fish),
            "zsh" => Ok(Self::Zsh),
            "bash" => Ok(Self::Bash),
            "sh" => Ok(Self::Sh),
            "ksh" => Ok(Self::Ksh),
            s => {
                eprintln!(
                    r"The shell {} isn't supported by senv yet.

It's really easy to add a conversion, though.
Add one at <https://github.com/krashanoff/senv>!",
                    s
                );
                exit(1);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use crate::{EnvParser, Rule};

    #[test]
    fn test_accept() {
        EnvParser::parse(Rule::file, "_valid_ident=test").expect("no err");
    }

    #[test]
    fn test_reject() {
        if let Ok(_) = EnvParser::parse(Rule::file, "1llegal=name") {
            assert!(false);
        }
    }
}
