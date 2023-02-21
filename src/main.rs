use std::{
    collections::HashMap, error, fs::OpenOptions, io::Read, path::PathBuf, process::exit,
    str::FromStr,
};

use clap::Parser as ClapParser;
use pest::Parser;

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

    let mut bindings: HashMap<String, String> = Default::default();
    for line in file.into_inner().filter(|line| {
        if let Rule::assignment = line.as_rule() {
            true
        } else {
            false
        }
    }) {
        let mut inner_rules = line.into_inner();
        let assignment_or_comment = inner_rules.next().expect("assignment");

        println!("{}", assignment_or_comment.as_str());
    }

    Ok(())
}
