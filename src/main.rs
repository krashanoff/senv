use std::{path::PathBuf, fs::OpenOptions, io::Read};

use clap::Parser;

mod parse;

#[derive(Parser)]
#[clap(version, author)]
struct Opt {
    #[clap(index = 1)]
    shell_name: String,

    #[clap(index = 2)]
    input_file: PathBuf,
}

fn main() {
    let opts = Opt::parse();

    let mut f = OpenOptions::new().read(true).open(opts.input_file).expect("open");
    let mut s = String::new();
    f.read_to_string(&mut s).expect("read");

    let results = parse::envfile(s.as_str()).expect("parse");
    println!("{:?}", results);
}
