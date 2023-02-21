use std::{fs::OpenOptions, io::Read, path::PathBuf};

use clap::Parser;

// use senv::parse;

#[derive(Parser)]
#[clap(version, author)]
struct Opt {
    #[clap(index = 1)]
    input_file: PathBuf,

    #[clap(index = 2, default_value = "fish")]
    shell_name: String,
}

fn main() {
    let opts = Opt::parse();

    let mut f = OpenOptions::new()
        .read(true)
        .open(opts.input_file)
        .expect("open");
    let mut s = String::new();
    f.read_to_string(&mut s).expect("read");

    // let s = parse(s.as_str()).expect("parse");
    // println!("{:?}", s);
}
