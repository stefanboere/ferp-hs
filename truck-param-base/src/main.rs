use std::env;
use std::io::{self, Write};
use truck_param_base::{torus, Torus};

fn main() {
    let args: Vec<String> = env::args().collect();
    let infile = if args.len() >= 2 {
        &args[1]
    } else {
        "default.dhall"
    };
    let outfile = if args.len() >= 3 {
        Some(&args[2])
    } else {
        None
    };

    let input: Torus = serde_dhall::from_file(infile)
        .parse()
        .unwrap_or(Default::default());
    let torus = torus(input);

    let json = serde_json::to_vec_pretty(&torus.compress()).unwrap();

    match outfile {
        Some(file) => std::fs::write(file, &json).unwrap(),
        None => io::stdout().write_all(&json).unwrap(),
    }
}
