use regex::Regex;

mod lexgen;

fn main() {
    println!("Hello, world!");
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    println!("{}",re.is_match("2014-01-01"));
}
