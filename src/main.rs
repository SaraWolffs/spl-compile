#![allow(dead_code)]
#![deny(unused_must_use)]
use ast::*;
use regex::Regex;
use std::mem::size_of;

mod ast;
mod parser;
mod pretty;

fn main() {
    println!("Hello, world!");
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    println!("{}", re.is_match("2014-01-01"));
    println!("{}", std::mem::size_of::<ast::Decl>());
    println!(
        "{}, {}, {} ",
        size_of::<Option<Id>>(),
        size_of::<Id>(),
        size_of::<Exp>()
    );
    println!(
        "{}, {}, {}, {}, {}",
        size_of::<Id>(),
        size_of::<Vec<Id>>(),
        size_of::<Option<FunType>>(),
        size_of::<Vec<Decl>>(),
        size_of::<Vec<Stmt>>()
    );
    println!(
        "{}, {}, {}, {},{}",
        size_of::<&str>(),
        size_of::<u32>(),
        size_of::<Box<&str>>(),
        size_of::<Option<Box<&str>>>(),
        size_of::<Type>()
    );
}
