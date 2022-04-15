#![allow(dead_code)]
#![deny(unused_must_use)]
use ast::*;
use itertools::Itertools;
use regex::Regex;
use std::mem::size_of;

mod ast;
mod parser;
mod pretty;

use parser::Parser;
use pretty::InternedShow;
use pretty::ShowConfig;

const EXAMPLESPL: &str = "/*
        Length
    of
    months
*/

[Int] dcLengthOfMonth = 0 : 31 : 28 : 31 : 30 : 31 : 30 : 31 : 31 : 30 : 31 : 30 : 31 : [];

getIndex (idx, list) :: Int [a] -> a
{
while (idx > 0) {
    list = list.tl;
}
return list.hd;
}


//Calculates in which month the given Int is. 1=January, 2=February, .., 12=December
month(dayOfYear)::Int -> Int{Int tempDay = dayOfYear;Int month = 0;




             // unuseful comment

while(month < 12)
{
                    if(tempDay > getIndex(month, dcLengthOfMonth))
{
                    tempDay = tempDay - - - - - getIndex(month, dcLengthOfMonth);
    }
else {return month + 1;}

   month = month + 1;
}
return month + 1;
}
";

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
    let mut example_p = Parser::new(EXAMPLESPL);
    let parseresult: Result<Vec<_>, _> = example_p.by_ref().try_collect();
    match parseresult {
        Ok(decls) => println!(
            "{}",
            decls.show(&ShowConfig::new(example_p.names().clone()))
        ),
        Err(perr) => println!("{}", format!("{perr}")),
    }
    let g3 = "-9223372036854775810".parse::<i64>();
    println!("{:?}", g3);
}
