
use regex::{Regex,RegexSet};
use std::error::Error;
pub fn lexgen<T>(spec: Vec<(&str, &dyn Fn(&str) -> T)>) -> 
    Result<impl Fn(&str) -> Result<Vec<T>,String>,String>{ 
        let (res,flist) : (Vec<_>,Vec<_>) = spec.iter().cloned().unzip();
        let rxlist : Vec<Regex> =  res.iter().map(|r| { Regex::new(r).unwrap() }).collect();
        let rxset = RegexSet::new(res); // consumes res
        //let flist : Vec<&dyn Fn(&str) -> T> 
        Ok(move |s : &_| { Err("Not Implemented".to_owned()) })
}
