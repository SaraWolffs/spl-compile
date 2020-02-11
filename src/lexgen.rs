mod lexgen{
use regex::Regex;
pub fn lexgen<T>(spec: Vec<(&str, &dyn Fn(&str) -> T)>) -> 
    impl Fn(&str) -> Result<Vec<T>,String> {

    move |s| { Err("Not Implemented".to_owned()) }

}
}
