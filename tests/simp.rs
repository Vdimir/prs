
extern crate prs;

use prs::pars::Token;
use prs::stream::char_stream::CharStream;
use prs::stream::TokenStream;
use prs::pars::Parse;
use prs::pars::predicate;
use prs::pars::fn_parser;
use prs::comb::ParserComb;

use prs::result::ParseErr::Unexpected;

#[test]
fn token_test() {
    let mut input = CharStream::new("xyz");

    assert_eq!(Token('x').parse(&mut input), Ok('x'));
    assert_eq!(Token('y').parse(&mut input), Ok('y'));
    assert_eq!(Token('w').parse(&mut input), Err(Unexpected('z')));
    assert_eq!(Token('z').parse(&mut input), Ok('z'));
}

#[test]
fn pred_test() {
    let mut input = CharStream::new("12a");

    // let dig = predicate::<_,CharStream,_>("digit",|c| c.is_digit(10));
    let dig = predicate(|c: &char| c.is_digit(10))
        .then(|c: char| c.to_digit(10).unwrap());

    assert_eq!(dig.parse(&mut input), Ok(1));
    assert_eq!(dig.parse(&mut input), Ok(2));
    assert_eq!(dig.parse(&mut input), Err(Unexpected('a')));
    assert_eq!(input.peek(), Some('a'));
}

#[test]
fn fn_parse_test() {

    fn any_before_space<'a>(s: &mut CharStream<'a>) -> Result<String, ()> {
        let mut res = String::new();
        while let Some(c) = s.next() {
            if c == ' ' {
                break;
            }
            res.push(c);
        }
        Ok(res)
    }

    let p = fn_parser(any_before_space);
    let input = &mut CharStream::new("abc de");
    assert_eq!(p.parse(input), Ok("abc".to_owned()));
    assert_eq!(input.peek(), Some('d'));

    let input = &mut CharStream::new("abcde");
    assert_eq!(p.parse(input), Ok("abcde".to_owned()));

    let input = &mut CharStream::new(" abcde");
    assert_eq!(p.parse(input), Ok("".to_owned()));
    assert_eq!(input.peek(), Some('a'));
}

