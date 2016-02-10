
extern crate prs;

use prs::pars::Token;
use prs::stream::char_stream::CharStream;
use prs::stream::TokenStream;
use prs::result::ParseErr::Expected;
use prs::pars::Parse;
use prs::pars::predicate;
use prs::pars::fn_parser;

#[test]
fn token_test() {
    let mut input = CharStream::new("xyz");

    assert_eq!(Token('x').parse(&mut input), Ok('x'));
    assert_eq!(Token('y').parse(&mut input), Ok('y'));
    assert_eq!(Token('w').parse(&mut input), Err(Expected('w')));
    assert_eq!(Token('z').parse(&mut input), Ok('z'));
}

#[test]
fn pred_test() {
    let mut input = CharStream::new("12a");

    // let dig = predicate::<_,CharStream,_>("digit",|c| c.is_digit(10));
    let dig = predicate("digit",|c: &char| c.is_digit(10));

    assert_eq!(dig.parse(&mut input), Ok('1'));
    assert_eq!(dig.parse(&mut input), Ok('2'));
    assert_eq!(dig.parse(&mut input), Err(Expected("digit".to_owned())));
    assert_eq!(input.peek(), Some('a'));
}

#[test]
fn fn_parse_test() {

    fn foo<'a>(s: &mut &'a str) -> Result<&'a str, ()> {
        let off = s.chars()
            .take_while(|c| c.is_digit(10))
            .fold(0, |len, c| len + c.len_utf8());
        if off > 0 {
            Ok(&s[..off])
        } else {
            Err(())
        }

    }

    let p = fn_parser(foo);
    assert_eq!(p.parse(&mut "1235 25"), Ok("1235"));
}
