
extern crate prs;

use prs::pars::Token;
use prs::stream::char_stream::CharStream;
use prs::stream::TokenStream;
use prs::pars::Parse;
use prs::pars::predicate;
use prs::pars::parse_while;
use prs::pars::fn_parser;
use prs::comb::ParserComb;

use prs::result::ParseErr::UnexpectedAt;

#[test]
fn token_test() {
    let mut input = CharStream::new("xyz");

    assert_eq!(Token('x').parse(&mut input), Ok('x'));
    assert_eq!(Token('y').parse(&mut input), Ok('y'));
    assert_eq!(Token('w').parse(&mut input), Err(UnexpectedAt('z', 2)));
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
    assert_eq!(dig.parse(&mut input), Err(UnexpectedAt('a', 2)));
    assert_eq!(input.peek(), Some('a'));
}

#[test]
fn parse_while_test() {
    let mut input = CharStream::new("12a1");

    let dig = parse_while(|c: &char| c.is_digit(10));

    assert_eq!(dig.parse(&mut input), Ok("12"));
    assert_eq!(dig.parse(&mut input), Err(UnexpectedAt('a', 2)));
}

#[test]
fn parse_while_zero_allocate_test() {

    let p = (parse_while(|c: &char| c.is_digit(10)),
            Token::<CharStream>('.'),
            parse_while(|c: &char| c.is_digit(10)));

    let s = "10.1";
    let (ten, _, one) = p.parse_from(s).unwrap();

    unsafe {
        assert_eq!(ten.as_ptr(), s.as_ptr());
        assert_eq!(one.as_ptr(), s.as_ptr().offset(3));
    }
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
