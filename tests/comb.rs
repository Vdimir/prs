
extern crate prs;

use prs::pars::Token;
use prs::pars::Parse;
use prs::pars::predicate;
use prs::comb::ParserComb;
use prs::comb::many;
use prs::comb::maybe;
use prs::stream::char_stream::CharStream;
use prs::stream::TokenStream;

use prs::result::ParseErr::UnexpectedAt;

#[test]
fn or_test() {
    let mut input = CharStream::new("xyz");
    let x_or_y = Token('x').or(Token('y'));

    assert_eq!(x_or_y.parse(&mut input), Ok('x'));
    assert_eq!(x_or_y.parse(&mut input), Ok('y'));
    assert_eq!(x_or_y.parse(&mut input), Err(UnexpectedAt('z', 2)));

    let xyz = x_or_y.or(Token('z'));
    assert_eq!(xyz.parse(&mut input), Ok('z'));
}

#[test]
fn then_test() {
    let dig_square = predicate(|c: &char| c.is_digit(10))
            .then(|c: char| c.to_digit(10).map(|d| d * d).unwrap());

    let input = &mut CharStream::new("123a");
    assert_eq!(dig_square.parse(input), Ok(1));
    assert_eq!(dig_square.parse(input), Ok(4));
    assert_eq!(dig_square.parse(input), Ok(9));
    assert_eq!(dig_square.parse(input), Err(UnexpectedAt('a', 3)));
}

#[test]
fn many_test() {
    let dig = many(predicate(|c: &char| c.is_digit(10)));

    let input = &mut CharStream::new("123a");
    assert_eq!(dig.parse(input), Ok(vec!['1','2','3']));
    assert_eq!(dig.parse(input), Err(UnexpectedAt('a', 3)));
    assert_eq!(input.peek(), Some('a'));
}

#[test]
fn many_comb_test() {
    let dig = predicate(|c: &char| c.is_digit(10))
                .many()
                .then(|s: String| s.parse::<u32>().unwrap());

    assert_eq!(dig.parse(&mut CharStream::new("123")), Ok(123));
}

#[test]
fn mabye_test() {
    let maybe_x = maybe(Token('x'));

    let mut input = CharStream::new("xy");
    assert_eq!(maybe_x.parse(&mut input), Ok(Some('x')));
    assert_eq!(maybe_x.parse(&mut input), Ok(None));
    assert_eq!(maybe_x.parse(&mut input), Ok(None));
    assert_eq!(input.peek(), Some('y'));
}

#[test]
fn mabye_many_test() {
    let whitespace = maybe(Token(' ').many::<String>());

    let mut input = CharStream::new("     xy");
    assert!(whitespace.parse(&mut input).is_ok());
    assert_eq!(input.peek(), Some('x'));

    let mut input = CharStream::new("xy");
    assert!(whitespace.parse(&mut input).is_ok());
    assert_eq!(input.peek(), Some('x'));
}

#[test]
fn tup_test() {
    let x_and_y = (Token('x'), Token('y'), Token('z'));

    assert_eq!(x_and_y.parse(&mut CharStream::new("xyzx")), Ok(('x', 'y', 'z')));

    let input = &mut CharStream::new("yxx");
    assert_eq!(x_and_y.parse(input), Err(UnexpectedAt('y', 0)));
    assert_eq!(input.peek(), Some('y'));

    let input = &mut CharStream::new("xyy");
    assert_eq!(x_and_y.parse(input), Err(UnexpectedAt('y', 2)));
    assert_eq!(input.peek(), Some('x'));
}

#[test]
fn skip_test() {
    let x_and_y = (Token('x').skip(Token(' ')), Token('y'));

    assert_eq!(x_and_y.parse(&mut CharStream::new("x y")), Ok(('x', 'y')));

    let input = &mut CharStream::new("xy");
    assert_eq!(x_and_y.parse(input), Err(UnexpectedAt('y',1)));
    assert_eq!(input.peek(), Some('x'));
}

#[test]
fn skip_many_test() {
    let x_and_y = (Token('x').skip_any(Token(' ')), Token('y'));

    assert_eq!(x_and_y.parse(&mut CharStream::new("xy")), Ok(('x', 'y')));
    assert_eq!(x_and_y.parse(&mut CharStream::new("x y")), Ok(('x', 'y')));
    assert_eq!(x_and_y.parse(&mut CharStream::new("x     y")), Ok(('x', 'y')));
}

