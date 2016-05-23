extern crate prs;
use prs::pars::{Token, predicate, Parse};
use prs::comb::{ParserComb, many, wrap, ParserWraper, Seq };

use prs::stream::char_stream::CharStream;
use prs::result::ParseErr;


fn parse_int<S: Into<String>>(s: S) -> i64 {
    s.into().chars()
          .map(|c| c.to_digit(10).unwrap() as i64)
          .fold(0, |n, a| n*10 + a)
}

fn digs<'a>() -> ParserWraper<'a, CharStream<'a>, i64, ParseErr<char>> {
    wrap(many::<_,String>(predicate(|c| char::is_digit(*c, 10) )).then(parse_int))
}

fn add_expr<'a>() -> ParserWraper<'a, CharStream<'a>, i64, ParseErr<char>> {
    wrap((digs(),Token('+'),digs()).then(|(a,_,b)| a+b ))
}

#[test]
fn example_one() {
    let input = String::from("10+5");
    let mut s = CharStream::new(&input);
    let res = add_expr().parse(&mut s).unwrap();
    println!("{} = {}", input, res);
}

#[test]
fn example_two() {

    let mut input = String::from("10+5");
    {
        let mut s = CharStream::new(&input);
        let res = add_expr().parse(&mut s).unwrap();
        println!("{} = {}", input, res);
    }
    input.push('6');
    //let x = many::<_,u32>(5);
}

#[test]
fn pair() {
    let op = Token('+').or(Token('-'));
    let p = (digs(), op, digs());

    assert_eq!(p.parse_from("10+5").unwrap(), (10,'+',5));
}

#[derive(Debug, PartialEq)]
enum ExprToken {
    Num(i64),
    Op(char)
}

#[test]
fn seq() {
    use ExprToken::*;
    let op = Token('+').or(Token('-')).then(Op);
    let p = digs().then(Num)
            .and(op)
            .and(digs().then(Num));

    let res: Vec<_> = p.parse_from("10+5").unwrap();
    assert_eq!(res, vec![Num(10),Op('+'),Num(5)]);
}
