
#![feature(test)]

extern crate test;

use test::Bencher;

extern crate prs;
use prs::pars::{Token, predicate, fn_parser, Parse };
use prs::comb::{ParserComb, many, many0, maybe, Seq, wrap, Wrap };

use prs::stream::char_stream::CharStream;
use prs::stream::{ RangeStream, TokenStream };
use prs::result::{ ParseErr, SupressedRes };

use std::iter::FromIterator;
use std::collections::HashMap;
use std::convert::Into;
use std::ops::Neg;

#[derive(PartialEq, Clone, Debug)]
enum JsonValue {
    Str(String),
    Num(f64),
    Object(HashMap<String, JsonValue>),
    Array(Vec<JsonValue>),
    True,
    False,
    Null,
}

fn json_parse(input: &str) -> Result<JsonValue, String>  {
    let stream = &mut CharStream::new(input);

    // Panic if string containt non digit characters
    fn parse_int<S: Into<String>>(s: S) -> i64 {
        s.into().chars()
              .map(|c| c.to_digit(10).unwrap() as i64)
              .fold(0, |n, a| n*10 + a)
    }
    fn parse_frac<S: Into<String>>(s: S) -> f64 {
        let mut n = parse_int(s) as f64;
        while n > 1.0 {
            n /= 10.0
        }
        n
    }

    fn digs<'a>() -> Wrap<'a, CharStream<'a>, String, ParseErr<char>> {
        wrap(many::<_,String>(predicate(|c| char::is_digit(*c, 10) )))
    }

    fn num<'a>() -> Wrap<'a, CharStream<'a>, f64, ParseErr<char>> {
        let sign = wrap(maybe(Token('+').or(Token('-')))
                        .then(|c| c.unwrap_or('+')));
        let zero = Token('0').then(|_| "0".to_owned());
        let integer = zero
                      .or(digs())
                      .then(parse_int);
        let frac = (Token('.'), digs())
                    .then(|(_,s)| parse_frac(s));
        let unsigned = (integer, maybe(frac).then(|m| m.unwrap_or(0.0)))
                        .then(|(n, c)| n as f64 + c);
        let number = (sign, unsigned)
            .then(|(s, n)| if s == '+' { n } else { -n });
        return wrap(number);
    }

    fn ws<'a>() -> Wrap<'a, CharStream<'a>, (), ParseErr<char>> {
        wrap(predicate(|c| char::is_whitespace(*c)).then(|_| ()))
    }

    fn q_str<'a>() ->  Wrap<'a, CharStream<'a>, String, ParseErr<char>> {
        let iden = many0::<_,String>(predicate(|c| *c != '"'));
        let quoted_str = (Token('"'), iden, Token('"'))
                .then(|(_, s, _)| s);
        wrap(quoted_str.skip_any(ws()))
    }


    fn create_kwd<'a, 'b: 'a, R>(kwd: &'a str) -> Wrap<'a, CharStream<'b>, R, ParseErr<char>>
    where R: FromIterator<char> + 'a
    {
        wrap(Seq::from(kwd.chars().map(Token)))
    }

    fn keyword<'a, 'b: 'a>() -> Wrap<'a, CharStream<'b>, JsonValue, ParseErr<char>> {
        let tru = create_kwd("true").then(|_: SupressedRes| JsonValue::True);
        let fals = create_kwd("false").then(|_: SupressedRes| JsonValue::False);
        let nul = create_kwd("null").then(|_: SupressedRes| JsonValue::Null);
        return wrap(tru.or(fals).or(nul).skip_any(ws()));
    }

    fn value<'a, 'b: 'a>() -> Wrap<'a, CharStream<'b>, JsonValue, ParseErr<char>> {
        wrap(
        q_str().then(JsonValue::Str)
        .or(num().then(JsonValue::Num))
        .or(fn_parser(object_f))
        .or(fn_parser(array_f))
        .or(keyword())
        .skip_any(ws()))
    }

    fn array_f(tokens: &mut CharStream) -> Result<JsonValue, ParseErr<char>>
    {
        let delim = Token(',').skip_any(ws());
        let list = (many0(value().skip(delim)),
            value())
            .then(|(mut v, r):(Vec<_>,_)| { v.push(r); v });
        (Token('[').skip_any(ws()),
        maybe(list).then(|r| r.unwrap_or(Vec::new())),
        Token(']').skip_any(ws()))
        .then(|(_, a, _)| JsonValue::Array(a))
        .parse(tokens)
    }

    fn kv_pair<'a>() -> Wrap<'a, CharStream<'a>, (String, JsonValue), ParseErr<char>> {
        let delim = Token(':').skip_any(ws());
        wrap((q_str().skip(delim), value()))
    }

    fn object_f(tokens: &mut CharStream) -> Result<JsonValue, ParseErr<char>> {
        let delim = Token(',').skip_any(ws());
        // ( k:v,)* k:v
        let list = (many0(kv_pair().skip(delim)), kv_pair())
                     .then(|(mut v, r):(Vec<_>,_)| { v.push(r); v });

        (Token('{').skip_any(ws()),
        maybe(wrap(list))
            .then(|r| r.unwrap_or(Vec::new()))
            .then(HashMap::from_iter)
            .then(JsonValue::Object),
        Token('}').skip_any(ws()))
            .then(|(_,a,_)| a)
            .parse(tokens)
    }

    let res = value().parse(stream);
    return res.map_err(|e| format!("{}", e))
}

#[bench]
fn json_test(b: &mut Bencher) {
    use JsonValue::*;
    let mut sub_obj: HashMap<String, JsonValue> = HashMap::new();
    sub_obj.insert("first".to_owned(), Num(1023_f64));
    sub_obj.insert("second".to_owned(), Str("two".to_owned()));

    let mut res = HashMap::new();
    res.insert("ob1".to_owned(), Object(sub_obj));
    res.insert("second".to_owned(), Str("001".to_owned()));
    res.insert("float".to_owned(), Num(3.14159_f64));
    res.insert("neg".to_owned(), Num(-0.5));
    res.insert("empty_obj".to_owned(), Object(HashMap::new()));
    res.insert("arr".to_owned(), Array(vec![ Num(1.0),
                                        Num(2.0),
                                        Str("three".into()),
                                        Num(-4.0),
                                        Object(HashMap::new()),
                                        True,
                                    ]));
    res.insert("nil".to_owned(), Null);
    let mut parsed = Err("None".to_owned());
    b.iter(|| {
        parsed = json_parse(r#"{
            "ob1": {
                    "first" : 1023,
                    "second" : "two"
                    },
            "second" : "001",
            "float": 3.14159,
            "neg": -0.5,
            "empty_obj" : { },
            "arr": [ 1, 2.0, "three", -4, { }, true],
            "nil" : null
        }"#);
    });
    assert_eq!(parsed.unwrap(), JsonValue::Object(res));
}

use std::io::Read;
use std::fs::File;
use std::path::Path;

#[bench]
fn json_file_test(b: &mut Bencher) {
    let mut data = String::new();
    File::open(&Path::new("data.json"))
        .and_then(|mut file| file.read_to_string(&mut data))
        .unwrap();

    let mut parsed = Err("None".to_owned());
    b.iter(|| {
        parsed = json_parse(&data);
    });
    assert!(parsed.is_ok());
}

