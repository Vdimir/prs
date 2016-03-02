
extern crate prs;
use prs::pars::Token;
use prs::pars::predicate;
use prs::pars::fn_parser;
use prs::pars::Parse;
use prs::comb::many;
use prs::comb::many0;
use prs::comb::maybe;

use prs::comb::ParserComb;
use std::collections::HashMap;

use prs::stream::char_stream::CharStream;
use prs::stream::RangeStream;
use prs::stream::TokenStream;
use prs::result::ParseErr;

#[allow(dead_code)]
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

use prs::comb::wrap;
use prs::comb::Wrap;

fn json_parse(input: &str) -> Result<JsonValue, String>  {
    let stream = &mut CharStream::new(input);


    fn num<'a>() -> Wrap<'a, CharStream<'a>, f64, ParseErr<char>> {
        #[derive(PartialEq)]
        enum Signum {
            Posititive,
            Negative
        }
        let sign = wrap(maybe(Token('+').or(Token('-')))
                        .then(|c| if c == Some('-') { Signum::Negative } else { Signum::Posititive }));
        let zero = Token('0').then(|_| "0".to_owned());
        let digs = wrap(many::<_,String>(predicate(|c| char::is_digit(*c, 10) )));
        let integer = zero.or(digs.clone())
            .then(|s| s.chars()
                  .map(|c| c.to_digit(10).unwrap() as i64)
                  .fold(0, |n, a| n*10 + a));
        let frac = maybe((Token('.'), digs.clone()).then(|(_,s)| s.chars()
                      .map(|c| c.to_digit(10).unwrap() as f64)
                      .fold((0.0, 1), |n, a| (n.0 + a/10f64.powi(n.1) , n.1 + 1)).0))
            .then(|r| r.unwrap_or(0.0));
        let number = (sign,
                      (wrap(integer.then(|n| n as f64)),
                      wrap(frac))
                        .then(|(n, c)| n+c))
            .then(|(s, n)| if s == Signum::Negative { -n } else { n });
        return wrap(number);
    }

    fn ws<'a>() -> Wrap<'a, CharStream<'a>, (), ParseErr<char>> {
        wrap(predicate(|c| char::is_whitespace(*c)).then(|_| ()))
    }

    fn q_str<'a>() ->  Wrap<'a, CharStream<'a>, String, ParseErr<char>> {
        let iden = predicate(|c| char::is_alphanumeric(*c) || *c == '_');
        let quoted_str = wrap((Token('\"'), many::<_,String>(iden).skip(Token('\"'))
                                  .skip_any(ws()))
                                  .then(|(_, s)| s));
        wrap(quoted_str)
    }

    fn value_f(tokens: &mut CharStream) -> Result<JsonValue, ParseErr<char>>
    {
        q_str().then(JsonValue::Str)
        .or(num().then(JsonValue::Num))
        .or(fn_parser(object_f))
        .or(fn_parser(array_f))
        .skip_any(ws())
        .parse(tokens)
    }

    fn array_f(tokens: &mut CharStream) -> Result<JsonValue, ParseErr<char>>
    {
        (Token('[').skip_any(ws()),
            many0(fn_parser(value_f).skip(Token(',').skip_any(ws()))),
            Token(']').skip_any(ws()))
        .then(|(_, a, _)| JsonValue::Array(a))
        .parse(tokens)
    }

    fn object_f(tokens: &mut CharStream) -> Result<JsonValue, ParseErr<char>> {
        let value = fn_parser(value_f);
        let kv_pair = wrap((q_str().skip(Token(':').skip_any(ws())), value));

        (Token('{').skip_any(ws()),
        wrap(many0(kv_pair.skip(Token(',').skip_any(ws()))).then(JsonValue::Object)),
        Token('}').skip_any(ws()))
            .then(|(_,a,_)| a)
            .parse(tokens)
    }

    let res = fn_parser(object_f).parse(stream);
    return res.map_err(|e| format!("{}", e))
}


#[test]
fn json_test() {
    let mut sub_obj: HashMap<String, JsonValue> = HashMap::new();
    sub_obj.insert("first".to_owned(), JsonValue::Num(1023_f64));
    sub_obj.insert("second".to_owned(), JsonValue::Str("two".to_owned()));

    let mut res = HashMap::new();
    res.insert("ob1".to_owned(), JsonValue::Object(sub_obj));
    res.insert("second".to_owned(), JsonValue::Str("001".to_owned()));
    res.insert("float".to_owned(), JsonValue::Num(3.14159_f64));
    res.insert("neg".to_owned(), JsonValue::Num(-0.5));
    res.insert("empty_obj".to_owned(), JsonValue::Object(HashMap::new()));
    res.insert("arr".to_owned(), JsonValue::Array(vec![
                                                JsonValue::Num(1.0),
                                                JsonValue::Num(2.0),
                                                JsonValue::Str("three".to_owned()),
                                                JsonValue::Num(-4.0),
                                                JsonValue::Object(HashMap::new()),
                                    ]));

    let parsed = json_parse(r#"{
        "ob1": {
                "first" : 1023,
                "second" : "two",
                },
        "second" : "001",
        "float": 3.14159,
        "neg": -0.5,
        "empty_obj" : { },
        "arr": [ 1, 2.0, "three", -4, { },],
    }"#);
    assert_eq!(parsed.unwrap(), JsonValue::Object(res));
}

