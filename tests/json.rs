
extern crate prs;
use prs::pars::Token;
use prs::pars::predicate;
use prs::pars::fn_parser;
use prs::pars::Parse;
use prs::comb::many;
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
fn json_parse(input: &str) -> Result<JsonValue, String>  {
    let stream = &mut CharStream::new(input);

    fn object_f(tokens: &mut CharStream) -> Result<JsonValue, ParseErr<char>> {
        let ws = wrap(predicate(|c| char::is_whitespace(*c)));
        let iden = predicate(|c| char::is_alphanumeric(*c));
        let quoted_str = wrap((Token('\"'), many::<_,String>(iden).skip(Token('\"'))
                                  .skip_any(ws.clone()))
                                  .then(|(_, s)| s));

        #[derive(PartialEq)]
        enum Signum {
            Posititive,
            Negative
        }
        let sign = wrap(maybe(Token('+').or(Token('-'))).then(|c| if c == Some('-')
                                                         { Signum::Negative}
                                                         else { Signum::Posititive }));
        let zero = Token('0').then(|_| "0".to_owned());
        let digs = wrap(many::<_,String>(predicate(|c| char::is_digit(*c, 10) )));
        let integer = zero.or(digs.clone())
            .then(|s| s.chars()
                  .map(|c| c.to_digit(10).unwrap() as i64)
                  .fold(0, |n, a| n*10 + a));
        let frac = maybe((Token('.'), digs.clone()).then(|(_,s)| s.chars()
                      .map(|c| c.to_digit(10).unwrap() as f64)
                      .fold((0.0, 1), |n, a| (n.0 + a/10f64.powi(n.1) , n.1 + 1)).0));
        let number = (sign,
                      (wrap(integer.then(|n| n as f64)),
                      wrap(frac))
                        .then(|(n, c)| n+c.unwrap_or(0.0f64)))
            .then(|(s, n)| if s == Signum::Negative { -n } else { n });

        let value = wrap(quoted_str.clone().then(JsonValue::Str))
                        .or(number.then(JsonValue::Num))
                        .or(fn_parser(object_f))
                        .skip_any(ws.clone());
        let kv_pair = wrap((quoted_str.skip(Token(':').skip_any(ws.clone())), value));

        (Token('{').skip_any(ws.clone()),
        wrap(many(kv_pair.skip(Token(',').skip_any(ws.clone()))).then(JsonValue::Object)),
        Token('}').skip_any(ws.clone()))
            .then(|(_,a,_)| a)
            .parse(tokens)
    }

    fn_parser(object_f).parse(stream).map_err(|e| format!("{}", e))
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

    let parsed = json_parse("{    \"ob1\" : { \"first\" : 1023, \"second\" : \"two\", }, \"second\" : \"001\", \"float\": 3.14159, \"neg\": -0.5,} ");
    assert_eq!(parsed.unwrap(), JsonValue::Object(res));
}

