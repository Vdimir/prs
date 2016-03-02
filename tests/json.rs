
extern crate prs;
use prs::pars::Token;
use prs::pars::predicate;
use prs::pars::fn_parser;
use prs::pars::Parse;
use prs::comb::many;

use prs::comb::ParserComb;
use std::collections::HashMap;

use prs::stream::char_stream::CharStream;
use prs::stream::RangeStream;
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

    fn num_f<'a, S>(tokens: &mut S) -> Result<f64, ParseErr<char>>
        where S: RangeStream<Token=char, Range=&'a str>
    {
//        let zero = Token('0');
//        let dig = predicate(|c| char::is_digit(*c, 10) );
//        let dig19 = predicate(|c| char::is_digit(*c, 10) && *c != '0');
//        zero.or(

        let save = tokens.save();
        loop {
            match tokens.peek() {
                Some('0'...'9') => tokens.next(),
                _ => break
            };
        }
        if let Some(s) = tokens.range(save) {
            Ok(s.parse::<f64>().unwrap())
        } else {
            Err(ParseErr::unexpected(tokens.peek()).at(tokens.position()))
        }
    }

    fn object_f(tokens: &mut CharStream) -> Result<JsonValue, ParseErr<char>> {
        let ws = wrap(predicate(|c| char::is_whitespace(*c)));
        let iden = predicate(|c| char::is_alphanumeric(*c));
        let quoted_str = wrap((Token('\"'), many::<_,String>(iden).skip(Token('\"'))
                                  .skip_any(ws.clone()))
                                  .then(|(_, s)| s));

        let value = wrap(quoted_str.clone().then(JsonValue::Str))
                        .or(fn_parser(num_f).then(JsonValue::Num))
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
    sub_obj.insert("first".to_owned(), JsonValue::Num(123_f64));
    sub_obj.insert("second".to_owned(), JsonValue::Str("two".to_owned()));

    let mut res = HashMap::new();
    res.insert("ob1".to_owned(), JsonValue::Object(sub_obj));
    res.insert("second".to_owned(), JsonValue::Str("001".to_owned()));

    let parsed = json_parse("{    \"ob1\" : { \"first\" : 123, \"second\" : \"two\", }, \"second\" : \"001\", } ");
    assert_eq!(parsed.unwrap(), JsonValue::Object(res));
}

