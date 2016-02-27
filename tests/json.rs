
extern crate prs;
use prs::pars::Token;
use prs::pars::predicate;
use prs::pars::fn_parser;
use prs::pars::Parse;
use prs::comb::many;

use prs::comb::{ParserComb,  ParserCombDynamic};
use prs::comb::Pair;
use std::collections::HashMap;
use std::vec::Vec;

use prs::stream::char_stream::CharStream;
use prs::stream::TokenStream;

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
use std::iter::FromIterator;
use prs::result::ParseErr;

fn json_parse(input: &str) -> Result<JsonValue, String>  {
    let stream = &mut CharStream::new(input);

    fn object_f(tokens: &mut CharStream) -> Result<JsonValue, ParseErr<char>> {

        let alph_num = predicate(move |c: &char| c.is_alphanumeric());

        let quoted_str = fn_parser(move |inp| (Token('\"'), many::<_,String>(&alph_num).skip(Token('\"')))
                .then(|(_, s)| s)
                .parse(inp));

        let value = (&quoted_str).then(|s| JsonValue::Str(s))
                        .or(fn_parser(object_f));
        let l_fig_bracket = Token('{');
        let r_fig_bracket = Token('}');

        let colon = Token(':');

        (l_fig_bracket,
            many(
            fn_parser(|inp|
                ((&quoted_str).skip(&colon), &value).skip(Token(','))
               .parse(inp))
            )
            .then(|v: Vec<(String, JsonValue)>| JsonValue::Object(HashMap::from_iter(v))),
        r_fig_bracket)
            .then(|(_, a, _)| a)
            .parse(tokens)
    }

    fn_parser(object_f).parse(stream).map_err(|e| format!("{}", e))
}


#[test]
fn json_test() {
    let mut sub_obj: HashMap<String, JsonValue> = HashMap::new();
    sub_obj.insert("first".to_owned(), JsonValue::Str("123".to_owned()));
    sub_obj.insert("second".to_owned(), JsonValue::Str("two".to_owned()));

    let mut res = HashMap::new();
    res.insert("ob1".to_owned(), JsonValue::Object(sub_obj));
    res.insert("second".to_owned(), JsonValue::Str("001".to_owned()));

    let parsed = json_parse("{\"ob1\":{\"first\":\"123\",\"second\":\"two\",},\"second\":\"001\",}");
    assert_eq!(parsed.unwrap(), JsonValue::Object(res));
}

