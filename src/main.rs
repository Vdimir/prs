//
//
//
//
//

use std::marker::PhantomData;
use std::default::Default;


// ================================ TokenStream ================================
trait TokenStream {
    type TokenType;
    type RangeType;

    fn get(&self) -> Option<Self::TokenType>;

}

trait IterableTokenStream<It>: TokenStream
    where It: Iterator<Item=Self::TokenType> {
    fn iter(&self) -> It;
}


// rustc --explain E0117
// impl<'a> Iterator for &'a str { ... }
impl<'a> TokenStream for &'a str {

    type TokenType = char;
    type RangeType = &'a str; //Self;

    fn get(&self) -> Option<Self::TokenType>{
        return self.chars().next();
    }
}

use std::str::Chars;

impl<'a> IterableTokenStream<Chars<'a>> for &'a str {
    fn iter(&self) -> Chars<'a> {
        return self.chars()
    }
}

// use std::str::CharIndices;
// impl<'a> IterableTokenStream<CharIndices<'a>> for &'a str {
//     fn iter(&self) -> CharIndices<'a> {
//         return self.char_indices()
//     }
// }

// ================================ trait Parser ================================
pub type ParseErrorType =  ();
pub type ParseResult<D, T> = (Result<D, ParseErrorType>, T);

pub trait Parser {
    type ParsedDataType;
    type Tokens: TokenStream;

    // type ParseResult = Result<Self::ParsedDataType, ParseErrorType>;
    fn parse(&self, tokens: Self::Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens>;
        // (Result<Self::ParsedDataType, ParseErrorType>, Self::Tokens);

    fn map<F>(self, f: F) -> MapedParser<F, Self> where Self: Sized {
        MapedParser {
            f: f,
            parser: self
        }
    }
}


// ================================ MapedParser ================================

pub struct MapedParser<F, P> {
    f: F,
    parser: P,
}

impl<F, B, P, Tokens> Parser for MapedParser<F ,P>
where
    P: Parser<Tokens=Tokens>,
    F: Fn(P::ParsedDataType) -> B,
    Tokens: TokenStream
{
    type ParsedDataType = B;
    type Tokens = Tokens;

    fn parse(&self, tokens: Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens> {
        let orgiginal = self.parser.parse(tokens);

        return (orgiginal.0.map(&self.f), orgiginal.1);
    }
}

// ================================ OR ================================
struct Or<P1,P2>
where P1: Parser,
      P2: Parser,
{
    first: P1,
    second: P2
}

impl<R, T, P1, P2> Parser for Or<P1, P2>
where P1: Parser<Tokens=T, ParsedDataType=R>,
      P2: Parser<Tokens=T, ParsedDataType=R>,
      T: TokenStream + Clone
{
    type ParsedDataType = R;
    type Tokens = T;

    fn parse(&self, tokens: Self::Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens> {

        // TODO without clone ?
        let res = self.first.parse(tokens.clone());
        if res.0.is_ok() {
            return res;
        }
        return self.second.parse(tokens);
    }
}



// ================================ PredicateParser ================================

// хранить замыкание в поле структуры или создать trait и каждый раз перегружать метод?
struct PredicateParser<F, T>
    where F: Fn(T) -> bool {
    valid_cheker: F,
    _phantom: PhantomData<T>
}

impl<F, T> PredicateParser<F, T>
    where F: Fn(T) -> bool
{
    fn new(f: F) -> Self {
        PredicateParser { valid_cheker: f, _phantom: PhantomData }
    }

    fn is_valid(&self,arg: T) -> bool {
        (&self.valid_cheker)(arg)
    }
}

// ================================ CharsParser ================================
type CharsParser<'a, F> = PredicateParser<F, &'a char>;

impl<'a, F> Parser for CharsParser<'a, F>
where F: Fn(&char) -> bool {
// where F: for<'r> Fn(&'r char) -> bool {

    type ParsedDataType = String;
    type Tokens = &'a str;

    fn parse(&self, tokens: &'a str) -> ParseResult<Self::ParsedDataType, Self::Tokens> {

        let mut final_parsed_offset = 0;
        let res_str = tokens.char_indices()
            // .take(2)
            .take_while(|&(_,c)| self.is_valid(&c))
            .map(|(i,c)| { final_parsed_offset = i+1; return c; })
            .collect::<String>();

        if final_parsed_offset > 0 {
           return (Ok(res_str), &tokens[final_parsed_offset..]);
        }
        return (Err(ParseErrorType::default()), tokens);
    }
}


// ================================ main ================================


fn main() {
}

#[test]
fn or_test() {


    #[derive(Debug, PartialEq)]
    enum NumOrString {
        Num(i32),
        Str(String),
    }

    let num_parser = CharsParser::new(|c:&char| c.is_numeric());

    let uppercase_parser = CharsParser::new(|c:&char| c.is_uppercase());

    let num_or_uppercase = Or {
        first: num_parser.map(|s: String| NumOrString::Num(s.parse::<i32>().unwrap())),
        second: uppercase_parser.map(|s: String| NumOrString::Str(s)),
    };


    let test_list = &[
        ("633XA",(Ok(NumOrString::Num(633)),"XA")),
        ("XA5",(Ok(NumOrString::Str("XA".to_string())),"5")),
        ("633",(Ok(NumOrString::Num(633)),"")),
        ("d5A",(Err(()),"d5A")),
        ("6A33xa",(Ok(NumOrString::Num(6)),"A33xa")),
        ("FOO", (Ok(NumOrString::Str("FOO".to_string())),"")),
    ];

    for t in test_list {
        let res = num_or_uppercase.parse(t.0);
        println!("{} {:?}", t.0, res);
        assert_eq!(res, t.1);
    }


}

#[test]
fn num_parser_test() -> () {
    let num_parser = CharsParser::new(|c:&char| c.is_numeric());

    let num_parser_num = num_parser
        .map(|x:String| x.parse::<i32>().unwrap());
    
    let test_list = &[
        ("633xa",(Ok(633),"xa")),
        ("-1",(Err(()),"-1")),
        ("633",(Ok(633),"")),
        ("a633xa",(Err(()),"a633xa")),
        ("6_33xa",(Ok(6),"_33xa")),
        ("s633a", (Err(()),"s633a")),
    ];

    for t in test_list {
        let res = num_parser_num.parse(t.0);
        println!("{} {:?}", t.0, res);
        assert_eq!(res, t.1);
    }
}
