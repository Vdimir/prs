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

    fn get(&self) -> Option<Self::TokenType> {
        return self.chars().next();
    }
}

use std::str::Chars;

impl<'a> IterableTokenStream<Chars<'a>> for &'a str {
    fn iter(&self) -> Chars<'a> {
        return self.chars();
    }
}

// use std::str::CharIndices;
// impl<'a> IterableTokenStream<CharIndices<'a>> for &'a str {
//     fn iter(&self) -> CharIndices<'a> {
//         return self.char_indices()
//     }
// }

// ================================ trait Parser ================================
pub type ParseErrorType = ();
pub type ParseResult<D, T> = (Result<D, ParseErrorType>, T);

pub trait Parser: Sized {
    type ParsedDataType;
    type Tokens: TokenStream;
    // I wanna:
    // type ParseResult = (Result<Self::ParsedDataType, ParseErrorType>, Self::Tokens);

    fn parse(&self, tokens: Self::Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens>;
}

trait ParserComb: Parser {
    fn or<P>(self, parser: P) -> Or<Self, P>
        where P: Parser<Tokens = Self::Tokens, ParsedDataType = Self::ParsedDataType>
    {
        Or {
            first: self,
            second: parser,
        }
    }

    fn and<P>(self, parser: P) -> And<Self, P>
        where P: Parser<Tokens = Self::Tokens>
    {
        And {
            first: self,
            second: parser,
        }
    }

    fn skip<P>(self, parser: P) -> Skip<Self, P>
        where P: Parser<Tokens = Self::Tokens>
    {
        Skip {
            actual: self,
            skiped: parser,
        }
    }

    fn map<F, B>(self, f: F) -> MapedParser<F, Self>
        where Self: Sized,
              F: Fn(Self::ParsedDataType) -> B
    {
        MapedParser {
            f: f,
            parser: self,
        }
    }
}

impl<P: Parser> ParserComb for P {}

// TODO CHECK THIS
impl<'a, I, O, P: ?Sized> Parser for &'a P
    where I: TokenStream,
          P: Parser<ParsedDataType = O, Tokens = I>
{
    type ParsedDataType = O;
    type Tokens = I;

    fn parse(&self, tokens: Self::Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens> {
        (*self).parse(tokens)
    }
}


// ================================ MapedParser ================================

pub struct MapedParser<F, P> {
    f: F,
    parser: P,
}

impl<F, B, P, Tokens> Parser for MapedParser<F, P>
    where P: Parser<Tokens = Tokens>,
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
pub struct Or<P1, P2>
    where P1: Parser,
          P2: Parser
{
    first: P1,
    second: P2,
}

impl<R, T, P1, P2> Parser for Or<P1, P2>
    where P1: Parser<Tokens = T, ParsedDataType = R>,
          P2: Parser<Tokens = T, ParsedDataType = R>,
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


// ================================ Seq ================================
pub struct And<P1, P2>
    where P1: Parser,
          P2: Parser
{
    first: P1,
    second: P2,
}

impl<T, P1, P2> Parser for And<P1, P2>
    where P1: Parser<Tokens = T>,
          P2: Parser<Tokens = T>,
          T: TokenStream + Clone
{
    type ParsedDataType = (P1::ParsedDataType, P2::ParsedDataType);
    type Tokens = T;

    fn parse(&self, tokens: Self::Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens> {

        let (first_res, other) = self.first.parse(tokens.clone());
        if first_res.is_ok() {
            let (second_res, other2) = self.second.parse(other);
            if second_res.is_ok() {
                return (Ok((first_res.unwrap(), second_res.unwrap())), other2);
            }
        }
        return (Err(()), tokens);
    }
}


// ================================ Skip ================================
pub struct Skip<P1, P2>
    where P1: Parser,
          P2: Parser
{
    actual: P1,
    skiped: P2,
}

impl<T, P1, P2> Parser for Skip<P1, P2>
    where P1: Parser<Tokens = T>,
          P2: Parser<Tokens = T>,
          T: TokenStream + Clone
{
    type ParsedDataType = P1::ParsedDataType;
    type Tokens = T;

    fn parse(&self, tokens: Self::Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens> {
        let (res, other) = (&self.actual).and(&self.skiped).parse(tokens.clone());

        if res.is_ok() {
            return (Ok(res.unwrap().0), other);
        }
        return (Err(()), tokens);
    }
}

// ================================ Repetition ================================
pub struct Rep<P> {
    parser: P,
}

impl<P> Parser for Rep<P> where P: Parser
{
    type ParsedDataType =  Box<[P::ParsedDataType]>;
    type Tokens = P::Tokens;

    fn parse(&self, tokens: Self::Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens> {

        let mut results = Vec::new();
        let mut other = tokens;
        loop {
            let parse_res = self.parser.parse(other);
            let res = parse_res.0;
            other = parse_res.1;
            if !res.is_ok() {
                break;
            }
            results.push(res.unwrap());
        }
        if results.len() > 0 {
            return (Ok(results.into_boxed_slice()), other);
        }
        return (Err(()), other);
    }
}



// ================================ PredicateParser ================================

// хранить замыкание в поле структуры или создать trait и каждый раз перегружать метод?
struct PredicateParser<F, T>
    where F: Fn(T::TokenType) -> bool,
          T: TokenStream
{
    valid_cheker: F,
    _phantom: PhantomData<T>,
}

impl<F, T> PredicateParser<F, T>
    where F: Fn(T::TokenType) -> bool,
          T: TokenStream
{
    fn new(f: F) -> Self {
        PredicateParser {
            valid_cheker: f,
            _phantom: PhantomData,
        }
    }

    fn is_valid(&self, arg: T::TokenType) -> bool {
        (&self.valid_cheker)(arg)
    }
}

// ================================ CharsParser ================================
// type CharsParser<'a, F> = PredicateParser<F, &'a str>;

impl<'a, F> Parser for PredicateParser<F, &'a str>
    where F: Fn(<&'a str as TokenStream>::TokenType) -> bool
{
    type ParsedDataType = String;
    type Tokens = &'a str;

    fn parse(&self, tokens: Self::Tokens) -> ParseResult<Self::ParsedDataType, Self::Tokens> {

        let mut final_parsed_offset = 0;
        let res_str = tokens.char_indices()
                            .take_while(|&(_, c)| self.is_valid(c))
                            .map(|(i, c)| {
                                final_parsed_offset = i + 1;
                                return c;
                            })
                            .collect::<String>();

        if final_parsed_offset > 0 {
            return (Ok(res_str), &tokens[final_parsed_offset..]);
        }
        return (Err(ParseErrorType::default()), tokens);
    }
}


// ================================ main ================================


#[test]
fn or_test() {
    #[derive(Debug, PartialEq)]
    enum NumOrString {
        Num(i32),
        Str(String),
        Space,
    }

    let num_parser = PredicateParser::new(|c: char| c.is_numeric())
                         .map(|s: String| NumOrString::Num(s.parse::<i32>().unwrap()));

    let uppercase_parser = PredicateParser::new(|c: char| c.is_uppercase())
                               .map(|s| NumOrString::Str(s));

    let space_parser = PredicateParser::new(|c| c == ' ').map(|_| NumOrString::Space);

    let num_or_uppercase = num_parser.or(space_parser)
                                     .or(uppercase_parser);


    let test_list = &[("633XA", (Ok(NumOrString::Num(633)), "XA")),
                      ("XA5", (Ok(NumOrString::Str("XA".to_string())), "5")),
                      ("633", (Ok(NumOrString::Num(633)), "")),
                      (" 633", (Ok(NumOrString::Space), "633")),
                      ("   x ", (Ok(NumOrString::Space), "x ")),
                      ("d5A", (Err(()), "d5A")),
                      ("6A33xa", (Ok(NumOrString::Num(6)), "A33xa")),
                      ("FOO", (Ok(NumOrString::Str("FOO".to_string())), ""))];

    for t in test_list {
        let res = num_or_uppercase.parse(t.0);
        println!("{} {:?}", t.0, res);
        assert_eq!(res, t.1);
    }


}


#[test]
fn and_test() {

    let num_parser = PredicateParser::new(|c: char| c.is_numeric())
                         .map(|s: String| (s.parse::<i32>().unwrap()));
    {
        let uppercase_parser = PredicateParser::new(|c: char| c.is_uppercase())
                                   .map(|s: String| (s));


        let num_or_uppercase = num_parser.and(uppercase_parser);

        let test_list = &[("633XA", (Ok((633, "XA".to_string())), "")),
                          ("5", (Err(()), "5")),
                          ("633X", (Ok((633, "X".to_string())), "")),
                          ("XA", (Err(()), "XA")),
                          ("500FFbar", (Ok((500, "FF".to_string())), "bar")),
                          ("d5A", (Err(()), "d5A"))];

        for t in test_list {
            let res = num_or_uppercase.parse(t.0);
            println!("{} {:?}", t.0, res);
            assert_eq!(res, t.1);
        }
    }
}

#[test]
fn skip_test() {


    let num_parser = PredicateParser::new(|c: char| c.is_numeric())
                         .map(|s: String| s.parse::<i32>().unwrap());

    let uppercase_parser = PredicateParser::new(|c: char| c.is_uppercase()).map(|s| s);

    let space_parser = PredicateParser::new(|c| c == ' ').map(|_| ());

    let num_space_uppercase = num_parser.skip(space_parser)
                                        .and(uppercase_parser);


    let test_list = &[("633 XA", (Ok((633, "XA".to_string())), "")),
                      ("5", (Err(()), "5")),
                      ("633X", (Err(()), "633X")),
                      ("XA", (Err(()), "XA")),
                      ("500 FFbar", (Ok((500, "FF".to_string())), "bar"))];

    for t in test_list {
        let res = num_space_uppercase.parse(t.0);
        println!("{} {:?}", t.0, res);
        assert_eq!(res, t.1);
    }
}



#[test]
fn rep_test() {


    let num_parser = PredicateParser::new(|c: char| c.is_numeric())
                         .map(|s| s.parse::<i32>().unwrap());

    let space_parser = PredicateParser::new(|c| c == ' ').map(|_| ());

    let list_of_nums_sum = (Rep { parser: num_parser.skip(space_parser) })
                               .map(|x| x.iter().fold(0, |acc, &x| acc + x));

    let test_list = &[("5", (Err(()), "5")),
                      ("1 2 3 4 50  12 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ",
                       (Ok(100), "")),
                      ("633X", (Err(()), "633X")),
                      ("XA", (Err(()), "XA")),
                      ("500 20  bar", (Ok(520), "bar"))];

    for t in test_list {
        let res = list_of_nums_sum.parse(t.0);
        println!("{} {:?}", t.0, res);
        assert_eq!(res, t.1);
    }
}



#[test]
fn num_parser_test() -> () {
    let num_parser = PredicateParser::new(|c: char| c.is_numeric());

    let num_parser_num = num_parser.map(|x: String| x.parse::<i32>().unwrap());

    let test_list = &[("633xa", (Ok(633), "xa")),
                      ("-1", (Err(()), "-1")),
                      ("633", (Ok(633), "")),
                      ("a633xa", (Err(()), "a633xa")),
                      ("6_33xa", (Ok(6), "_33xa")),
                      ("s633a", (Err(()), "s633a"))];

    for t in test_list {
        let res = num_parser_num.parse(t.0);
        println!("{} {:?}", t.0, res);
        assert_eq!(res, t.1);
    }
}
