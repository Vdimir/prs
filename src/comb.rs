// *
// *
// *

use pars::{Parser, ParseResult, TokenStream};

// ================================ MapedParser ================================
pub struct MapedParser<F, P> {
    f: F,
    parser: P,
}

impl<F, B, P, Tokens> Parser<Tokens> for MapedParser<F, P>
    where P: Parser<Tokens>,
          F: Fn(P::ParsedDataType) -> B,
          Tokens: TokenStream
{
    type ParsedDataType = B;

    fn parse(&self, tokens: Tokens) -> ParseResult<Self::ParsedDataType, Tokens> {
        let orgiginal = self.parser.parse(tokens);

        return (orgiginal.0.map(&self.f), orgiginal.1);
    }
}

// ================================ OR ================================
pub struct Or<P1, P2> {
    first: P1,
    second: P2,
}

impl<R, T, P1, P2> Parser<T> for Or<P1, P2>
    where P1: Parser<T, ParsedDataType = R>,
          P2: Parser<T, ParsedDataType = R>,
          T: TokenStream + Clone
{
    type ParsedDataType = R;
    // type Tokens = T;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {

        // TODO without clone ?
        let res = self.first.parse(tokens.clone());
        if res.0.is_ok() {
            return res;
        }
        return self.second.parse(tokens);
    }
}

// ================================ Seq ================================
pub struct And<P1, P2> {
    first: P1,
    second: P2,
}

impl<T, P1, P2> Parser<T> for And<P1, P2>
    where P1: Parser<T>,
          P2: Parser<T>,
          T: TokenStream + Clone
{
    type ParsedDataType = (P1::ParsedDataType, P2::ParsedDataType);

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {

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
pub struct Skip<P1, P2> {
    actual: P1,
    skiped: P2,
}

impl<T, P1, P2> Parser<T> for Skip<P1, P2>
    where P1: Parser<T>,
          P2: Parser<T>,
          T: TokenStream + Clone
{
    type ParsedDataType = P1::ParsedDataType;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let (first_res, other) = self.actual.parse(tokens.clone());
        if first_res.is_ok() {
            let (second_res, other2) = self.skiped.parse(other);
            if second_res.is_ok() {
                return (Ok(first_res.unwrap()), other2);
            }
        }
        return (Err(()), tokens);
    }
    // fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
    //         let (res, other) = (&self.actual).and(&self.skiped).parse(tokens.clone());
    //     if res.is_ok() {
    //         return (Ok(res.unwrap().0), other);
    //     }
    //     return (Err(()), tokens);
    // }
}

// ================================ Repetition ================================
pub struct Rep<P> {
    parser: P,
}

impl<T: TokenStream, P> Parser<T> for Rep<P> where P: Parser<T>
{
    type ParsedDataType =  Box<[P::ParsedDataType]>;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {

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

// ================================ Mabye ================================
pub struct Mabye<P> {
    parser: P,
}

impl<T: TokenStream, P> Parser<T> for Mabye<P> where P: Parser<T>
{
    type ParsedDataType = Option<P::ParsedDataType>;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let (res, other) = self.parser.parse(tokens);

        // if self.parser wont return original tokens on error,
        // these result would be wrong
        return (Ok(res.ok()), other);
    }
}


// ================================ ParserComb ================================
pub trait ParserComb<T>: Parser<T> where T : TokenStream  {
    fn or<P>(self, parser: P) -> Or<Self, P>
        where P: Parser<T, ParsedDataType = Self::ParsedDataType>
    {
        Or {
            first: self,
            second: parser,
        }
    }

    fn and<P>(self, parser: P) -> And<Self, P>
        where P: Parser<T>
    {
        And {
            first: self,
            second: parser,
        }
    }

    fn skip<P>(self, parser: P) -> Skip<Self, P>
        where P: Parser<T>
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

pub fn maybe<T, P>(parser: P) -> Mabye<P>
    where P: Parser<T>,
          T: TokenStream
{
    Mabye { parser: parser }
}

pub fn rep<T, P>(parser: P) -> Rep<P>
    where P: Parser<T>,
          T: TokenStream
{
    Rep { parser: parser }
}

impl<T, P> ParserComb<T> for P
    where T: TokenStream,
          P: Parser<T>
{}
