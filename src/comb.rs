// *
// *
// *

use pars::{Parse, ParseResult, TokenStream};

// ================================ MapedParser ================================
pub struct MapedParser<F, P> {
    f: F,
    parser: P,
}

impl<F, B, P, T> Parse<T> for MapedParser<F, P>
    where P: Parse<T>,
          F: Fn(P::ParsedDataType) -> B,
          T: TokenStream
{
    type ParsedDataType = B;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        return self.parser.parse(tokens).map(&self.f);
    }
}

// ================================ OR ================================
pub struct Or<P1, P2> {
    first: P1,
    second: P2,
}

impl<R, T, P1, P2> Parse<T> for Or<P1, P2>
    where P1: Parse<T, ParsedDataType = R>,
          P2: Parse<T, ParsedDataType = R>,
          T: TokenStream + Clone
{
    type ParsedDataType = R;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {

        let first_res = self.first.parse(tokens);
        if first_res.is_ok() {
            return first_res;
        }
        // on fail previous parser should return orgiginal tokens
        return self.second.parse(first_res.other);
    }
}

// ================================ Seq ================================
pub struct And<P1, P2> {
    first: P1,
    second: P2,
}

impl<T, P1, P2> Parse<T> for And<P1, P2>
    where P1: Parse<T>,
          P2: Parse<T>,
          T: TokenStream + Clone
{
    type ParsedDataType = (P1::ParsedDataType, P2::ParsedDataType);

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {

        let ParseResult { res:first_res, other } = self.first.parse(tokens);

        if !first_res.is_ok() {
            return ParseResult::fail((), other);
        }

        let ParseResult { res:second_res, other } = self.second.parse(other);
        if !second_res.is_ok() {
            return ParseResult::fail((), other);
        }

        return ParseResult::succ((first_res.unwrap(), second_res.unwrap()), other);
    }
}

// ================================ Skip ================================
pub struct Skip<P1, P2> {
    actual: P1,
    skiped: P2,
}

impl<T, P1, P2> Parse<T> for Skip<P1, P2>
    where P1: Parse<T>,
          P2: Parse<T>,
          T: TokenStream + Clone
{
    type ParsedDataType = P1::ParsedDataType;


    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let res = (&self.actual).and(&self.skiped).parse(tokens);
        return res.map(|(a, _)| a);
    }
}

// ================================ Repetition ================================
pub struct Rep<P> {
    parser: P,
}

impl<T: TokenStream, P> Parse<T> for Rep<P> where P: Parse<T>
{
    type ParsedDataType =  Box<[P::ParsedDataType]>;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {

        let mut results = Vec::new();
        let mut other = tokens;
        loop {
            let parse_res = self.parser.parse(other);
            other = parse_res.other;
            let res = parse_res.res;
            if !res.is_ok() {
                break;
            }
            results.push(res.unwrap());
        }
        if results.len() > 0 {
            return ParseResult::succ(results.into_boxed_slice(), other);
        }
        return ParseResult::fail((), other);
    }
}

// ================================ Mabye ================================
pub struct Mabye<P> {
    parser: P,
}

impl<T: TokenStream, P> Parse<T> for Mabye<P> where P: Parse<T>
{
    type ParsedDataType = Option<P::ParsedDataType>;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let ParseResult {res, other} = self.parser.parse(tokens);

        return ParseResult::succ(res.ok(), other);
    }
}


// ================================ ParserComb ================================
pub trait ParserComb<T>: Parse<T> where T : TokenStream, Self: Sized  {
    fn or<P>(self, parser: P) -> Or<Self, P>
        where P: Parse<T, ParsedDataType = Self::ParsedDataType>
    {
        Or {
            first: self,
            second: parser,
        }
    }

    fn and<P>(self, parser: P) -> And<Self, P>
        where P: Parse<T>
    {
        And {
            first: self,
            second: parser,
        }
    }

    fn skip<P>(self, parser: P) -> Skip<Self, P>
        where P: Parse<T>
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
    where P: Parse<T>,
          T: TokenStream
{
    Mabye { parser: parser }
}

pub fn rep<T, P>(parser: P) -> Rep<P>
    where P: Parse<T>,
          T: TokenStream
{
    Rep { parser: parser }
}

impl<T, P> ParserComb<T> for P
    where T: TokenStream,
          P: Parse<T>
{}
