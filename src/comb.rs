// *
// *
// *

use pars::{Parse, ParseResult, ParseError, ParserType, TokenStream};

// ================================ MapedParser ================================
#[derive(Clone)]
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
#[derive(Clone)]
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

        let (res, other) = self.first.parse(tokens).to_tuple();
        if res.is_ok() {
            return ParseResult::succ(res.unwrap(), other);
        }

        // NOTE: previous parser should return orgiginal tokens
        let (res, other) = self.second.parse(other).to_tuple();
        if res.is_ok() {
            return ParseResult::succ(res.unwrap(), other);
        }

        return ParseResult::fail(
            ParseError::Expected(self.parser_type()),
            other);
    }

    fn parser_type(&self) -> ParserType {
            ParserType::Or(
                Box::new(self.first.parser_type()),
                Box::new(self.second.parser_type())
            )
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

        // TODO without clone?
        let ParseResult { res: first_res, other } = self.first.parse(tokens.clone());

        if !first_res.is_ok() {
            return ParseResult::fail(
                ParseError::Expected(self.first.parser_type()),
                other);
        }

        let ParseResult { res: second_res, other } = self.second.parse(other);
        if !second_res.is_ok() {
            return ParseResult::fail(
                ParseError::Expected(self.second.parser_type()),
                tokens);
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
    type ParsedDataType =  Vec<P::ParsedDataType>;

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
            return ParseResult::succ(results, other);
        }
        return ParseResult::fail(ParseError::Expected(self.parser.parser_type()),other);
    }
}


// ================================ Iter ================================
pub struct Iter< P, I> {
    parser: P,
    pub input: I
}

impl<P, I> Iterator for Iter<P, I>
where P:Parse<I> , I : Clone{

    type Item = P::ParsedDataType;
    fn next(&mut self) -> Option<Self::Item> {
        // clone !!!
        let res = self.parser.parse(self.input.clone());
        if !res.is_ok() {
            return None;
        }
        self.input = res.other;
        return Some(res.res.unwrap());
    }
}

pub fn iterate_parser_over_input<P,I>(p: P, inp: I) -> Iter<P, I>
where P:Parse<I> {
    Iter { parser: p, input: inp}
}


// ================================ IterRep ================================
use std::iter::FromIterator;
use std::marker::PhantomData;

pub struct IterRep<P, R> {
    parser: P,
    _res_collection: PhantomData<R>
}

impl<T: TokenStream + Clone, P, R> Parse<T> for IterRep<P, R> where P: Parse<T>,
R: FromIterator<P::ParsedDataType>
{
    type ParsedDataType =  R;

    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let first_res = self.parser.parse(tokens.clone());
        if !first_res.is_ok() {
            return ParseResult::fail(ParseError::Expected(self.parser_type()),tokens);
        }
        let first = first_res.res.unwrap();

        let mut it = iterate_parser_over_input(&self.parser, first_res.other);


        let results: R = Some(first)
                        .into_iter()
                        .chain(it.by_ref()).collect();
        let other = it.input;

        return ParseResult::succ(results, other);

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

pub fn rep<T, P, R>(parser: P) -> IterRep<P, R>
    where P: Parse<T>,
          T: TokenStream
{
    IterRep { parser: parser, _res_collection: PhantomData }
}

// pub fn rep<T, P>(parser: P) -> Rep<P>
//     where P: Parse<T>,
//           T: TokenStream
// {
//     Rep { parser: parser}
// }

impl<T, P> ParserComb<T> for P
    where T: TokenStream,
          P: Parse<T>
{}
