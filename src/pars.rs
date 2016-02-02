// *
// *
// *

use std::marker::PhantomData;
// ================================ TokenStream ================================
pub trait Verify<T> {
    fn satisfies(&self, arg: &T) -> bool;
}

pub struct IsEqual<'a, T: 'a>(&'a T);

#[derive(Clone)]
pub struct IsEqualOwn<T>(T);

impl<'a, T: PartialEq> Verify<T> for IsEqual<'a, T> {
    fn satisfies(&self, arg: &T) -> bool {
        arg == self.0
    }
}

impl<'a, T: PartialEq> Verify<T> for IsEqualOwn<T> {
    fn satisfies(&self, arg: &T) -> bool {
        *arg == self.0
    }
}

pub struct Predicate<T, F>(F, PhantomData<T>);

impl<T, F> Verify<T> for Predicate<T, F> where F: Fn(&T) -> bool
{
    fn satisfies(&self, arg: &T) -> bool {
        (self.0)(arg)
    }
}

pub trait TokenStream: Sized {
    type TokenType;

    fn look_ahead(&self) -> Option<Self::TokenType>;
    fn get(self) -> (Option<Self::TokenType>, Self);
    fn get_if<'a, C>(self, condition: &C) -> (Option<Self::TokenType>, Self)
        where C: Verify<Self::TokenType>
    {
        let next_token = self.look_ahead();

        if next_token.is_some() {
            if condition.satisfies(&next_token.unwrap()) {
                return self.get();
            }
        }
        (None, self)
    }
}

// pub trait IterableTokenStream<It>: TokenStream
//     where It: Iterator<Item=Self::TokenType> {
//     fn iter(&self) -> It;
// }

pub trait RangeTokenStream: TokenStream
{
    fn get_while<C>(self, condition: &C) -> (Option<Self>, Self) where C: Verify<Self::TokenType>;
}


impl<'a> TokenStream for &'a str {
    type TokenType = char;

    fn look_ahead(&self) -> Option<Self::TokenType>{
        self.chars().next()
    }

    fn get(self) -> (Option<Self::TokenType>, Self) {
        match self.chars().next() {
            Some(c) => (Some(c), &self[c.len_utf8()..]),
            None => (None, self),
        }
    }
}

impl<'a> RangeTokenStream for &'a str {
    fn get_while<C>(self, condition: &C) -> (Option<Self>, Self)
        where C: Verify<Self::TokenType>
    {
        let offset = self.chars()
                         .take_while(|c| condition.satisfies(c))
                         .fold(0, |len, c: char| len + c.len_utf8());
        if offset == 0 {
            return (None, &self[offset..]);
        }
        return (Some(&self[..offset]), &self[offset..]);
    }
}

// struct IterableTokens<T>(T);
// 
// impl<T> TokenStream for IterableTokens<T>
// where T: Iterator {
//     type TokenType = T::Item;
// 
//     fn look_ahead(&self) -> Option<Self::TokenType> {
// 
//     }
// 
//     fn get(self) -> Option<(Self::TokenType, Self)> {
//         unimplemented!();
//         // match self.chars().next() {
//             // Some(c) => Some((c, &self[c.len_utf8()..])),
//             // None => None,
//         // }
//     }
// }

// ================================ ParseResult ================================
#[derive(Debug)]
pub enum ParseError {
    Expected(ParserType),
}

#[derive(Debug)]
pub enum ParserType {
    Unknown,
    Named(String),
    Or(Box<ParserType>, Box<ParserType>)
}


pub struct ParseResult<R, S > {
    pub res: Result<R, ParseError>,
    pub other: S,
}

impl<R, S> ParseResult<R, S> {
    pub fn succ(res: R, other: S) -> Self {
        ParseResult {
            res: Ok(res),
            other: other,
        }
    }

    pub fn fail(error: ParseError, other: S) -> Self {
        ParseResult {
            res: Err(error),
            other: other,
        }
    }

    pub fn map<U, F>(self, op: F) -> ParseResult<U, S>
        where F: FnOnce(R) -> U
    {
        ParseResult {
            res: self.res.map(op),
            other: self.other,
        }
    }

    pub fn is_ok(&self) -> bool {
        self.res.is_ok()
    }
}
// ================================ trait Parse ================================

// T is type of input
pub trait Parse<T>: Sized {
    type ParsedDataType;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T>;
    fn parser_type(&self) -> ParserType {
        ParserType::Unknown
    }
}


//where T: TokenStream ,C: Verify<T::TokenType>
#[derive(Clone)]
pub struct Parser<C, T>
{
    name: Option<String>,
    checker: C,
    _p: PhantomData<T>
}

impl<C, T: TokenStream> Parser<C, T> {
    pub fn greedy(self) -> GrParser<C, T> {
        GrParser { checker: self.checker, _p: PhantomData }
    }
}

impl<T, C> Parse<T> for Parser<C, T>
    where T: TokenStream,
          C: Verify<T::TokenType>
{
    type ParsedDataType = T::TokenType;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let (res_opt, other) = tokens.get_if(&self.checker);
        if res_opt.is_some() {
            let res = res_opt.unwrap();
            return ParseResult::succ(res, other);
        }
        return ParseResult::fail(ParseError::Expected(self.parser_type()), other);
    }

    fn parser_type(&self) -> ParserType {
        match self.name {
            Some(ref name) => ParserType::Named(name.clone()),
            None => ParserType::Unknown,
        }
    }
}

#[derive(Clone)]
pub struct GrParser<C, T: TokenStream>{
    checker: C,
    _p: PhantomData<T>
}

impl<T, C> Parse<T> for GrParser<C, T>
    where T: RangeTokenStream,
          C: Verify<T::TokenType>
{
    type ParsedDataType = T;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let (res, other) = tokens.get_while(&self.checker);
        if let Some(parsed) = res {
            return ParseResult::succ(parsed, other);
        }
        return ParseResult::fail(ParseError::Expected(self.parser_type()), other);
    }
}

pub struct FnParser<F>(F);
// where F:Fn(T) -> ParseResult<R, T>;

impl<T, R, F> Parse<T> for FnParser<F> where T:TokenStream, F:Fn(T) -> ParseResult<R, T>
{
    type ParsedDataType = R;

    fn parse(&self, tokens: T) -> ParseResult<R, T> {
        self.0(tokens)
    }
}

pub fn pred<T: TokenStream, F>(f: F) -> Parser<Predicate<T::TokenType, F>, T> {
    Parser { name: None, checker: Predicate(f, PhantomData), _p: PhantomData }
}

pub fn token<T: TokenStream>(t: T::TokenType) -> Parser<IsEqualOwn<T::TokenType>, T>
where T::TokenType: ToString {
    Parser { name: Some(t.to_string()), checker: IsEqualOwn(t), _p: PhantomData }
}


pub fn fn_parser<T,R,F> (f:F) -> FnParser<F> where T:TokenStream, F:Fn(T) -> ParseResult<R, T>
{
    FnParser(f)
}



// ================================ impl Parse for ref ================================
// TODO CHECK THIS
impl<'a, I, O, P> Parse<I> for &'a P
    where I: TokenStream,
          P: Parse<I, ParsedDataType = O>
{
    type ParsedDataType = O;

    fn parse(&self, tokens: I) -> ParseResult<Self::ParsedDataType, I> {
        (*self).parse(tokens)
    }
}
