// *
// *
// *

use std::marker::PhantomData;
use std::default::Default;

// ================================ TokenStream ================================
pub trait Checker<T: TokenStream> {
    fn satisfies(&self, arg: &T::TokenType) -> bool;
}

pub trait TokenStream: Sized {
    type TokenType;

    fn get(&self) -> Option<(Self::TokenType, Self)>;
    fn get_if<C>(&self, condition: &C) -> Option<(Self::TokenType, Self)>
        where C: Checker<Self>
    {
        self.get().and_then(|s| {
            if condition.satisfies(&s.0) {
                Some(s)
            } else {
                None
            }
        })
    }
}

pub trait IterableTokenStream<It>: TokenStream
    where It: Iterator<Item=Self::TokenType> {
    fn iter(&self) -> It;
}

pub trait RangeTokenStream: TokenStream + Sized
{
    fn get_while<C>(&self, condition: &C) -> (Option<Self>, Self) where C: Checker<Self>;
}


impl<'a> TokenStream for &'a str {
    type TokenType = char;

    // TODO test
    fn get(&self) -> Option<(Self::TokenType, Self)> {
        match self.chars().next() {
            Some(c) => Some((c, &self[c.len_utf8()..])),
            None => None,
        }
    }
}

impl<'a> RangeTokenStream for &'a str {
    fn get_while<C>(&self, condition: &C) -> (Option<Self>, Self)
        where C: Checker<Self>
    {
        let parsed_offset = self.chars()
                                .take_while(|c| condition.satisfies(c))
                                .fold(0, |len, c: char| len + c.len_utf8());

        if parsed_offset > 0 {
            return (Some(&self[..parsed_offset]), &self[parsed_offset..]);
        }
        return (None, self);
    }
}

// ================================ trait Parser ================================
pub type ParseErrorType = ();
pub type ParseResult<D, T> = (Result<D, ParseErrorType>, T);

pub trait Parse<T: TokenStream>: Sized {
    // Default associated type not allowed yet
    // type Tokens = T;
    // type ParseResult = (Result<Self::ParsedDataType, ParseErrorType>, Self::Tokens);
    type ParsedDataType;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T>;
}

// TODO CHECK THIS
// impl<'a, I, O, P> Parse<I> for &'a P
//     where I: TokenStream,
//           P: Parse<I, ParsedDataType = O>
// {
//     type ParsedDataType = O;
//     // type Tokens = I;

//     fn parse(&self, tokens: I) -> ParseResult<Self::ParsedDataType, I> {
//         (*self).parse(tokens)
//     }
// }

pub struct PredicateChecker<F, T>
    where F: Fn(&T::TokenType) -> bool,
          T: TokenStream
{
    valid_cheker: F,
    _phantom: PhantomData<T>,
}

impl<F, T> Checker<T> for PredicateChecker<F, T>
    where F: Fn(&T::TokenType) -> bool,
          T: TokenStream
{
    fn satisfies(&self, arg: &T::TokenType) -> bool {
        (&self.valid_cheker)(arg)
    }
}


pub struct TokenChecker<T> {
    token: T,
}

impl<T> Checker<T> for TokenChecker<T::TokenType>
    where T: TokenStream,
          T::TokenType: PartialEq
{
    fn satisfies(&self, arg: &T::TokenType) -> bool {
        &self.token == arg
    }
}

pub struct Parser<C> {
    checker: C,
}

impl<C> Parser<C> {
    fn greedy(self) -> GreedyParser<C> {
        GreedyParser { checker: self.checker }
    }
}

pub struct GreedyParser<C> {
    checker: C,
}

impl<F, T> From<F> for Parser<PredicateChecker<F, T>>
    where T: TokenStream,
          F: Fn(&T::TokenType) -> bool
{
    fn from(f: F) -> Parser<PredicateChecker<F, T>> {
        Parser {
            checker: PredicateChecker {
                valid_cheker: f,
                _phantom: PhantomData,
            },
        }
    }
}

impl<T> From<T> for Parser<TokenChecker<T>> where T: TokenStream
{
    fn from(t: T) -> Parser<TokenChecker<T>> {
        Parser { checker: TokenChecker { token: t } }
    }
}

impl<C> Parser<C> {
    pub fn new<T>(t: T) -> Self
        where Self: From<T>
    {
        Parser::from(t)
    }
}


impl<C> GreedyParser<C> {
    pub fn new<T>(t: T) -> Self
        where Parser<C>: From<T>
    {
        Parser::from(t).greedy()
    }
}

// ================================ Parser ================================

impl<S, C> Parse<S> for GreedyParser<C>
    where C: Checker<S>,
          S: RangeTokenStream
{
    type ParsedDataType = S;

    fn parse(&self, tokens: S) -> ParseResult<Self::ParsedDataType, S> {
        let (res, other) = tokens.get_while(&self.checker);
        return (res.ok_or(ParseErrorType::default()), other);
    }
}


impl<S, C> Parse<S> for Parser<C>
    where C: Checker<S>,
          S: TokenStream
{
    type ParsedDataType = S::TokenType;

    fn parse(&self, tokens: S) -> ParseResult<Self::ParsedDataType, S> {
        let res_opt = tokens.get_if(&self.checker);
        if res_opt.is_none() {
            return (Err(ParseErrorType::default()), tokens);
        }
        let res = res_opt.unwrap();
        return (Ok(res.0), res.1);
    }
}
