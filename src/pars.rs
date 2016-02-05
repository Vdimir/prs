// *
// *
// *

use std::marker::PhantomData;
use stream::{TokenStream, RangeTokenStream};
use verify::*;
use result::*;
use result::ParseError::{Expected};
// =============================================================================
// ================================ Parse Trait ================================
// =============================================================================

// T is type of input
pub trait Parse<T>: Sized {
    type ParsedDataType;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T>;
    fn parser_type(&self) -> ParserType {
        ParserType::Unknown
    }
}

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

// =============================================================================
// =========================== Parse Implementations ===========================
// =============================================================================

pub struct FnParser<F>(F);

impl<T, R, F> Parse<T> for FnParser<F>
    where T: TokenStream,
          F: Fn(T) -> ParseResult<R, T>
{
    type ParsedDataType = R;

    fn parse(&self, tokens: T) -> ParseResult<R, T> {
        self.0(tokens)
    }
}

pub fn fn_parser<T, R, F>(f: F) -> FnParser<F>
    where T: TokenStream,
          F: Fn(T) -> ParseResult<R, T>
{
    FnParser(f)
}


// ====================================================================
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

#[derive(Clone)]
pub struct Parser<C, T> {
    name: Option<String>,
    checker: C,
    _p: PhantomData<T>,
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
            finish_ok!(res, other);
        }
        finish_err!(Expected(self.parser_type()), other);
    }

    fn parser_type(&self) -> ParserType {
        match self.name {
            Some(ref name) => ParserType::Named(name.clone()),
            None => ParserType::Unknown,
        }
    }
}

impl<C, T: TokenStream> Parser<C, T> {
    pub fn greedy(self) -> GrParser<C, T> {
        GrParser {
            checker: self.checker,
            _p: PhantomData,
        }
    }
}

#[derive(Clone)]
pub struct GrParser<C, T: TokenStream> {
    checker: C,
    _p: PhantomData<T>,
}

impl<T, C> Parse<T> for GrParser<C, T>
    where T: RangeTokenStream,
          C: Verify<T::TokenType>
{
    type ParsedDataType = T::RangeType;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let (res, other) = tokens.get_while(&self.checker);
        if let Some(parsed) = res {
            finish_ok!(parsed, other);
        }
        finish_err!(Expected(self.parser_type()), other);
    }
}


// ====================================== FactoryMethods ======================================
pub fn pred<T: TokenStream, F>(f: F) -> Parser<Predicate<T::TokenType, F>, T>
    where F: Fn(&T::TokenType) -> bool
{
    Parser {
        name: None,
        checker: Predicate(f, PhantomData),
        _p: PhantomData,
    }
}

pub fn named_pred<T: TokenStream, F>(name: &str, f: F) -> Parser<Predicate<T::TokenType, F>, T>
    where F: Fn(&T::TokenType) -> bool
{
    Parser {
        name: Some(name.to_owned()),
        checker: Predicate(f, PhantomData),
        _p: PhantomData,
    }
}

pub fn token<T: TokenStream>(t: T::TokenType) -> Parser<EqualCheker<T::TokenType>, T>
    where T::TokenType: ToString
{
    Parser {
        name: Some(t.to_string()),
        checker: EqualCheker(t),
        _p: PhantomData,
    }
}
