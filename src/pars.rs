// *
// *
// *

use std::marker::PhantomData;
use stream::TokenStream;

// ========================================= Parse Trait ==========================================


pub trait Parse {
    type Input: TokenStream;
    type Output;
    type Error;

    fn parse(&self, &mut Self::Input) -> Result<Self::Output, Self::Error>;
}


// // TODO CHECK THIS
// impl<'a, I, O, P> Parse<I> for &'a P
//     where I: TStream,
//           P: Parse<I, ResultType = O>
// {
//     type ResultType = O;
//     fn parse(&self, tokens: I) -> ParseResult<Self::ResultType, I> {
//         (*self).parse(tokens)
//     }
// }

// ==================================== Parse Implementations =====================================

// ------------------------------------------- FnParser -------------------------------------------
pub struct FnParser<F, I>(F, PhantomData<I>);

impl<S, R, E, F> Parse for FnParser<F, S>
    where S: TokenStream,
          F: Fn(&mut S) -> Result<R, E>
{
    type Input = S;
    type Output = R;
    type Error = E;

    fn parse(&self, tokens: &mut S) -> Result<R, E> {
        self.0(tokens)
    }
}

// pub fn fn_parser<I, R, E, F>(f: F) -> FnParser<F>
//     where I: TokenStream,
//           F: Fn(I) -> Result<R, E>
// {
//     FnParser(f, PhantomData)
// }



// ====================================================================
use result::{ExpectedButFound, Expected};

pub struct Token<S: TokenStream>(pub S::Token);

impl<S> Parse for Token<S>
    where S: TokenStream,
          S::Token: PartialEq + Copy
{
    type Input = S;
    type Output = S::Token;
    type Error = Expected<S::Token>;

    fn parse(&self, tokens: &mut S) -> Result<Self::Output, Self::Error> {
        let next_token = tokens.peek();
        let satified = next_token.map_or(false, |t| t == self.0);

        if satified {
            Ok(tokens.next().unwrap())
        } else {
            Err(Expected(self.0))
        }
    }
}




// impl<T> PParse for Token<T>
//     where T: TStream,
//           T::TokenType: PartialEq
// {
//     type Input = T;
//     type ResultType = T::TokenType;
//     fn parse(&self, mut tokens: T) -> ParseResult<Self::ResultType, T> {
//         let satified = tokens.peek()
//                              .map_or(false, |t| t == self.0);
//         if satified {
//             finish_ok!(tokens.next().unwrap(), tokens);
//         }
//         finish_err!(ParseError::expected("token"), tokens);
//     }
// }

// ====================================================================
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

// /// Parses wraps any type implements `Verify` for parse satisfied tokens
// #[derive(Clone)]
// pub struct Parser<C, T> {
//     name: Option<String>,
//     checker: C,
//     _p: PhantomData<T>,
// }

// impl<C,T> Parser<C,T> {
//     fn parser_type(&self) -> ParserType {
//         match self.name {
//             Some(ref name) => ParserType::Named(name.clone()),
//             None => ParserType::Named("NA".to_owned()),
//         }
//     }
// }

// impl<T, C> Parse<T> for Parser<C, T>
//     where T: TStream,
//           C: Verify<T::TokenType>
// {
//     type ResultType = T::TokenType;
//     fn parse(&self, tokens: T) -> ParseResult<Self::ResultType, T> {
//         let (res_opt, other) = tokens.get_if(&self.checker);
//         if res_opt.is_some() {
//             let res = res_opt.unwrap();
//             finish_ok!(res, other);
//         }
//         finish_err!(Expected(self.parser_type()), other);
//     }
// }

// impl<C, T: TStream> Parser<C, T> {
//     pub fn greedy(self) -> GrParser<C, T> {
//         GrParser {
//             checker: self.checker,
//             _p: PhantomData,
//         }
//     }
// }

// #[derive(Clone)]
// pub struct GrParser<C, T: TStream> {
//     checker: C,
//     _p: PhantomData<T>,
// }

// impl<T, C> Parse<T> for GrParser<C, T>
//     where T: RangeTStream,
//           C: Verify<T::TokenType>
// {
//     type ResultType = T::RangeType;
//     fn parse(&self, tokens: T) -> ParseResult<Self::ResultType, T> {
//         let (res, other) = tokens.get_while(&self.checker);
//         if let Some(parsed) = res {
//             finish_ok!(parsed, other);
//         }
//         finish_err!(ParseError::Unknown, other);
//     }
// }

// // ====================================== FactoryMethods ======================================
// pub fn pred<T: TStream, F>(f: F) -> Parser<Predicate<T::TokenType, F>, T>
//     where F: Fn(&T::TokenType) -> bool
// {
//     Parser {
//         name: None,
//         checker: Predicate(f, PhantomData),
//         _p: PhantomData,
//     }
// }

// pub fn named_pred<T: TStream, F>(name: &str, f: F) -> Parser<Predicate<T::TokenType, F>, T>
//     where F: Fn(&T::TokenType) -> bool
// {
//     Parser {
//         name: Some(name.to_owned()),
//         checker: Predicate(f, PhantomData),
//         _p: PhantomData,
//     }
// }

// pub fn token<T: TStream>(t: T::TokenType) -> Parser<EqualCheker<T::TokenType>, T>
//     where T::TokenType: ToString
// {
//     Parser {
//         name: Some(t.to_string()),
//         checker: EqualCheker(t),
//         _p: PhantomData,
//     }
// }

