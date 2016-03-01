// *
// *
// *

use std::marker::PhantomData;
use stream::TokenStream;

// ========================================= Parse Trait ==========================================

pub trait Parse {
    type Input;
    type Output;
    type Error;

    fn parse(&self, &mut Self::Input) -> Result<Self::Output, Self::Error>;
}
use std::rc::Rc;

impl<'a, I, O, P, E> Parse for &'a P
    where P: Parse<Input=I, Output = O, Error=E>,
          // I: TokenStream,
{
    type Input = I;
    type Output = O;
    type Error = E;
    fn parse(&self, tokens: &mut I) -> Result<O, E> {
        (**self).parse(tokens)
    }
}


impl<'a, I, O, P, E> Parse for Rc<P>
    where P: Parse<Input=I, Output = O, Error=E>,
          // I: TokenStream,
{
    type Input = I;
    type Output = O;
    type Error = E;
    fn parse(&self, tokens: &mut I) -> Result<O, E> {
        (**self).parse(tokens)
    }
}

// ==================================== Parse Implementations =====================================

// -------------------------------------------- Token ---------------------------------------------
use result::ParseErr;

pub struct Token<S: TokenStream>(pub S::Token);

impl<S> Parse for Token<S>
    where S: TokenStream,
          S::Token: PartialEq + Clone
{
    type Input = S;
    type Output = S::Token;
    type Error = ParseErr<S::Token>;

    fn parse(&self, tokens: &mut S) -> Result<Self::Output, Self::Error> {
        let next_token = tokens.peek();
        let satified = next_token.map_or(false, |t| t == self.0);

        if satified {
            Ok(tokens.next().unwrap())
        } else {
            Err(ParseErr::unexpected(tokens.peek()).at(tokens.position()))
        }
    }
}

// ------------------------------------------ Predicate -------------------------------------------
pub struct Predicate<F, S>
where S: TokenStream,
      F: Fn(&S::Token) -> bool
{
    predicate: F,
    _phantom: PhantomData<S>
}

impl<S, F> Parse for Predicate<F, S>
    where S: TokenStream,
          S::Token: Clone,
        F: Fn(&S::Token) -> bool
{
    type Input = S;
    type Output = S::Token;
    type Error = ParseErr<S::Token>;

    fn parse(&self, tokens: &mut S) -> Result<Self::Output, Self::Error> {
        let next_token = tokens.peek();
        let satified = next_token.map_or(false, |t| (self.predicate)(&t));

        if satified {
            Ok(tokens.next().unwrap())
        } else {
            Err(ParseErr::unexpected(tokens.peek()).at(tokens.position()))
        }
    }
}

pub fn predicate<F, T>(f: F) -> Predicate<F, T>
where T: TokenStream,
      F: Fn(&T::Token) -> bool,
{
    Predicate {
        predicate: f,
        _phantom: PhantomData
    }
}

// ------------------------------------------- FnParser -------------------------------------------
pub struct FnParser<F, I>(F, PhantomData<I>);

impl<S, R, E, F> Parse for FnParser<F, S>
    where F: Fn(&mut S) -> Result<R, E>
{
    type Input = S;
    type Output = R;
    type Error = E;

    fn parse(&self, tokens: &mut S) -> Result<R, E> {
        self.0(tokens)
    }
}

pub fn fn_parser<S, R, E, F>(f: F) -> FnParser<F, S>
    where F: Fn(&mut S) -> Result<R, E>
{
    FnParser(f, PhantomData)
}
