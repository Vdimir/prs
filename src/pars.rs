// *
// *
// *

use std::marker::PhantomData;
use stream::{TokenStream, RangeStream};
use result::{ParseErr, SupressedRes};


pub trait Parse {
    type Input;
    type Output;
    type Error;

    fn parse(&self, &mut Self::Input) -> Result<Self::Output, Self::Error>;
}

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

// -------------------------------------------- Token ---------------------------------------------

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


// ------------------------------------------ While -------------------------------------------
pub struct While<F, S>
where S: RangeStream,
      F: Fn(&S::Token) -> bool
{
    predicate: F,
    _phantom: PhantomData<S>
}

impl<S, F> Parse for While<F, S>
    where S: RangeStream,
        F: Fn(&S::Token) -> bool
{
    type Input = S;
    type Output = S::Range;
    type Error = ParseErr<S::Token>;

    fn parse(&self, tokens: &mut S) -> Result<Self::Output, Self::Error> {
        let lexem_beg = tokens.save();
        while tokens.peek().map_or(false, |t| (self.predicate)(&t) ) {
            tokens.next();
        }
        tokens.range(lexem_beg)
            .ok_or( ParseErr::unexpected(tokens.peek()).at(tokens.position()))
    }
}

pub fn parse_while<F, T>(f: F) -> While<F, T>
where T: RangeStream,
      F: Fn(&T::Token) -> bool,
{
    While {
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

// ------------------------------------------- Nop -------------------------------------------
pub struct Nop<I, E>(PhantomData<I>, PhantomData<E>);

impl<I, E> Parse for Nop<I, E>
    where I: TokenStream,
{
    type Input = I;
    type Output = SupressedRes;
    type Error = E;

    fn parse(&self, _: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        Ok(SupressedRes)
    }
}

pub fn nop<I, E>() -> Nop<I, E> {
    Nop(PhantomData, PhantomData)
}

// ------------------------------------------- Eof -------------------------------------------
pub struct Eof<I>(PhantomData<I>);
impl<I> Parse for Eof<I>
    where I: TokenStream,
{
    type Input = I;
    type Output = SupressedRes;
    type Error = ParseErr<I::Token>;

    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        match tokens.peek() {
            Some(t) => Err(ParseErr::unexpected(Some(t))),
            None => Ok(SupressedRes),
        }
    }
}
pub fn eof<I>() -> Eof<I> {
    Eof(PhantomData)
}

