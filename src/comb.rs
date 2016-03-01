// // *
// // * Module to combinate parsers in different way
// // *
use pars::Parse;
use stream::TokenStream;
use std::iter::FromIterator;
use std::marker::PhantomData;
use stream::SavableStream;
use result::ParseErr;
use result::SupressedRes;

pub type ParseTrait<'a, I, O, E> = Parse<Input=I, Output=O, Error=E> + 'a;

// --------------------------------------------- Nop ---------------------------------------------
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
    Nop(PhantomData,PhantomData)
}

// --------------------------------------------- Eof ---------------------------------------------
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

// --------------------------------------------- Then ---------------------------------------------
pub struct Then<P, F>(P, F);
impl<F, P, R> Parse for Then<P, F>
    where P: Parse,
        F: Fn(P::Output) -> R
{
    type Input = P::Input;
    type Output = R;
    type Error = P::Error;

    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        return self.0.parse(tokens).map(&self.1);
    }
}

pub struct OnError<P, E>(P, E);
impl<E, P> Parse for OnError<P, E>
    where P: Parse, E: Clone
{
    type Input = P::Input;
    type Output = P::Output;
    type Error = E;

    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        return self.0.parse(tokens).or(Err(self.1.clone()));
    }
}


// ---------------------------------------------- Or ----------------------------------------------
pub struct Or<P1, P2>(pub P1, pub P2);
impl<R, T, P1, P2> Parse for Or<P1, P2>
    where P1: Parse<Input=T, Output = R>,
          P2: Parse<Input=T, Output = R, Error=ParseErr<T::Token>>,
          T: TokenStream
{
    type Input = T;
    type Output = R;
    type Error = ParseErr<T::Token>;

    fn parse(&self, tokens: &mut T) -> Result<Self::Output, Self::Error> {
        match self.0.parse(tokens) {
            Ok(v) => Ok(v),
            Err(e0) => {
                match self.1.parse(tokens) {
                    Ok(v) => Ok(v),
                    // TODO combine e0 and e1
                    Err(e1) => Err(e1),
                }
            },
        }
    }
}

// -------------------------------------------- Many ----------------------------------------------
struct ManyIter<'a, P: 'a, I: 'a> {
    parser: &'a P,
    input: &'a mut I
}

impl<'a, P, I: 'a> ManyIter<'a, P, I>
    where P: Parse<Input=I>,
          I: TokenStream
{
    fn parse_once(&mut self) ->  Result<P::Output, P::Error> {
        self.parser.parse(self.input)
    }
}

impl<'a, P, I> Iterator for ManyIter<'a, P, I>
where P: Parse<Input=I>,
      I: TokenStream {
    type Item = P::Output;
    fn next(&mut self) -> Option<Self::Item> {
        self.parser.parse(self.input).ok()
    }
}

pub struct Many<P, R>(P, PhantomData<R>);
impl<P, R> Parse for Many<P, R>
    where P: Parse,
          P::Input: TokenStream,
          R: FromIterator<P::Output>,
{
    type Input = P::Input;
    type Output = R;
    type Error = P::Error;
    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        let mut it = ManyIter { parser: &self.0, input: tokens };
        let first = try!(it.parse_once());
        return Ok(Some(first).into_iter()
                    .chain(it)
                    .collect());
    }
}

pub struct AndMany<P1, P2, R>(P1, P2, PhantomData<R>);
impl<P1, P2, I, O, E, R> Parse for AndMany<P1, P2, R>
    where R: FromIterator<O>,
         I: SavableStream,
         P1: Parse<Input=I, Output=O, Error=E>,
         P2: Parse<Input=I, Output=O, Error=E>,
{
    type Input = I;
    type Output = R;
    type Error = E;
    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        let first = try!(self.0.parse(tokens));
        let mut it = ManyIter { parser: &self.1, input: tokens };
        return Ok(Some(first).into_iter()
                    .chain(it)
                    .collect());
    }
}

pub struct Many0<P, R>(P, PhantomData<R>);
impl<P, R> Parse for Many0<P, R>
where P::Input: SavableStream,
    P: Parse,
    R: FromIterator<P::Output>,
{
    type Input = P::Input;
    type Output = R;
    type Error = P::Error;
    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        let it = ManyIter { parser: &self.0, input: tokens };
        Ok(it.collect())
    }
}

// -------------------------------------------- Skip ----------------------------------------------

pub struct SkipAny<P1, P2>(P1,P2);
impl<P1, P2, I, E> Parse for SkipAny<P1, P2>
where I: SavableStream,
    P1: Parse<Input=I, Error=E>,
    P2: Parse<Input=I>,
{
    type Input = I;
    type Output = P1::Output;
    type Error = P1::Error;
    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        let res = try!(self.0.parse(tokens));
        let it = ManyIter {
            parser: &self.1,
            input: tokens
        };
        for _ in it {}
        Ok(res)
    }
}

// ----------------------------------------- Maybe ------------------------------------------
pub struct Maybe<P>(P);

impl<P> Parse for Maybe<P>
    where P: Parse,
{
    type Input = P::Input;
    type Output = Option<P::Output>;
    type Error = P::Error;
    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        Ok(self.0.parse(tokens).ok())
    }
}

macro_rules! impl_tup {
    ($($t:ident),*) => (
        #[allow(non_camel_case_types)]
        impl<$($t,)* I, E> Parse for ($($t,)*)
        where I: SavableStream,
            $($t: Parse<Input=I, Error=E>,)*
        {
            type Input = I;
            type Output = ($($t::Output,)*);
            type Error = E;
            fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
                let &($( ref $t, )*) = self;
                let save = tokens.save();
                Ok(($(
                    match $t.parse(tokens) {
                        Ok(res) => res,
                        Err(e) => { tokens.restore(save); return Err(e); }
                    },
                )*))
            }
        })
}

impl_tup!(a,b);
impl_tup!(a,b,c);

pub struct Wrap<'a, I, O, E>(Box<Parse<Input=I, Output=O, Error=E> + 'a>);
impl<'a, I, O, E> Parse for Wrap<'a, I, O, E>
{

    type Input = I;
    type Output = O;
    type Error = E;

    fn parse(&self, tokens: &mut I) -> Result<O, E>
    {
        self.0.parse(tokens)
    }

}

pub fn wrap<'a, P>(p: P) -> Wrap<'a, P::Input, P::Output, P::Error>
where P: Parse+'a
{
    Wrap(Box::new(p))
}

// ----------------------------------------- Constructor ------------------------------------------
pub trait ParserComb: Parse
where Self: Sized, {
    fn or<P: Parse>(self, parser: P) -> Or<Self, P> {
        Or(self, parser)
    }

    fn then<F, B>(self, f: F) -> Then<Self, F>
    where F: Fn(Self::Output) -> B {
        Then(self, f)
    }

    fn many<R>(self) -> Many<Self, R> {
        Many(self, PhantomData)
    }

    fn skip<P>(self, parser: P) -> Then<(Self, P), fn((Self::Output, P::Output)) -> Self::Output>
    where P: Parse<Input=Self::Input, Error=Self::Error> {
        fn first<A,B>((a,_): (A,B)) -> A { a }
        Then((self, parser), first)
    }

    fn skip_any<P>(self, parser: P) -> SkipAny<Self, P>
    where P: Parse<Input=Self::Input> {
        SkipAny(self, parser)
    }

    fn maybe(self) -> Maybe<Self> {
        Maybe(self)
    }

    fn on_err<E>(self, err: E) -> OnError<Self, E> {
        OnError(self, err)
    }
}

impl<P> ParserComb for P
    where P: Parse + Sized,
{}


pub fn skip_first<I, E, P1, P2>(a: P1, b: P2) ->
                                Then<(P1, P2), fn((P1::Output, P2::Output)) -> P2::Output>
where P1: Parse<Input=I, Error=E>,
      P2: Parse<Input=I, Error=E>
{
    fn second<A,B>((_,b): (A,B)) -> B { b }
    Then((a, b), second)
}

pub fn many<P, R>(p: P) -> Many<P, R> {
    Many(p, PhantomData)
}

pub fn maybe<P>(p: P) -> Maybe<P> {
    Maybe(p)
}


