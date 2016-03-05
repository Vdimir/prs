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

use std::rc::Rc;

pub type ParseTrait<'a, I, O, E> = Parse<Input=I, Output=O, Error=E> + 'a;

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

pub struct Or<P1, P2>(pub P1, pub P2);
impl<R, T, P1, P2> Parse for Or<P1, P2>
    where P1: Parse<Input=T, Output = R, Error=ParseErr<T::Token>>,
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
                    Err(e1) => Err(e0),
                }
            },
        }
    }
}

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
impl_tup!(a,b,c,d);


pub struct Seq<'a, I, R, O, E> {
    parsers: Vec<Box<ParseTrait<'a, I, O, E>>>,
    _phantom: PhantomData<R>
}

impl<'a, I, R, O, E> Seq<'a, I, R, O, E> {
    pub fn new() -> Self {
        Seq {
            parsers: Vec::new(),
            _phantom: PhantomData,
        }
    }

    pub fn and<P>(mut self, parser: P) -> Self
    where P: Parse<Input=I, Output=O, Error=E> + 'a
    {
        self.parsers.push(Box::new(parser));
        self
    }
}

impl<'a, I, R, O, E> Parse for Seq<'a, I, R, O, E>
where I: SavableStream,
      R: FromIterator<O>
{
    type Input = I;
    type Output = R;
    type Error = E;

    fn parse(&self, tokens: &mut I) -> Result<R, E> {
        let save = tokens.save();
        let res: Result<R, E> = self.parsers.iter().map(|p| p.parse(tokens)).collect();
        if res.is_err() {
            tokens.restore(save);
        }
        res
    }
}


pub struct Wrap<'a, I, O, E>(Rc<Parse<Input=I, Output=O, Error=E> + 'a>);

impl<'a, I, O, E> Clone for Wrap<'a, I, O, E>{
    fn clone(&self) -> Self {
        Wrap(self.0.clone())
    }
}

impl<'a, I, O, E> Parse for Wrap<'a, I, O, E>
{
    type Input = I;
    type Output = O;
    type Error = E;

    fn parse(&self, tokens: &mut I) -> Result<O, E> {
        self.0.parse(tokens)
    }
}

pub fn wrap<'a, P>(p: P) -> Wrap<'a, P::Input, P::Output, P::Error>
where P: Parse+'a
{
    Wrap(Rc::new(p))
}

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

pub fn many<P, R>(p: P) -> Many<P, R> {
    Many(p, PhantomData)
}

pub fn many0<P, R>(p: P) -> Many0<P, R> {
    Many0(p, PhantomData)
}

pub fn maybe<P>(p: P) -> Maybe<P> {
    Maybe(p)
}

