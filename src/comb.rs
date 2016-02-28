// // *
// // * Module to combinate two or more parsers in different way 
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
// where  P: Parse, F: Fn(P::Output) -> R

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

pub struct DynamicThen<'a, I, O, E, F>(Box<ParseTrait<'a, I, O, E>>, F);
impl<'a, I, O, E, F, R> Parse for DynamicThen<'a, I, O, E, F>
    where I: TokenStream,
        F: Fn(O) -> R
{
    type Input = I;
    type Output = R;
    type Error = E;

    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        return self.0.parse(tokens).map(&self.1);
    }
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

pub struct DynamicOr<'a, R, T, E1, E2>(
        Box<ParseTrait<'a, T, R, E1>>,
        Box<ParseTrait<'a, T, R, E2>>);

impl<'a, R, T> Parse for DynamicOr<'a, R, T, ParseErr<T::Token>, ParseErr<T::Token>>
    where T: TokenStream,
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
                // TODO e0+e1
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

pub struct Skip<'a, I, S, O, E>(Box<ParseTrait<'a, I, O, E>>, Box<ParseTrait<'a, I, S, E>>);

impl<'a, I, S, O, E> Parse for Skip<'a, I, S, O, E>
where I: SavableStream
{
    type Input = I;
    type Output = O;
    type Error = E;

    fn parse(&self, tokens: &mut Self::Input) -> Result<O, E> {
        let save = tokens.save();
        let res = try!(self.0.parse(tokens));
        match self.1.parse(tokens) {
            Ok(_) => Ok(res),
            Err(e) => { tokens.restore(save); Err(e) }
        }
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

// -------------------------------------------- And ---------------------------------------------
pub struct AndIter<'a, I: 'a, R: 'a, E: 'a, P>
where P: Iterator<Item=&'a Box<ParseTrait<'a, I, R, E>>>
{
    parsers: P,
    input: &'a mut I,
}

impl<'a, I, R, E, P> AndIter<'a, I, R, E, P>
where I: SavableStream,
      P: Iterator<Item=&'a Box<ParseTrait<'a, I, R, E>>>
{
    fn new(a: P, input: &'a mut I) -> Self {
        AndIter {
            parsers: a,
            input: input,
        }
    }
}

impl<'a, I, R, E, P> Iterator for AndIter<'a, I, R, E, P>
where I: SavableStream,
      P: Iterator<Item=&'a Box<ParseTrait<'a, I, R, E>>>
{
    type Item = Result<R, E>;
    fn next(&mut self) -> Option<Self::Item> {
        return self.parsers.next()
            .map(|p| p.parse(self.input));
    }
}

pub struct And<'a, I, R, E> {
    parsers: Vec<Box<ParseTrait<'a, I, R, E>>>,
}

impl<'a, I, R, E> And<'a, I, R, E>
where I: SavableStream
{
    pub fn and<P>(mut self, parser: P) -> Self
    where P: Parse<Input=I, Output=R, Error=E> + 'a
    {
        self.parsers.push(Box::new(parser));
        self
    }
}

// What is result type?
// - Any
// - Forse use common result type
// - etc
impl<'a, I, R, E> Parse for And<'a, I, R, E>
    where I: SavableStream
{
    type Input = I;
    type Output = Vec<R>;
    type Error = E;

    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        let save = tokens.save();
        let res: Result<Vec<R>, E> = AndIter::new(self.parsers.iter(), tokens).collect();
        if res.is_err() {
            tokens.restore(save);
        }
        res
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

pub struct Pair<'a, I, E, O1, O2>(pub Box<ParseTrait<'a, I, O1, E>>,
                                  pub Box<ParseTrait<'a, I, O2, E>>,);

impl<'a, I, E, O1, O2> Parse for Pair<'a, I, E, O1, O2>
where I: SavableStream,
{
    type Input = I;
    type Output = (O1, O2);
    type Error = E;
    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        let save = tokens.save();
        Ok((
            match self.0.parse(tokens) {
                Ok(res) => res,
                Err(e) => { tokens.restore(save); return Err(e); }
            },
            match self.1.parse(tokens) {
                Ok(res) => res,
                Err(e) => { tokens.restore(save); return Err(e); }
            },
        ))
    }
}

// ----------------------------------------- Constructor ------------------------------------------
// TODO: refactor bad name
pub trait ParserCombDynamic<'a, I, O, E>: Parse<Input=I, Output=O, Error=E>
where Self: Sized + 'a  {

    fn and<P>(self, parser: P) -> And<'a, I, O, E>
    where P: Parse<Input=I, Output=O, Error=E> + 'a
    {
        And { parsers: vec![Box::new(self), Box::new(parser)] }
    }

    fn or<P, E2>(self, parser: P) -> DynamicOr<'a, O, I, E, E2>
    where P: Parse<Input=I, Output=O, Error=E2> + 'a
    {
        DynamicOr(Box::new(self), Box::new(parser))
    }

    fn then<F, B>(self, f: F) -> DynamicThen<'a, I, O, E, F>
    where F: Fn(Self::Output) -> B {
        DynamicThen(Box::new(self), f)
    }

    fn skip<P: 'a, R>(self, parser: P) -> Then<Pair<'a, I, E, O, R>,
                                         fn((O, P::Output)) -> O>
    where P: Parse<Input=I, Error=E, Output=R> {
        fn first<A,B>(t: (A,B)) -> A { t.0 }
        Then(Pair(Box::new(self), Box::new(parser)), first )
    }

    fn skip_any<P>(self, parser: P) -> Skip<'a, I, SupressedRes, O, E>
    where P: Parse<Input=I, Error=E> + 'a,
          Self::Input: SavableStream
    {
        Skip(Box::new(self), Box::new(Many0(parser, PhantomData)))
    }

}

impl<'a, P, I, O, E> ParserCombDynamic<'a, I, O, E> for P
    where P: Parse<Input=I, Output=O, Error=E> + 'a
{}

pub trait ParserComb: Parse
where Self: Sized, Self::Input: TokenStream  {
    fn many<R>(self) -> Many<Self, R> {
        Many(self, PhantomData)
    }

    fn maybe(self) -> Maybe<Self> {
        Maybe(self)
    }
}

impl<P> ParserComb for P
    where P: Parse + Sized,
          P::Input: TokenStream
{}


pub fn skip_first<'a, I, E, P1, P2>(a: P1, b: P2) -> Then<Pair<'a, I, E, P1::Output, P2::Output>,
                                         fn((P1::Output, P2::Output)) -> P2::Output>
where P1: Parse<Input=I, Error=E> + 'a,
      P2: Parse<Input=I, Error=E> + 'a
{
    fn second<A,B>((_,b): (A,B)) -> B { b }
    Then(Pair(Box::new(a), Box::new(b)), second)
}

pub fn pair<'a, I, E, P1, P2>(a: P1, b: P2) -> Pair<'a, I, E, P1::Output, P2::Output>
where P1: Parse<Input=I, Error=E> + 'a,
      P2: Parse<Input=I, Error=E> + 'a
{
    Pair(Box::new(a), Box::new(b))
}

pub fn many<P, R>(p: P) -> Many<P, R> {
    Many(p, PhantomData)
}

pub fn maybe<P>(p: P) -> Maybe<P> {
    Maybe(p)
}


