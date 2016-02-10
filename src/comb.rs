// // *
// // * Module to combinate two or more parsers in different way 
// // *

use pars::Parse;
use stream::TokenStream;


// --------------------------------------------- Nop ---------------------------------------------
pub struct Nop<I, E>(PhantomData<I>,PhantomData<E>);
// where  P: Parse, F: Fn(P::Output) -> R

impl<I, E> Parse for Nop<I, E>
    where I: TokenStream,
{
    type Input = I;
    type Output = ();
    type Error = E;

    fn parse(&self, _: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        Ok(())
    }
}

pub fn nop<I, E>() -> Nop<I, E> {
    Nop(PhantomData,PhantomData)
}

// --------------------------------------------- Eof ---------------------------------------------
pub struct Eof<I>(PhantomData<I>);
// where  P: Parse, F: Fn(P::Output) -> R

impl<I> Parse for Eof<I>
    where I: TokenStream,
{
    type Input = I;
    type Output = ();
    type Error = ();

    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        match tokens.peek() {
            Some(_) => Err(()),
            None => Ok(()),
        }
    }
}

pub fn eof<I>() -> Eof<I> {
    Eof(PhantomData)
}



// --------------------------------------------- Then ---------------------------------------------
pub struct Then<P, F>(P, F);
// where  P: Parse, F: Fn(P::Output) -> R

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
          P2: Parse<Input=T, Output = R>,
          T: TokenStream
{
    type Input = T;
    type Output = R;
    type Error = (P1::Error, P2::Error);

    fn parse(&self, tokens: &mut T) -> Result<Self::Output, Self::Error> {

        match self.0.parse(tokens) {
            Ok(v) => Ok(v),
            Err(e0) => {
                match self.1.parse(tokens) {
                    Ok(v) => Ok(v),
                    Err(e1) => Err((e0, e1)),
                }
            },
        }
    }
}

// -------------------------------------------- Many ----------------------------------------------
use std::iter::FromIterator;
use std::marker::PhantomData;

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

        let mut it = Iter {
            parser: &self.0,
            input: tokens
        };

        let first = try!(it.parse_borrowed());

        return Ok(Some(first).into_iter()
                    .chain(it)
                    .collect());
    }
}
// -------------------------------------------- Skip ----------------------------------------------

pub struct Skip<P1, P2>(P1,P2);
impl<P1, P2, I, E> Parse for Skip<P1, P2>
where I: SaveStream,
    P1: Parse<Input=I, Error=E>,
    P2: Parse<Input=I, Error=E>,
{
    type Input = I;
    type Output = P1::Output;
    type Error = E;
    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        (&self.0, &self.1).parse(tokens).map(|(r,_)| r)
    }
}

pub struct SkipAny<P1, P2>(P1,P2);
impl<P1, P2, I, E> Parse for SkipAny<P1, P2>
where I: SaveStream,
    P1: Parse<Input=I, Error=E>,
    P2: Parse<Input=I>,
{
    type Input = I;
    type Output = P1::Output;
    type Error = P1::Error;
    fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
        let res = try!(self.0.parse(tokens));

        let it = Iter {
            parser: &self.1,
            input: tokens
        };

        // for _ in 0..0 {
            // try!(it.parse_borrowed());
        // }

        it.count();
        Ok(res)
    }
}



struct Iter<'a, P, I: 'a> {
    parser: P,
    input: &'a mut I
}

impl<'a, P, I: 'a> Iter<'a, P, I>
    where P: Parse<Input=I>,
          I: TokenStream
{
    fn parse_borrowed(&mut self) ->  Result<P::Output, P::Error> {
        self.parser.parse(self.input)
    }
}

impl<'a, P, I> Iterator for Iter<'a, P, I>
where P: Parse<Input=I>,
      I: TokenStream {

    type Item = P::Output;
    fn next(&mut self) -> Option<Self::Item> {
        self.parser.parse(self.input).ok()
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

use stream::SaveStream;

macro_rules! impl_tup {
    ($($t:ident),*) => (   
        #[allow(non_camel_case_types)] 
        impl<$($t,)* I, E> Parse for ($($t,)*) 
        where I: SaveStream,
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



// ----------------------------------------- Constructor ------------------------------------------


pub struct DynamicOr<'a, R, T, E1, E2>(
        Box<Parse<Input=T, Output = R, Error=E1> + 'a>,
        Box<Parse<Input=T, Output = R, Error=E2> + 'a>);

impl<'a, R, T, E1, E2> Parse for DynamicOr<'a, R, T, E1, E2>
    where T: TokenStream
{
    type Input = T;
    type Output = R;
    type Error = (E1, E2);

    fn parse(&self, tokens: &mut T) -> Result<Self::Output, Self::Error> {

        match self.0.parse(tokens) {
            Ok(v) => Ok(v),
            Err(e0) => {
                match self.1.parse(tokens) {
                    Ok(v) => Ok(v),
                    Err(e1) => Err((e0, e1)),
                }
            },
        }
    }
}

pub trait ParserCombDynamic<'a>: Parse
where Self: Sized + 'a  {

    fn or<P, E2>(self, parser: P) -> DynamicOr<'a, Self::Output, Self::Input, Self::Error, E2>
    where P: Parse<Input=Self::Input, Output=Self::Output, Error=E2> + 'a
    {
        DynamicOr(Box::new(self), Box::new(parser))
    }
}

impl<'a, P> ParserCombDynamic<'a> for P
    where P: Parse + 'a
{}


pub trait ParserComb: Parse
where Self: Sized  {
    

   fn static_or<P: Parse>(self, parser: P) -> Or<Self, P> {
       Or(self, parser)
   }

    fn then<F, B>(self, f: F) -> Then<Self, F>
        where F: Fn(Self::Output) -> B
    {
        Then(self, f)
    }

    fn many<R>(self) -> Many<Self, R> {
        Many(self, PhantomData)
    }

    fn skip<P>(self, parser: P) -> Skip<Self, P>
        where P: Parse<Input=Self::Input, Error=Self::Error>
    {
        Skip(self, parser)
    }

    fn skip_any<P>(self, parser: P) -> SkipAny<Self, P>
        where P: Parse<Input=Self::Input>
    {
        SkipAny(self, parser)
    }

    fn maybe(self) -> Maybe<Self> {
        Maybe(self)
    }

    fn on_err<E>(self, err: E) -> OnError<Self, E> {
        OnError(self, err)
    }

}

pub fn many<P, R>(p: P) -> Many<P, R>{
    Many(p, PhantomData)
}

pub fn maybe<P>(p: P) -> Maybe<P>{
    Maybe(p)
}



impl<P> ParserComb for P
    where P: Parse
{}
