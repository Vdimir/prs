// // *
// // * Module to combinate two or more parsers in different way 
// // *

use pars::Parse;
use stream::TokenStream;

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


// pub struct OnError<P, E>(pub P, pub E);

// impl<E, P> Parse for OnError<P, E>
//     where P: Parse, E: Clone
// {
//     type Input = P::Input;
//     type Output = P::Output;
//     type Error = E;

//     fn parse(&self, tokens: &mut Self::Input) -> Result<Self::Output, Self::Error> {
//         return self.0.parse(tokens).or(Err(self.1.clone()));
//     }
// }


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

// ----------------------------------------- Many ------------------------------------------
use std::iter::FromIterator;
use std::marker::PhantomData;

pub struct Many<P, R>(P, PhantomData<R>);

impl<P, R> Parse for Many<P, R>
    where P: Parse, R: FromIterator<P::Output>
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
        let res = self.parser.parse(self.input);
        res.ok()
    }
}


// ----------------------------------------- Constructor ------------------------------------------
pub trait ParserComb: Parse
where Self: Sized  {
    fn or<P>(self, parser: P) -> Or<Self, P>
    {
        Or(self, parser)
    }


    fn then<F, B>(self, f: F) -> Then<Self, F>
        where F: Fn(Self::Output) -> B
    {
        Then(self, f)
    }

    fn many<R>(self) -> Many<Self, R>{
        Many(self, PhantomData)
    }
}

pub fn many<P, R>(p: P) -> Many<P, R>{
    Many(p, PhantomData)
}



impl<P> ParserComb for P
    where P: Parse
{}

// // ================================ Seq ================================
// pub struct And<P1, P2> {
//     first: P1,
//     second: P2,
// }

// impl<T, P1, P2> Parse<T> for And<P1, P2>
//     where P1: Parse<T>,
//           P2: Parse<T>,
//           T: TStream + Clone
// {
//     type ParsedDataType = (P1::ParsedDataType, P2::ParsedDataType);

//     fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {

//         // TODO without clone?
//         let ParseResult { res: first_res, other } = self.first.parse(tokens.clone());

//         if !first_res.is_ok() {
//             return ParseResult::fail(
//                 ParseError::Unknown,
//                 // ParseError::Expected(self.first.parser_type()),
//                 other);
//         }

//         let ParseResult { res: second_res, other } = self.second.parse(other);
//         if !second_res.is_ok() {
//             return ParseResult::fail(
//                 ParseError::Unknown,
//                 // ParseError::Expected(self.second.parser_type()),
//                 tokens);
//         }

//         return ParseResult::succ((first_res.unwrap(), second_res.unwrap()), other);
//     }
// }


// // ================================ Mabye ================================
// pub struct Mabye<P> {
//     parser: P,
// }

// impl<T: TStream, P> Parse<T> for Mabye<P> where P: Parse<T>
// {
//     type ParsedDataType = Option<P::ParsedDataType>;

//     fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
//         let ParseResult {res, other} = self.parser.parse(tokens);

//         return ParseResult::succ(res.ok(), other);
//     }
// }




//     fn orph<P>(self, parser: P) -> OrPh<Self, P, T>
//         // where P: Parse<T, ParsedDataType = Self::ParsedDataType>
//     {
//         OrPh {
//             first: self,
//             second: parser,
//             _phan: PhantomData
//         }
//     }

//     fn and<P>(self, parser: P) -> And<Self, P>
//         where P: Parse<T>
//     {
//         And {
//             first: self,
//             second: parser,
//         }
//     }

//     // fn skip<P>(self, parser: P) -> Skip<Self, P>
//     //     where P: Parse<T>
//     // {
//     //     Skip {
//     //         actual: self,
//     //         skiped: parser,
//     //     }
//     // }

// }
