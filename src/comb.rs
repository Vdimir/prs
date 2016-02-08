// // *
// // * Module to combinate two or more parsers in different way 
// // *

use pars::Parse;
// use pars::PParse;
// use result::*;
use stream::TokenStream;

// // ================================ MapedParser ================================
// #[derive(Clone)]
// pub struct MapedParser<F, P> {
//     f: F,
//     parser: P,
// }

// impl<F, B, P, T> Parse<T> for MapedParser<F, P>
//     where P: Parse<T>,
//           F: Fn(P::ParsedDataType) -> B,
//           T: TStream
// {
//     type ParsedDataType = B;

//     fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
//         return self.parser.parse(tokens).map(&self.f);
//     }
// }

// // ================================ OR ================================
// use std::marker::PhantomData;


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


// // // ================================ Skip ================================
// pub struct Skip<P1, P2> {
//     actual: P1,
//     skiped: P2,
// }

// // impl<T, P1, P2> Parse<T> for Skip<P1, P2>
// //     where P1: Parse<T>,
// //           P2: Parse<T>,
// //           T: TStream + Clone
// // {
// //     type ParsedDataType = P1::ParsedDataType;

// //     fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
// //         let res = (&self.actual).and(&self.skiped).parse(tokens);
// //         return res.map(|(a, _)| a);
// //     }
// // }


// // ================================ Repetition ================================
// pub struct Rep<P> {
//     parser: P,
// }

// impl<T: TStream, P> Parse<T> for Rep<P> where P: Parse<T>
// {
//     type ParsedDataType =  Vec<P::ParsedDataType>;

//     fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {

//         let mut results = Vec::new();
//         let mut other = tokens;
//         loop {
//             let parse_res = self.parser.parse(other);
//             other = parse_res.other;
//             let res = parse_res.res;
//             if !res.is_ok() {
//                 break;
//             }
//             results.push(res.unwrap());
//         }
//         if !results.is_empty() {
//             return ParseResult::succ(results, other);
//         }
//         return ParseResult::fail(ParseError::Unknown,other);
//     }
// }


// // ================================ Iter ================================
// pub struct Iter< P, I> {
//     parser: P,
//     pub input: I
// }

// impl<P, I> Iterator for Iter<P, I>
// where P:Parse<I> , I : Clone{

//     type Item = P::ParsedDataType;
//     fn next(&mut self) -> Option<Self::Item> {
//         // clone !!!
//         let res = self.parser.parse(self.input.clone());
//         if !res.is_ok() {
//             return None;
//         }
//         self.input = res.other;
//         return Some(res.res.unwrap());
//     }
// }

// pub fn iterate_parser_over_input<P,I>(p: P, inp: I) -> Iter<P, I>
// where P:Parse<I> {
//     Iter { parser: p, input: inp}
// }


// // // ================================ IterRep ================================
// // use std::iter::FromIterator;
// // use std::marker::PhantomData;

// // pub struct IterRep<P, R> {
// //     parser: P,
// //     _res_collection: PhantomData<R>
// // }

// // impl<T: TStream + Clone, P, R> Parse<T> for IterRep<P, R> where P: Parse<T>,
// // R: FromIterator<P::ParsedDataType>
// // {
// //     type ParsedDataType =  R;

// //     fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
// //         let first_res = self.parser.parse(tokens.clone());
// //         if !first_res.is_ok() {
// //             return ParseResult::fail(ParseError::Unknown, tokens);
// //         }
// //         let first = first_res.res.unwrap();

// //         let mut it = iterate_parser_over_input(&self.parser, first_res.other);


// //         let results: R = Some(first)
// //                         .into_iter()
// //                         .chain(it.by_ref()).collect();
// //         let other = it.input;

// //         return ParseResult::succ(results, other);

// //     }
// // }


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


// // ================================ ParserComb ================================
pub trait ParserComb: Parse
where Self: Sized  {
    fn or<P>(self, parser: P) -> Or<Self, P>
    {
        Or(self, parser,)
    }
}

impl<P> ParserComb for P
    where P: Parse
{}


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

//     // fn map<F, B>(self, f: F) -> MapedParser<F, Self>
//     //     where Self: Sized,
//     //           F: Fn(Self::ParsedDataType) -> B
//     // {
//     //     MapedParser {
//     //         f: f,
//     //         parser: self,
//     //     }
//     // }
// }


// pub fn or<T, P>(parser1: P, parser: P) -> OrPh<P, P, T>
//     where P: Parse<T>
// {
//     OrPh {
//         first: parser1,
//         second: parser,
//         _phan: PhantomData
//     }
// }

// pub fn maybe<T, P>(parser: P) -> Mabye<P>
//     where P: Parse<T>,
//           T: TStream
// {
//     Mabye { parser: parser }
// }

// // pub fn rep<T, P, R>(parser: P) -> IterRep<P, R>
// //     where P: Parse<T>,
// //           T: TStream
// // {
// //     IterRep { parser: parser, _res_collection: PhantomData }
// // }

// // pub fn rep<T, P>(parser: P) -> Rep<P>
// //     where P: Parse<T>,
// //           T: TStream
// // {
// //     Rep { parser: parser}
// // }
