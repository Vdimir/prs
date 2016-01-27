// *
// *
// *

use std::marker::PhantomData;
use std::default::Default;

// ================================ TokenStream ================================
pub trait TokenStream {
    type TokenType;
    type RangeType;
    fn get(&self) -> (Option<Self::TokenType>, Self);
}

pub trait IterableTokenStream<It>: TokenStream
    where It: Iterator<Item=Self::TokenType> {
    fn iter(&self) -> It;
}

pub trait RangeTokenStream: TokenStream + Sized
{
    fn get_while<P>(&self, p: P) -> (Option<Self>, Self);
}


// rustc --explain E0117
// impl<'a> Iterator for &'a str { ... }
impl<'a> TokenStream for &'a str {
    type TokenType = char;
    type RangeType = Self;

    // TODO test
    fn get(&self) -> (Option<Self::TokenType>, Self::RangeType) {
        match self.char_indices().next() {
            Some((i, c)) => (Some(c), &self[i..]),
            None => (None, &self),
        }
    }
}

use std::str::Chars;

impl<'a> IterableTokenStream<Chars<'a>> for &'a str {
    fn iter(&self) -> Chars<'a> {
        return self.chars();
    }
}

// ================================ trait Parser ================================
pub type ParseErrorType = ();
pub type ParseResult<D, T> = (Result<D, ParseErrorType>, T);

pub trait Parser<T: TokenStream>: Sized {
    // Default associated type not allowed yet
    // type Tokens = T;
    // type ParseResult = (Result<Self::ParsedDataType, ParseErrorType>, Self::Tokens);
    type ParsedDataType;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T>;
}

// // TODO CHECK THIS
// impl<'a, I, O, P> Parser<I> for &'a P
//     where I: TokenStream,
//           P: Parser<I, ParsedDataType = O>
// {
//     type ParsedDataType = O;
//     // type Tokens = I;

//     fn parse(&self, tokens: I) -> ParseResult<Self::ParsedDataType, I> {
//         (*self).parse(tokens)
//     }
// }

trait Checker<T> where
          T: TokenStream {
    fn is_valid(&self, arg: T::TokenType) -> bool;
}

pub struct PredicateParser<F, T>
    where F: Fn(T::TokenType) -> bool,
          T: TokenStream
{
    valid_cheker: F,
    _phantom: PhantomData<T>,
}

impl<F, T> PredicateParser<F, T>
    where F: Fn(T::TokenType) -> bool,
          T: TokenStream
{
    pub fn new(f: F) -> Self {
        PredicateParser {
            valid_cheker: f,
            _phantom: PhantomData,
        }
    }
}

impl<F, T> Checker<T> for PredicateParser<F, T>
    where F: Fn(T::TokenType) -> bool,
          T: TokenStream
{
    fn is_valid(&self, arg: T::TokenType) -> bool {
        (&self.valid_cheker)(arg)
    }
}

// struct TokenParser<T>
//     where T: TokenStream
// {
//     symb: T::TokenType,
// }

// ================================ StrParser ================================

// impl<'a, P> Parser<&'a str> for P where P: Checker<&'a str>
impl<'a, P> Parser<&'a str> for PredicateParser<P, &'a str> where P: Fn(char) -> bool
{
    type ParsedDataType = &'a str;

    fn parse(&self, tokens: &'a str) -> ParseResult<Self::ParsedDataType, &'a str> {

        let parsed_offset = tokens.iter()
                                  .take_while(|&c| self.is_valid(c))
                                  .fold(0, |len, c| len + c.len_utf8());

        if parsed_offset > 0 {
            return (Ok(&tokens[..parsed_offset]), &tokens[parsed_offset..]);
        }
        return (Err(ParseErrorType::default()), tokens);
    }
}
