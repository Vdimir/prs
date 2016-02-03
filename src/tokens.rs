// *
// *
// *

use pars::Verify;

// =============================================================================
// ================================ TokenStream ================================
// =============================================================================
pub trait TokenStream: Sized {
    type TokenType;

    fn look_ahead(&mut self) -> Option<Self::TokenType>;
    fn get(self) -> (Option<Self::TokenType>, Self);

    fn get_if<'a, C>(mut self, condition: &C) -> (Option<Self::TokenType>, Self)
        where C: Verify<Self::TokenType>
    {
        let next_token = self.look_ahead();

        if next_token.is_some() {
            if condition.satisfies(&next_token.unwrap()) {
                return self.get();
            }
        }
        (None, self)
    }
}

pub trait RangeTokenStream: TokenStream
{
    type RangeType;
    fn get_while<C>(self, condition: &C) -> (Option<Self::RangeType>, Self)
        where C: Verify<Self::TokenType>;
}


impl<'a> TokenStream for &'a str {
    type TokenType = char;

    fn look_ahead(&mut self) -> Option<Self::TokenType> {
        self.chars().next()
    }

    fn get(self) -> (Option<Self::TokenType>, Self) {
        match self.chars().next() {
            Some(c) => (Some(c), &self[c.len_utf8()..]),
            None => (None, self),
        }
    }
}

impl<'a> RangeTokenStream for &'a str {
    type RangeType = &'a str;
    fn get_while<C>(self, condition: &C) -> (Option<Self::RangeType>, Self)
        where C: Verify<Self::TokenType>
    {
        let offset = self.chars()
                         .take_while(|c| condition.satisfies(c))
                         .fold(0, |len, c: char| len + c.len_utf8());
        if offset == 0 {
            return (None, &self[offset..]);
        }
        return (Some(&self[..offset]), &self[offset..]);
    }
}

// struct IterableTokens<T>(T);

use std::iter::Peekable;

impl<T> TokenStream for Peekable<T>
    where T: Iterator,
          T::Item: Clone
{
    type TokenType = T::Item;

    fn look_ahead(&mut self) -> Option<Self::TokenType> {
        self.peek().map(|a| a.clone())
    }

    fn get(mut self) -> (Option<Self::TokenType>, Self) {
        (self.next(), self)
    }
}


// use std::fmt::Debug;
impl<'a, T> TokenStream for &'a [T]
    where T: Clone
{
    type TokenType = T;

    fn look_ahead(&mut self) -> Option<Self::TokenType> {
        let r = self.first().cloned();
        r
    }

    fn get(self) -> (Option<Self::TokenType>, Self) {
        if self.len() == 0 {
            (None, self)
        } else {
            (Some(self[0].clone()), &self[1..])
        }
    }
}
