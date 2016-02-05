// * 
// * Tra &str into array of tokens
// *

use std::str::CharIndices;

use tokens::{TokenStream, RangeTokenStream};
use pars::Verify;

type BytePos = usize;
type CharPos = usize;

#[derive(Clone)]
pub struct CharsStream<'a> {
    source: &'a str,

    lexem_begin_position: BytePos,
    lookahead_offset: BytePos
}

impl<'a> CharsStream<'a> {
    pub fn new(s: &'a str) -> Self {
        CharsStream {
            source: s,
            lexem_begin_position: 0,
            lookahead_offset: 0
        }
    }

    pub fn drop_lookahead(&mut self) {
        self.lookahead_offset = 0;
    }

    pub fn take_lexem(&mut self) -> &'a str {
        let beg = self.lexem_begin_position;
        let end = beg + self.lookahead_offset;

        self.accept_lookahead();
        &self.source[beg..end]
    }

    pub fn peek_char(&mut self) -> Option<char> {
        self.source[self.current_offset()..].chars().next()
    }

    pub fn next_char(&mut self) -> Option<char> {
        let res = self.peek_char();
        self.lookahead_offset += self.peek_char()
                                        .map_or(0, |c| c.len_utf8());
        res
    }

    pub fn eof(&mut self) -> bool {
        self.peek_char().is_none()
    }

    fn accept_lookahead(&mut self) {
        self.lexem_begin_position += self.lookahead_offset;
        self.drop_lookahead();
    }

    fn current_offset(&self) -> BytePos {
        self.lexem_begin_position + self.lookahead_offset
    }

    pub fn passed_chars_count(&self) -> usize {
        self.source[..self.current_offset()].chars().count()
    }

    pub fn rest(&self) -> &str {
        &self.source[self.current_offset()..]
    }
}   

impl<'a> TokenStream for CharsStream<'a> {
    type TokenType = char;

    fn look_ahead(&mut self) -> Option<Self::TokenType> {
        self.peek_char()
    }

    fn get(mut self) -> (Option<Self::TokenType>, Self) {
        (self.next_char(), self)
        // match self.chars().next() {
            // Some(c) => (Some(c), &self[c.len_utf8()..]),
            // None => (None, self),
        // }
    }
    fn get_if<C>(mut self, condition: &C) -> (Option<Self::TokenType>, Self)
        where C: Verify<Self::TokenType>
    {
        if let Some(c) = self.peek_char() {
             if condition.satisfies(&c) {
                return self.get();
             }
        }
        return (None, self);
    }
}

impl<'a> RangeTokenStream for CharsStream<'a> {
    type RangeType = &'a str;
    fn get_while<C>(mut self, condition: &C) -> (Option<Self::RangeType>, Self)
        where C: Verify<Self::TokenType>
    {
        while let Some(c) = self.peek_char()
        {
             if condition.satisfies(&c) {
                self.next_char();
             } else {
                break;
             }
        }
        let res = self.take_lexem();
        if res.is_empty() {
            return (None, self);
        }
        return (Some(res), self);
    }
}


