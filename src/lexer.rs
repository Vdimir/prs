// * 
// * Tra &str into array of tokens
// *

use std::str::CharIndices;

use tokens::{TokenStream, RangeTokenStream};
use pars::Verify;

type BytePos = usize;
type CharPos = usize;

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
        self.lookahead_offset += self.peek_char()
                                        .map(|c| c.len_utf8())
                                        .unwrap_or(0);
        self.peek_char()
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
            return (None, self);
        }
        return (Some(&self[..offset]), &self[offset..]);
    }
}

