
#[derive(PartialEq, Debug, Clone)]
pub enum ParseErr<T> {
    Undefined,
    Unexpected(T),
    UnexpectedAt(T, usize),
    UnexpectedEof,
}

impl<T> ParseErr<T> {
    pub fn unexpected(token_or_eof: Option<T>) -> Self {
        if let Some(token) = token_or_eof {
            ParseErr::Unexpected(token)
        } else {
            ParseErr::UnexpectedEof
        }
    }

    pub fn at(self, pos: usize) -> Self {
        match self {
            ParseErr::Unexpected(t) => ParseErr::UnexpectedAt(t, pos),
            e => e
        }
    }
}

use std::fmt;
use self::ParseErr::*;
impl<T: fmt::Display> fmt::Display for ParseErr<T>{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Undefined => write!(f, "Unknown error"),
            &Unexpected(ref t) => write!(f, "Unexpected token: `{}`", *t),
            &UnexpectedAt(ref t, p) => write!(f, "Unexpected token: `{}` at {}", *t, p),
            &UnexpectedEof => write!(f, "Unexpected EOF"),
        }
    }
}

use std::iter::FromIterator;
pub struct SupressedRes;

impl<A> FromIterator<A> for SupressedRes {
    fn from_iter<T>(it: T) -> Self
    where T: IntoIterator<Item=A>
    {
        for _ in it {}
        SupressedRes
    }
}
