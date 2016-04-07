
#[derive(PartialEq, Debug, Clone)]
pub enum ParseErr<T> {
    Undefined,
    Unexpected(T),
    UnexpectedAt(T, usize),
    UnexpectedEof,
    Multiple(Vec<Box<ParseErr<T>>>)
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

    pub fn add_error(self, e: ParseErr<T>) -> Self {
        match self {
            ParseErr::Multiple(mut v) => { v.push(Box::new(e)); ParseErr::Multiple(v) },
            a => ParseErr::Multiple(vec![Box::new(a), Box::new(e)])
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
            &Multiple(ref v) => write!(f, "{:?}", v.iter()
                                                    .map(|e| e.to_string())
                                                    .collect::<Vec<String>>()
                                                    .join("; ")),

        }
    }
}

use std::iter::FromIterator;
pub struct DummyResult;

impl<A> FromIterator<A> for DummyResult {
    fn from_iter<T>(it: T) -> Self
    where T: IntoIterator<Item=A>
    {
        for _ in it {}
        DummyResult
    }
}
