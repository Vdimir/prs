// =============================================================================
// ================================ ParseResult ================================
// =============================================================================

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Expected(ParserType),
}

#[derive(Debug, PartialEq)]
pub enum ParserType {
    Unknown,
    Named(String),
    Or(Box<ParserType>, Box<ParserType>),
}

pub struct ParseResult<R, S> {
    pub res: Result<R, ParseError>,
    pub other: S,
}

impl<R, S> ParseResult<R, S> {
    pub fn succ(res: R, other: S) -> Self {
        ParseResult {
            res: Ok(res),
            other: other,
        }
    }

    pub fn fail(error: ParseError, other: S) -> Self {
        ParseResult {
            res: Err(error),
            other: other,
        }
    }

    pub fn map<U, F>(self, op: F) -> ParseResult<U, S>
        where F: FnOnce(R) -> U {
        ParseResult {
            res: self.res.map(op),
            other: self.other,
        }
    }

    pub fn into_tuple(self) -> (Result<R, ParseError>, S) {
        (self.res, self.other)
    }

    pub fn is_ok(&self) -> bool {
        self.res.is_ok()
    }
}

#[macro_export]
macro_rules! finish_ok {
    ($res:expr, $other:expr) =>
        (return $crate::result::ParseResult::succ($res, $other);)
}

#[macro_export]
macro_rules! finish_err {
    ($res:expr, $other:expr) =>
        (return $crate::result::ParseResult::fail($res, $other);)
}