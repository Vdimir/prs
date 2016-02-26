
#[derive(PartialEq, Debug, Clone)]
pub enum ParseErr<T> {
    Undefined,
    Unexpected(T),
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
}
