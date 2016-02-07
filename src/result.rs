
#[derive(PartialEq, Debug)]
pub struct Expected<T>(pub T);

#[derive(PartialEq, Debug)]
pub struct Found<T>(pub T);


#[derive(PartialEq, Debug)]
pub struct ExpectedButFound<E, F>(pub Option<E>, pub Option<F>);


// impl 


// #[macro_export]
// macro_rules! finish_ok {
//     ($res:expr, $other:expr) =>
//         (return $crate::result::ParseResult::succ($res, $other);)
// }

// #[macro_export]
// macro_rules! finish_err {
//     ($res:expr, $other:expr) =>
//         (return $crate::result::ParseResult::fail($res, $other);)
// }



// #[macro_export]
// macro_rules! finish_err_unexp {
//     ($parser:expr, $other:expr) =>
//         (return $crate::result::ParseResult::fail(ParseError::Expected($parser), $other);)
// }