
#[derive(PartialEq, Debug, Clone)]
pub enum ParseErr<E> {
    Undefined,
    Expected(E)
}
// #[derive(PartialEq, Debug)]
// pub struct Found<T>(pub T);

// #[derive(PartialEq, Debug)]
// pub struct ExpectedButFound<E, F>(pub Option<E>, pub Option<F>);
