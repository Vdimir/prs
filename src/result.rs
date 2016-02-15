
#[derive(PartialEq, Debug, Clone)]
pub enum ParseErr<E> {
    Undefined,
    Expected(E),
    Complex(Box<ParseErr<E>>, Box<ParseErr<E>>)
}
