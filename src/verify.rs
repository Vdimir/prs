// =============================================================================
// ================================== Verify ==================================
// =============================================================================
use std::marker::PhantomData;

/// Trait that check tokens
pub trait Verify<T> {
    fn satisfies(&self, arg: &T) -> bool;
}

/// Pass tokens that equal given
#[derive(Clone)]
pub struct EqualCheker<T>(pub T);
impl<'a, T: PartialEq> Verify<T> for EqualCheker<T> {
    fn satisfies(&self, arg: &T) -> bool {
        *arg == self.0
    }
}

/// Pass tokens than satisfies given predicate(closure or function)
pub struct Predicate<T, F>(pub F, pub PhantomData<T>);
impl<T, F> Verify<T> for Predicate<T, F> where F: Fn(&T) -> bool
{
    fn satisfies(&self, arg: &T) -> bool {
        (self.0)(arg)
    }
}