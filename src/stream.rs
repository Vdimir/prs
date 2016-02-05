// *
// *
// *

use verify::Verify;

// =============================================================================
// ================================ TokenStream ================================
// =============================================================================
pub trait TokenStream: Sized {
    type TokenType;

    fn look_ahead(&mut self) -> Option<Self::TokenType>;
    fn get(self) -> (Option<Self::TokenType>, Self);

    fn get_if<C>(mut self, condition: &C) -> (Option<Self::TokenType>, Self)
        where C: Verify<Self::TokenType>
    {
        let next_token = self.look_ahead();

        if next_token.is_some() && 
                condition.satisfies(&next_token.unwrap()) {
            return self.get();
        }
        (None, self)
    }
}

pub trait RangeTokenStream: TokenStream
{
    type RangeType;
    fn get_while<C>(self, condition: &C) -> (Option<Self::RangeType>, Self)
        where C: Verify<Self::TokenType>;
}

impl<'a, T> TokenStream for &'a [T]
    where T: Clone
{
    type TokenType = T;

    fn look_ahead(&mut self) -> Option<Self::TokenType> {
        self.first().cloned()
    }

    fn get(self) -> (Option<Self::TokenType>, Self) {
        if self.len() == 0 {
            (None, self)
        } else {
            (Some(self[0].clone()), &self[1..])
        }
    }
}
