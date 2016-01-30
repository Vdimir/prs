// *
// *
// *

use std::marker::PhantomData;
// ================================ TokenStream ================================
pub trait Verify<T> {
    fn satisfies(&self, arg: &T) -> bool;
}

pub struct IsEqual<'a, T: 'a>(&'a T);

#[derive(Clone)]
pub struct IsEqualOwn<T>(T);

impl<'a, T: PartialEq> Verify<T> for IsEqual<'a, T> {
    fn satisfies(&self, arg: &T) -> bool {
        arg == self.0
    }
}

impl<'a, T: PartialEq> Verify<T> for IsEqualOwn<T> {
    fn satisfies(&self, arg: &T) -> bool {
        *arg == self.0
    }
}

pub struct Predicate<T, F>(F, PhantomData<T>);

impl<T, F> Verify<T> for Predicate<T, F> where F: Fn(&T) -> bool
{
    fn satisfies(&self, arg: &T) -> bool {
        (self.0)(arg)
    }
}

pub trait TokenStream: Sized {
    type TokenType;

    fn get(&self) -> Option<(Self::TokenType, Self)>;
    fn get_if<'a, C>(&self, condition: &C) -> Option<(Self::TokenType, Self)>
        where C: Verify<Self::TokenType>
    {
        self.get().and_then(|s| {
            if condition.satisfies(&s.0) {
                Some(s)
            } else {
                None
            }
        })
    }
}

pub trait IterableTokenStream<It>: TokenStream
    where It: Iterator<Item=Self::TokenType> {
    fn iter(&self) -> It;
}

pub trait RangeTokenStream: TokenStream
{
    fn get_while<C>(&self, condition: &C) -> Option<(Self, Self)> where C: Verify<Self::TokenType>;
}


impl<'a> TokenStream for &'a str {
    type TokenType = char;

    fn get(&self) -> Option<(Self::TokenType, Self)> {
        match self.chars().next() {
            Some(c) => Some((c, &self[c.len_utf8()..])),
            None => None,
        }
    }
}

impl<'a> RangeTokenStream for &'a str {
    fn get_while<C>(&self, condition: &C) -> Option<(Self, Self)>
        where C: Verify<Self::TokenType>
    {
        let offset = self.chars()
                         .take_while(|c| condition.satisfies(c))
                         .fold(0, |len, c: char| len + c.len_utf8());
        if offset == 0 {
            return None;
        }
        return Some((&self[..offset], &self[offset..]));
    }
}


// ================================ ParseResult ================================
pub enum ParseError<T> {
    Expected(T),
}

pub struct ParseResult<R, S, E = ()> {
    pub res: Result<R, E>,
    pub other: S,
}

impl<R, S, E> ParseResult<R, S, E> {
    pub fn succ(res: R, other: S) -> Self {
        ParseResult {
            res: Ok(res),
            other: other,
        }
    }
    pub fn fail(error: E, other: S) -> Self {
        ParseResult {
            res: Err(error),
            other: other,
        }
    }

    pub fn map<U, F>(self, op: F) -> ParseResult<U, S, E>
        where F: FnOnce(R) -> U
    {
        ParseResult {
            res: self.res.map(op),
            other: self.other,
        }
    }

    pub fn is_ok(&self) -> bool {
        self.res.is_ok()
    }
}
// ================================ trait Parse ================================

// T is type of input
pub trait Parse<T>: Sized {
    type ParsedDataType;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T>;
}


// pub struct Token<T: TokenStream> {
//     token: T::TokenType,
// }

// impl<T> Parse<T> for Token<T>
//     where T: TokenStream,
//           T::TokenType: PartialEq
// {
//     type ParsedDataType = T::TokenType;
//     fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
//         let res_opt = tokens.get_if(&IsEqual(&self.token));
//         if res_opt.is_some() {
//             let (res, other) = res_opt.unwrap();
//             return ParseResult::succ(res, other);
//         }
//         return ParseResult::fail((), tokens);
//     }
// }

// pub struct Condition<F> {
//     condition: F,
// }

// impl<T, F> Parse<T> for Condition<F>
//     where T: RangeTokenStream,
//           F: Fn(&T::TokenType) -> bool
// {
//     type ParsedDataType = T;
//     fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
//         let res = tokens.get_while(&Predicate(&self.condition, PhantomData));
//         if let Some((parsed, other)) = res {
//             return ParseResult::succ(parsed, other);
//         }
//         return ParseResult::fail((), tokens);
//     }
// }

// pub fn pred<F>(f: F) -> Condition<F> {
//     Condition { condition: f }
// }
// pub fn token<T: TokenStream>(t: T::TokenType) -> Token<T> {
//     Token { token: t }
// }

//where T: TokenStream ,C: Verify<T::TokenType>
#[derive(Clone)]
pub struct Parser<C, T>
{
    checker: C,
    _p: PhantomData<T>
}

impl<C, T: TokenStream> Parser<C, T> {
    pub fn greedy(self) -> GrParser<C, T> {
        GrParser { checker: self.checker, _p: PhantomData }
    }
}

impl<T, C> Parse<T> for Parser<C, T>
    where T: TokenStream,
          C: Verify<T::TokenType>
{
    type ParsedDataType = T::TokenType;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let res_opt = tokens.get_if(&self.checker);
        if res_opt.is_some() {
            let (res, other) = res_opt.unwrap();
            return ParseResult::succ(res, other);
        }
        return ParseResult::fail((), tokens);
    }
}

#[derive(Clone)]
pub struct GrParser<C, T: TokenStream>{
    checker: C,
    _p: PhantomData<T>
}

impl<T, C> Parse<T> for GrParser<C, T>
    where T: RangeTokenStream,
          C: Verify<T::TokenType>
{
    type ParsedDataType = T;
    fn parse(&self, tokens: T) -> ParseResult<Self::ParsedDataType, T> {
        let res = tokens.get_while(&self.checker);
        if let Some((parsed, other)) = res {
            return ParseResult::succ(parsed, other);
        }
        return ParseResult::fail((), tokens);
    }
}

pub struct FnParser<F>(F);
// where F:Fn(T) -> ParseResult<R, T>;

impl<T, R, F> Parse<T> for FnParser<F> where T:TokenStream, F:Fn(T) -> ParseResult<R, T>
{
    type ParsedDataType = R;

    fn parse(&self, tokens: T) -> ParseResult<R, T> {
        self.0(tokens)
    }
}

pub fn pred<T: TokenStream, F>(f: F) -> Parser<Predicate<T::TokenType, F>, T> {
    Parser { checker: Predicate(f, PhantomData), _p: PhantomData }
}

pub fn token<T: TokenStream>(t: T::TokenType) -> Parser<IsEqualOwn<T::TokenType>, T> {
    Parser { checker: IsEqualOwn(t), _p: PhantomData }
}

pub fn fn_parser<T,R,F> (f:F) -> FnParser<F> where T:TokenStream, F:Fn(T) -> ParseResult<R, T>
{
    FnParser(f)
}



// ================================ impl Parse for ref ================================
// TODO CHECK THIS
impl<'a, I, O, P> Parse<I> for &'a P
    where I: TokenStream,
          P: Parse<I, ParsedDataType = O>
{
    type ParsedDataType = O;

    fn parse(&self, tokens: I) -> ParseResult<Self::ParsedDataType, I> {
        (*self).parse(tokens)
    }
}

