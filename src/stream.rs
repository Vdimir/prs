// *
// *
// *

// ============================================ Traits ============================================

pub trait TokenStream {
    type Token;
    fn peek(&mut self) -> Option<Self::Token>;
    fn next(&mut self) -> Option<Self::Token>;
    fn position(&self) -> usize;
}

pub trait SavableStream: TokenStream {
    type State: Clone;
    fn save(&self) -> Self::State;
    fn restore(&mut self, Self::State);
}

pub trait RangeStream: SavableStream {
    type Range;
    /// return range begin from previously saved state end with current state
    fn range(&self, Self::State) -> Option<Self::Range>;
}


// ======================================= Implementations ========================================

pub mod char_stream {
    use super::{ TokenStream, SavableStream, RangeStream };

    type BytePos = usize;

    pub struct CharStream<'a> {
        source: &'a str,
        position: BytePos,
    }
    #[derive(Clone, Copy)]
    pub struct CharStreamState(BytePos);

    impl<'a> SavableStream for CharStream<'a> {
        type State = CharStreamState;
        fn save(&self) -> CharStreamState {
            CharStreamState(self.position)
        }

        fn restore(&mut self, save: CharStreamState) {
            assert!(self.source.len() >= save.0, "This state out of range");
            self.position = save.0
        }
    }

    impl<'a> TokenStream for CharStream<'a> {
        type Token = char;
        fn peek(&mut self) -> Option<char> {
            self.source[self.position..].chars().next()
        }

        fn next(&mut self) -> Option<char> {
            let current_char = self.peek();
            self.position += current_char.map_or(0, |c| c.len_utf8());
            current_char
        }

        fn position(&self) -> usize {
            self.source[..self.position].chars().count()
        }
    }

    impl<'a> RangeStream for CharStream<'a> {
        type Range = &'a str;
        fn range(&self, state: CharStreamState) -> Option<&'a str> {
            let end: usize = self.position;
            match state.0 {
                beg if beg.lt(&end) => Some(&self.source[beg..end]),
                _ => None,
            }
        }
    }

    impl<'a> CharStream<'a> {
        pub fn new(s: &'a str) -> Self {
            CharStream {
                source: s,
                position: 0,
            }
        }
    }

    impl<'a> From<&'a str> for CharStream<'a> {
        fn from(s: &'a str) -> Self {
            CharStream::new(s)
        }
    }
}

pub mod vec_stream {
    use super::{ TokenStream, SavableStream };

    type Idx = usize;

    pub struct VecStream<T> {
        source: Vec<T>,
        position: Idx,
    }

    #[derive(Clone, Copy)]
    pub struct VecStreamState(Idx);

    impl<T: Clone> SavableStream for VecStream<T> {
        type State = VecStreamState;
        fn save(&self) -> VecStreamState {
            VecStreamState(self.position)
        }

        fn restore(&mut self, save: VecStreamState) {
            assert!(self.source.len() >= save.0, "This state out of range");
            self.position = save.0
        }
    }

    impl<T: Clone> TokenStream for VecStream<T> {
        type Token = T;
        fn peek(&mut self) -> Option<T> {
            self.source.get(self.position).cloned()
        }

        fn next(&mut self) -> Option<T> {
            let res = self.peek();
            self.position += 1;
            res
        }

        fn position(&self) -> usize {
            self.position
        }
    }

    impl<T: Clone> VecStream<T> {
        pub fn new(s: Vec<T>) -> Self {
            VecStream {
                source: s,
                position: 0,
            }
        }
    }

    impl<T: Clone> From<Vec<T>> for VecStream<T> {
        fn from(v: Vec<T>) -> Self {
            VecStream::new(v)
        }
    }
}

// ======================================= Tests ========================================
#[cfg(test)]
mod tests {
    use super::char_stream::CharStream;
    use super::vec_stream::VecStream;
    use super::{TokenStream, RangeStream, SavableStream};

    #[test]
    fn char_stream_test() {
        let mut stream = CharStream::new("eng_фцч_123");

        let saved = stream.save();
        assert_eq!(stream.peek(), Some('e'));
        assert_eq!(stream.next(), Some('e'));
        assert_eq!(stream.peek(), Some('n'));
        assert_eq!(stream.peek(), Some('n'));
        assert_eq!(stream.next(), Some('n'));
        assert_eq!(stream.next(), Some('g'));
        assert_eq!(stream.position(), 3);
        assert_eq!(stream.range(saved), Some("eng"));

        let saved = stream.save();
        assert_eq!(stream.peek(), Some('_'));
        assert_eq!(stream.next(), Some('_'));
        assert_eq!(stream.range(saved), Some("_"));

        let saved = stream.save();
        assert_eq!(stream.peek(), Some('ф'));
        assert_eq!(stream.next(), Some('ф'));
        assert_eq!(stream.next(), Some('ц'));
        assert_eq!(stream.next(), Some('ч'));
        stream.restore(saved);

        let saved = stream.save();
        assert_eq!(stream.range(saved), None);

        let saved = stream.save();
        assert_eq!(stream.peek(), Some('ф'));
        assert_eq!(stream.next(), Some('ф'));
        assert_eq!(stream.next(), Some('ц'));
        assert_eq!(stream.next(), Some('ч'));
        assert_eq!(stream.peek(), Some('_'));
        assert_eq!(stream.range(saved), Some("фцч"));

        assert_eq!(stream.next(), Some('_'));

        let saved = stream.save();
        assert_eq!(stream.peek(), Some('1'));
        assert_eq!(stream.next(), Some('1'));
        assert_eq!(stream.next(), Some('2'));
        assert_eq!(stream.next(), Some('3'));
        assert_eq!(stream.range(saved), Some("123"));
        assert_eq!(stream.position(), 11);
        assert_eq!(stream.peek(), None);
        assert_eq!(stream.next(), None);
        assert_eq!(stream.next(), None);
    }

    #[test]
    fn vec_stream_test() {
        let mut stream = VecStream::new(vec!['x','y','z']);
        assert_eq!(stream.peek(), Some('x'));
        assert_eq!(stream.next(), Some('x'));
        let save = stream.save();
        assert_eq!(stream.next(), Some('y'));
        assert_eq!(stream.peek(), Some('z'));
        assert_eq!(stream.next(), Some('z'));

        assert_eq!(stream.peek(), None);
        assert_eq!(stream.next(), None);

        stream.restore(save);
        assert_eq!(stream.next(), Some('y'));
        assert_eq!(stream.next(), Some('z'));
        assert_eq!(stream.next(), None);

    }
}

