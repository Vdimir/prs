// *
// *
// *

// ============================================ Traits ============================================

pub trait TokenStream {
    type Token;
    fn peek(&mut self) -> Option<Self::Token>;
    fn next(&mut self) -> Option<Self::Token>;
}

// /// Accumulates passed tokens from `TokenStream` and return ranges
// pub trait TStreamAccumulator: TokenStream {
//     type Range;
//     fn take_range(&mut self) -> Self::Range;
//     fn backtrack_range(&mut self);
// }


pub trait SaveStream: TokenStream {
    type State;
    fn save(&self) -> Self::State;
    fn restore(&mut self, Self::State);
}


// ======================================= Implementations ========================================

pub mod char_stream {
    use super::{ TokenStream, SaveStream };

    type BytePos = usize;

    pub struct CharStream<'a> {
        source: &'a str,
        position: BytePos,
    }

    pub struct CharStreamState(BytePos);

    impl<'a> SaveStream for CharStream<'a> {
        type State = CharStreamState;
        fn save(&self) -> CharStreamState {
            CharStreamState(self.position)
        }

        fn restore(&mut self, save: CharStreamState) {
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
    }

    impl<'a> CharStream<'a> {
        pub fn new(s: &'a str) -> Self {
            CharStream {
                source: s,
                position: 0,
            }
        }
    }
}

pub mod lexem_char_stream {
    
    use super::TokenStream;
    type BytePos = usize;
    type CharPos = usize;

    pub struct CharStream<'a> {
        source: &'a str,

        lexem_begin_position: BytePos,
        lookahead_offset: BytePos
    }

    impl<'a> TokenStream for CharStream<'a> {
        type Token = char;
        fn peek(&mut self) -> Option<char> {
            self.source[self.current_offset()..].chars().next()
        }

        fn next(&mut self) -> Option<char> {
            let current_char = self.peek();
            self.lookahead_offset += current_char.map_or(0, |c| c.len_utf8());
            current_char
        }
    }

    impl<'a> CharStream<'a> {
        pub fn new(s: &'a str) -> Self {
            CharStream {
                source: s,
                lexem_begin_position: 0,
                lookahead_offset: 0
            }
        }

        pub fn drop_lookahead(&mut self) {
            self.lookahead_offset = 0;
        }

        pub fn take_lexem(&mut self) -> &'a str {
            let beg = self.lexem_begin_position;
            let end = beg + self.lookahead_offset;

            self.accept_lookahead();
            &self.source[beg..end]
        }

        pub fn passed_chars_count(&self) -> usize {
            self.source[..self.current_offset()].chars().count()
        }

        pub fn rest(&self) -> &str {
            &self.source[self.current_offset()..]
        }

        pub fn eof(&mut self) -> bool {
            self.peek().is_none()
        }

        fn accept_lookahead(&mut self) {
            self.lexem_begin_position += self.lookahead_offset;
            self.drop_lookahead();
        }

        fn current_offset(&self) -> BytePos {
            self.lexem_begin_position + self.lookahead_offset
        }
    }
}

#[cfg(test)]
mod tests {

    use super::lexem_char_stream::CharStream;
    use super::TokenStream;
    #[test]
    fn char_stream_test() {
        let mut stream = CharStream::new("eng_фцч_123");
        assert_eq!(stream.passed_chars_count(), 0);
        assert_eq!(stream.peek(), Some('e'));
        assert_eq!(stream.passed_chars_count(), 0);
        assert_eq!(stream.next(), Some('e'));
        assert_eq!(stream.passed_chars_count(), 1);
        assert_eq!(stream.peek(), Some('n'));
        assert_eq!(stream.peek(), Some('n'));
        assert_eq!(stream.next(), Some('n'));
        assert_eq!(stream.next(), Some('g'));
        assert_eq!(stream.take_lexem(), ("eng"));
        assert_eq!(stream.passed_chars_count(), 3);
        assert_eq!(stream.peek(), Some('_'));
        assert_eq!(stream.take_lexem(), (""));
        assert_eq!(stream.peek(), Some('_'));
        assert_eq!(stream.next(), Some('_'));
        assert_eq!(stream.take_lexem(), ("_"));
        assert_eq!(stream.peek(), Some('ф'));
        assert_eq!(stream.next(), Some('ф'));
        assert_eq!(stream.next(), Some('ц'));
        assert_eq!(stream.next(), Some('ч'));
        assert_eq!(stream.passed_chars_count(), 7);
        stream.drop_lookahead();
        assert_eq!(stream.passed_chars_count(), 4);
        assert_eq!(stream.peek(), Some('ф'));
        assert_eq!(stream.next(), Some('ф'));
        assert_eq!(stream.next(), Some('ц'));
        assert_eq!(stream.next(), Some('ч'));
        assert_eq!(stream.take_lexem(), ("фцч"));
        assert_eq!(stream.next(), Some('_'));
        assert_eq!(stream.take_lexem(), ("_"));
        assert_eq!(stream.passed_chars_count(), 8);
        assert_eq!(stream.peek(), Some('1'));
        assert_eq!(stream.next(), Some('1'));
        assert_eq!(stream.next(), Some('2'));
        assert_eq!(stream.next(), Some('3'));
        assert_eq!(stream.take_lexem(), ("123"));
        assert_eq!(stream.eof(), true);
        assert_eq!(stream.peek(), None);
        assert_eq!(stream.next(), None);
        assert_eq!(stream.next(), None);
        assert_eq!(stream.passed_chars_count(), 11);
    }
}
