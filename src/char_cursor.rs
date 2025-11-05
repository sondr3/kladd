use std::str::Chars;

#[derive(Debug)]
pub struct CharCursor<'a> {
    pub curr: usize,
    pub start: usize,
    pub chars: Chars<'a>,
    pub source: &'a str,
    #[cfg(debug_assertions)]
    pub prev: Option<char>,
}

impl<'a> CharCursor<'a> {
    pub fn new(input: &'a str) -> Self {
        CharCursor {
            curr: 0,
            start: 0,
            chars: input.chars(),
            source: input,
            #[cfg(debug_assertions)]
            prev: None,
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn prev(&self) -> Option<char> {
        #[cfg(debug_assertions)]
        {
            self.prev
        }

        #[cfg(not(debug_assertions))]
        {
            None
        }
    }

    pub fn _peek_nth(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn lexeme(&self) -> &'a str {
        &self.source[self.start..self.curr]
    }

    pub fn advance(&mut self) -> Option<char> {
        self.curr += 1;
        let c = self.chars.next();

        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }

        c
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(Option<char>) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.advance();
        }
    }
}
