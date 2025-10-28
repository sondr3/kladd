use std::str::Chars;

#[derive(Debug)]
pub struct Cursor<'a> {
    pub len_remaining: usize,
    pub chars: Chars<'a>,
    #[cfg(debug_assertions)]
    pub prev: Option<char>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Cursor {
            len_remaining: input.len(),
            chars: input.chars(),
            prev: None,
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn peek_nth(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn token_pos(&self) -> usize {
        self.len_remaining - self.as_str().len()
    }

    pub fn reset_token_pos(&mut self) {
        self.len_remaining = self.as_str().len();
    }

    pub fn advance(&mut self) -> Option<char> {
        let c = self.chars.next();

        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }

        c
    }

    pub fn advance_nth(&mut self, n: usize) {
        self.chars = self.as_str()[n..].chars();
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(Option<char>) -> bool) {
        while predicate(self.peek()) && !self.is_eof() {
            self.advance();
        }
    }
}
