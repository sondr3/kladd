use std::str::Chars;

#[derive(Debug)]
pub struct Cursor<'a> {
    pub len_remaining: usize,
    pub chars: Chars<'a>,
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

    pub fn next(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn nth(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    pub fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn advance(&mut self) -> Option<char> {
        let c = self.chars.next();
        self.prev = c;
        c
    }

    pub fn advance_nth(&mut self, n: usize) {
        self.chars = self.as_str()[n..].chars()
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(Option<char>) -> bool) {
        while predicate(self.next()) && !self.is_eof() {
            self.advance();
        }
    }
}
