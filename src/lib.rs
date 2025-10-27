use std::{
    iter::{FusedIterator, Peekable},
    str::Chars,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    MetadataMarker,
    Bang,
    At,
    Quote,
    OpenCurly,
    CloseCurly,
    OpenBrace,
    CloseBrace,
    Equals,
    Newline,
    Text,
    EOL,
}

#[derive(Debug)]
pub struct Pos {
    start: usize,
    end: usize,
}

#[derive(Debug)]
pub struct Segment {
    pub token: Token,
    pub pos: Pos,
}

pub struct Lexer {
    input: Vec<char>,
    size: usize,
    pos: usize,
}

pub struct LexerIter<'a>(&'a mut Lexer);

impl<'a> FusedIterator for LexerIter<'a> {}

impl<'a> Iterator for LexerIter<'a> {
    type Item = Segment;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_token()
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            size: input.len(),
            pos: 0,
            input: input.chars().collect(),
        }
    }

    pub fn iter(&mut self) -> LexerIter {
        LexerIter(self)
    }

    fn at_end(&self) -> bool {
        self.pos >= self.size
    }

    fn read(&mut self) -> Option<char> {
        if self.at_end() {
            None
        } else {
            self.pos += 1;
            Some(self.input[self.pos - 1])
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.get(self.pos)
    }

    fn peek_nth(&mut self, n: usize) -> Option<&char> {
        self.input.get(self.pos + n)
    }

    fn next_if(&mut self, eq: char) -> Option<char> {
        match self.peek() {
            Some(c) if c == &eq => self.read(),
            _ => None,
        }
    }

    fn read_segment(&mut self) -> Segment {
        let tag = match self.peek() {
            Some('+') => Token::MetadataMarker,
            Some('!') => Token::Bang,
            Some('@') => Token::At,
            Some('"') => Token::Quote,
            Some('{') => Token::OpenCurly,
            Some('}') => Token::CloseCurly,
            Some('[') => Token::OpenBrace,
            Some(']') => Token::CloseBrace,
            Some('=') => Token::Equals,
            Some('\n') => Token::Newline,
            Some(_) => Token::Text,
            None => Token::EOL,
        };

        todo!()
    }

    fn next_token(&mut self) -> Option<Segment> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &'static str = r#"
+++
metadata: things
+++

!h1{some=value}[With] a body
    "#;

    #[test]
    fn it_works() {
        let result = Lexer::new(TEST_INPUT).iter().collect::<Vec<_>>();
        assert!(!result.is_empty());
    }
}
