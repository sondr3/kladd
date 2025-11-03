use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
pub struct TokenCursor {
    iter: Vec<Token>,
    idx: usize,
}

impl TokenCursor {
    pub fn new(input: Vec<Token>) -> Self {
        TokenCursor {
            iter: input,
            idx: 0,
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.iter.get(self.idx)
    }

    pub fn peek_kind(&self) -> Option<TokenKind> {
        self.iter.get(self.idx).map(|t| t.kind)
    }

    pub fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.iter.get(self.idx + n)
    }

    pub fn _peek_nth_kind(&self, n: usize) -> Option<TokenKind> {
        self.iter.get(self.idx + n).map(|t| t.kind)
    }

    pub fn advance(&mut self) -> Token {
        debug_assert!(self.idx <= self.iter.len());
        let t = self.iter[self.idx].clone();
        self.idx += 1;
        t
    }

    pub fn is_at_end(&self) -> bool {
        self.idx >= self.iter.len()
    }

    pub fn advance_if(&mut self, mut pred: impl FnMut(Option<&Token>) -> bool) -> Token {
        debug_assert!(pred(self.peek()));
        if pred(self.peek()) {
            self.advance()
        } else {
            panic!("invalid predicate")
        }
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(Option<&Token>) -> bool) -> Vec<Token> {
        let mut res = Vec::new();

        while predicate(self.peek()) && !self.is_at_end() {
            res.push(self.advance());
        }

        res
    }
}
