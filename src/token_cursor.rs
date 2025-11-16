use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct TokenCursor<'a> {
    iter: Vec<Token<'a>>,
    idx: usize,
}

impl<'a> TokenCursor<'a> {
    pub fn new(input: Vec<Token<'a>>) -> Self {
        TokenCursor {
            iter: input,
            idx: 0,
        }
    }

    pub fn peek(&self) -> Option<&Token<'a>> {
        self.iter.get(self.idx)
    }

    pub fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|t| t.kind)
    }

    pub fn peek_nth(&self, n: usize) -> Option<&Token<'a>> {
        self.iter.get(self.idx + n)
    }

    pub fn peek_nth_kind(&self, n: usize) -> Option<TokenKind> {
        self.peek_nth(self.idx + n).map(|t| t.kind)
    }

    pub fn advance(&mut self) -> Token<'a> {
        debug_assert!(self.idx <= self.iter.len());
        let t = self.iter[self.idx];
        self.idx += 1;
        t
    }

    pub fn is_at_end(&self) -> bool {
        self.idx >= self.iter.len()
    }

    pub fn advance_while<F>(&mut self, parse_fn: F) -> Vec<Token<'a>>
    where
        F: Fn(&mut Self) -> bool,
    {
        let mut res = Vec::new();

        while parse_fn(self) {
            res.push(self.advance());
        }

        res
    }

    pub fn _advance_if(&mut self, mut pred: impl FnMut(Option<&Token>) -> bool) -> Token<'a> {
        debug_assert!(pred(self.peek()));
        if pred(self.peek()) {
            self.advance()
        } else {
            panic!("invalid predicate")
        }
    }

    pub fn eat_while(&mut self, mut predicate: impl FnMut(&Token) -> bool) -> Vec<Token<'a>> {
        let mut res = Vec::new();

        while predicate(match self.peek() {
            Some(t) => t,
            None => return res,
        }) {
            res.push(self.advance());
        }

        res
    }
}
