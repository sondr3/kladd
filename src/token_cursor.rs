use crate::lexer::{Token, TokenKind};

#[derive(Debug)]
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
        self.iter.get(self.idx).map(|t| t.kind)
    }

    pub fn peek_nth(&self, n: usize) -> Option<&Token<'a>> {
        self.iter.get(self.idx + n)
    }

    pub fn advance(&mut self) -> Token<'a> {
        debug_assert!(self.idx <= self.iter.len());
        let t = self.iter[self.idx];
        self.idx += 1;
        t
    }

    pub fn advance_if(&mut self, mut pred: impl FnMut(Option<&Token<'a>>) -> bool) -> Token<'a> {
        debug_assert!(pred(self.peek()));
        if pred(self.peek()) {
            self.advance()
        } else {
            panic!("invalid predicate")
        }
    }

    pub fn eat_while(
        &mut self,
        mut predicate: impl FnMut(Option<&Token<'a>>) -> bool,
    ) -> Vec<Token<'a>> {
        let mut res = Vec::new();

        while predicate(self.peek()) {
            res.push(self.advance());
        }

        res
    }
}
