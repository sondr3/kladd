use std::rc::Rc;

use crate::lexer::{Token, TokenKind};

#[derive(Debug, Default, Clone)]
pub struct TokenStream(pub Rc<Vec<Token>>);

impl TokenStream {
    pub fn new(ts: Vec<Token>) -> Self {
        TokenStream(Rc::new(ts))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get(&self, idx: usize) -> Option<&Token> {
        self.0.get(idx)
    }

    pub fn iter(&self) -> TokenStreamIter<'_> {
        TokenStreamIter::new(self)
    }
}

impl FromIterator<Token> for TokenStream {
    fn from_iter<I: IntoIterator<Item = Token>>(iter: I) -> Self {
        TokenStream::new(iter.into_iter().collect::<Vec<Token>>())
    }
}

#[derive(Clone)]
pub struct TokenStreamIter<'a> {
    stream: &'a TokenStream,
    index: usize,
}

impl<'a> TokenStreamIter<'a> {
    pub fn new(stream: &'a TokenStream) -> Self {
        TokenStreamIter { stream, index: 0 }
    }

    pub fn peek(&self) -> Option<&'a Token> {
        self.stream.get(self.index)
    }
}

impl<'a> Iterator for TokenStreamIter<'a> {
    type Item = &'a Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.stream.get(self.index).inspect(|_| {
            self.index += 1;
        })
    }
}

#[derive(Debug)]
pub struct TokenCursor {
    iter: TokenStream,
    idx: usize,
}

impl TokenCursor {
    pub fn new(input: Vec<Token>) -> Self {
        TokenCursor {
            iter: TokenStream::from_iter(input),
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
        let t = self.iter.0[self.idx].clone();
        self.idx += 1;
        t
    }

    pub fn iter(&self) -> TokenStreamIter<'_> {
        self.iter.iter()
    }

    pub fn is_at_end(&self) -> bool {
        self.iter.is_empty()
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
