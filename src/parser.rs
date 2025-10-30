use crate::{
    lexer::{Token, TokenKind, Whitespace},
    token_cursor::TokenCursor,
};

#[derive(Debug)]
pub enum Block {
    Metadata(Vec<Token>),
    Block {
        name: Token,
        attributes: Vec<Token>,
        body: Vec<Token>,
    },
    Whitespace(Whitespace),
    Newline,
    Unknown,
    EOF,
}

pub fn parse(input: Vec<Token>) -> Vec<Block> {
    let mut cursor = TokenCursor::new(input);
    let mut res = Vec::new();

    if cursor
        .peek()
        .is_some_and(|t| t.kind == TokenKind::MetadataMarker)
    {
        res.push(cursor.parse_metadata());
    }

    res.extend(std::iter::from_fn(move || match cursor.advance_token() {
        Block::Unknown | Block::EOF => None,
        tok => Some(tok),
    }));

    res
}

impl TokenCursor {
    pub fn advance_token(&mut self) -> Block {
        let first_token = self.advance();

        let block = match first_token.kind {
            TokenKind::MetadataMarker => self.parse_metadata(),
            TokenKind::EOF => Block::EOF,
            TokenKind::Unknown => Block::Unknown,
            TokenKind::Whitespace(c) => Block::Whitespace(c),
            TokenKind::Newline => match self.peek_kind() {
                Some(TokenKind::Bang) => self.parse_block(),
                _ => Block::Newline,
            },
            _ => Block::Unknown,
            // crate::lexer::TokenKind::Bang => todo!(),
            // crate::lexer::TokenKind::At => todo!(),
            // crate::lexer::TokenKind::OpenCurly => todo!(),
            // crate::lexer::TokenKind::CloseCurly => todo!(),
            // crate::lexer::TokenKind::OpenBrace => todo!(),
            // crate::lexer::TokenKind::CloseBrace => todo!(),
            // crate::lexer::TokenKind::Equals => todo!(),
            // crate::lexer::TokenKind::Percent => todo!(),
            // crate::lexer::TokenKind::Text => todo!(),
            // crate::lexer::TokenKind::Newline => todo!(),
            // crate::lexer::TokenKind::EOF => todo!(),
            // crate::lexer::TokenKind::Unknown => todo!(),
        };

        block
    }

    fn parse_metadata(&mut self) -> Block {
        let body = self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::MetadataMarker));
        debug_assert!(self.peek_kind() == Some(TokenKind::MetadataMarker));
        self.advance();

        Block::Metadata(body)
    }

    fn parse_block(&mut self) -> Block {
        debug_assert!(self.peek_kind() == Some(TokenKind::Bang));
        self.advance();

        debug_assert!(self.peek_kind() == Some(TokenKind::Text));
        let name = self.advance();

        let attributes = if self.peek_kind() == Some(TokenKind::OpenCurly) {
            self.advance();
            let attrs = self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::CloseCurly));
            debug_assert!(self.peek_kind() == Some(TokenKind::CloseCurly));
            self.advance();
            attrs
        } else {
            vec![]
        };

        debug_assert!(self.peek_kind() == Some(TokenKind::OpenBrace));
        self.advance();

        let body = self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::CloseBrace));
        debug_assert!(self.peek_kind() == Some(TokenKind::CloseBrace));
        self.advance();

        Block::Block {
            name,
            attributes,
            body,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    const TEST_INPUT: &'static str = r#"
+++
metadata = things
+++

!h1{some=value}[Header] 

With a @bold[body]
 "#;

    #[test]
    fn it_works() {
        let tokens = tokenize(TEST_INPUT).collect::<Vec<_>>();
        let blocks = parse(tokens);
        insta::assert_debug_snapshot!(blocks);
    }
}
