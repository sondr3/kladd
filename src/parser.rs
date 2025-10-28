use crate::{
    lexer::{Token, TokenKind},
    token_cursor::TokenCursor,
};

#[derive(Debug)]
pub enum Block {
    Metadata(Vec<Token>),
    Unknown,
    Whitespace,
    Newline,
    EOF,
}

pub fn parse(input: Vec<Token>) -> impl Iterator<Item = Block> {
    let mut cursor = TokenCursor::new(input);
    std::iter::from_fn(move || match cursor.advance_token() {
        Block::Unknown | Block::EOF => None,
        tok => Some(tok),
    })
}

impl TokenCursor {
    pub fn advance_token(&mut self) -> Block {
        let first_token = self.advance();

        let block = match first_token.kind {
            TokenKind::MetadataMarker => self.parse_metadata(),
            TokenKind::EOF => Block::EOF,
            TokenKind::Unknown => Block::Unknown,
            TokenKind::Whitespace => Block::Whitespace,
            TokenKind::Newline => Block::Newline,
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
        debug_assert!(self.peek().unwrap().kind == TokenKind::MetadataMarker);
        self.advance();

        Block::Metadata(body)
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
        let blocks = parse(tokens).collect::<Vec<_>>();
        insta::assert_debug_snapshot!(blocks);
    }
}
