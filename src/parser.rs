use crate::{
    lexer::{Token, TokenKind},
    token_cursor::TokenCursor,
};

#[derive(Debug, PartialEq, Eq)]
pub enum AttributeValue<'a> {
    String(&'a str),
    Boolean,
}

#[derive(Debug)]
pub struct Attribute<'a> {
    name: &'a str,
    value: AttributeValue<'a>,
}

#[derive(Debug)]
pub enum Block<'a> {
    Metadata(String),
    Block {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        body: Vec<&'a str>,
    },
    Whitespace,
    Newline,
    Unknown,
    EOF,
}

pub fn parse<'a>(input: Vec<Token<'a>>) -> Vec<Block<'a>> {
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

impl<'a> TokenCursor<'a> {
    pub fn advance_token(&mut self) -> Block<'a> {
        let first_token = self.advance();

        let block = match first_token.kind {
            TokenKind::MetadataMarker => self.parse_metadata(),
            TokenKind::EOF => Block::EOF,
            TokenKind::Unknown => Block::Unknown,
            TokenKind::Whitespace => Block::Whitespace,
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

    fn parse_metadata(&mut self) -> Block<'a> {
        let body = self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::MetadataMarker));
        debug_assert!(self.peek_kind() == Some(TokenKind::MetadataMarker));
        self.advance();

        let body = body.iter().map(|t| t.lexeme).collect();

        Block::Metadata(body)
    }

    fn parse_block(&mut self) -> Block<'a> {
        debug_assert!(self.peek_kind() == Some(TokenKind::Bang));
        self.advance();

        debug_assert!(self.peek_kind() == Some(TokenKind::Text));
        let name = self.advance().lexeme;

        let attributes = if self.peek_kind() == Some(TokenKind::OpenCurly) {
            self.advance();
            let attrs = parse_attribute(self);
            debug_assert!(self.peek_kind() == Some(TokenKind::CloseCurly));
            self.advance();
            vec![attrs]
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
            body: vec![],
        }
    }
}

fn parse_attribute<'a>(cursor: &mut TokenCursor<'a>) -> Attribute<'a> {
    let name = cursor
        .advance_if(|t| t.is_some_and(|k| k.kind == TokenKind::Text))
        .lexeme;

    let value = match cursor.peek_kind() {
        Some(TokenKind::Comma) => AttributeValue::Boolean,
        Some(TokenKind::Equals) => {
            cursor.advance();
            AttributeValue::String(
                cursor
                    .advance_if(|t| t.is_some_and(|k| k.kind == TokenKind::Text))
                    .lexeme,
            )
        }
        Some(_) => panic!("invalid attribute value"),
        None => AttributeValue::Boolean,
    };

    Attribute { name, value }
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

    #[test]
    fn test_parse_attributes() {
        let attributes = vec![
            ("name", AttributeValue::Boolean),
            ("name2=value", AttributeValue::String("value")),
        ];
        for (attr, expected) in attributes {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_attribute(&mut cursor);

            assert_eq!(res.value, expected);
        }
    }
}
