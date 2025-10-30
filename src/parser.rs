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
    pub name: &'a str,
    pub value: AttributeValue<'a>,
}

#[derive(Debug)]
pub enum Block<'a> {
    /// A block of TOML data
    Metadata(String),
    /// A !h1{attribute=value}[content] block
    Block {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        body: Vec<Block<'a>>,
    },
    /// A @bold[content] inline block
    Inline {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        body: Vec<Block<'a>>,
    },
    /// A @{key=value}[content] block
    NakedInline {
        attributes: Vec<Attribute<'a>>,
        body: Vec<Block<'a>>,
    },
    /// A {/italic/} or {* bold *} simple inline node etc
    SimpleInline {
        name: &'a str,
        body: Box<Block<'a>>,
    },
    TextBlock {
        body: Vec<Block<'a>>,
    },
    /// A single pure text node
    Text {
        body: &'a str,
    },
    /// A {% comment %} comment
    Comment {
        body: String,
    },
    Whitespace,
    Newline,
    Unknown,
    EOF,
}

pub fn parse<'a>(input: Vec<Token<'a>>) -> Vec<Block<'a>> {
    let mut cursor = TokenCursor::new(input);
    let mut res = Vec::new();

    cursor.eat_while(|t| {
        t.is_some_and(|k| matches!(k.kind, TokenKind::Newline | TokenKind::Whitespace))
    });

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

        match first_token.kind {
            TokenKind::EOF => Block::EOF,
            TokenKind::Unknown => Block::Unknown,
            TokenKind::Whitespace => Block::Whitespace,
            TokenKind::Newline => match self.peek_kind() {
                Some(TokenKind::Bang) => self.parse_block(),
                _ => Block::Newline,
            },
            TokenKind::OpenCurly => match self.peek_kind() {
                Some(TokenKind::Percent) => self.parse_comment(),
                _ => self.parse_simple_inline(),
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
        }
    }

    fn parse_metadata(&mut self) -> Block<'a> {
        debug_assert!(self.peek_kind() == Some(TokenKind::MetadataMarker));
        self.advance();

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

        let _body = self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::CloseBrace));
        debug_assert!(self.peek_kind() == Some(TokenKind::CloseBrace));
        self.advance();

        Block::Block {
            name,
            attributes,
            body: vec![],
        }
    }

    fn parse_comment(&mut self) -> Block<'a> {
        debug_assert!(self.peek_kind() == Some(TokenKind::Percent));
        self.advance();

        let body = self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::Percent));
        self.advance();
        debug_assert!(self.peek_kind() == Some(TokenKind::CloseCurly));
        self.advance();

        Block::Comment {
            body: body.iter().map(|c| c.lexeme).collect::<String>(),
        }
    }

    fn parse_simple_inline(&mut self) -> Block<'a> {
        let (t, name) = match self.peek_kind() {
            Some(t @ TokenKind::Slash) => (t, "italic"),
            Some(t @ TokenKind::Star) => (t, "bold"),
            Some(t @ TokenKind::Underscore) => (t, "underline"),
            Some(t @ TokenKind::Equals) => (t, "highlight"),
            Some(t @ TokenKind::Dash) => (t, "strikethrough"),
            _ => panic!("invalid short inline"),
        };

        self.advance();

        let body = Box::new(self.parse_text());

        debug_assert!(self.peek_kind() == Some(t));
        self.advance();
        debug_assert!(self.peek_kind() == Some(TokenKind::CloseCurly));
        self.advance();

        Block::SimpleInline { name, body }
    }

    fn parse_text(&mut self) -> Block<'a> {
        let body = self.eat_while(|t| t.is_some_and(|i| i.kind == TokenKind::Text));
        Block::TextBlock {
            body: body
                .iter()
                .map(|f| Block::Text { body: f.lexeme })
                .collect(),
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
    use crate::{lexer::tokenize, test_utils::test_utils::TEST_INPUT};

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
