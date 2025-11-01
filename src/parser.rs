use crate::{
    lexer::{Token, TokenKind},
    token_cursor::TokenCursor,
};

#[derive(Debug, PartialEq, Eq)]
pub enum AttributeValue<'a> {
    String(&'a str),
    Boolean,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Attribute<'a> {
    pub name: &'a str,
    pub value: AttributeValue<'a>,
}

#[derive(Debug)]
pub enum Block<'a> {
    /// A block of TOML data
    Metadata(String),
    /// A `!h1{attribute=value}[content]` block
    Block {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        body: Vec<Block<'a>>,
    },
    /// A `@bold[content]` inline block
    Inline {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        body: Vec<Block<'a>>,
    },
    /// A `@{key=value}[content]` block
    NakedInline {
        attributes: Vec<Attribute<'a>>,
        body: Vec<Block<'a>>,
    },
    /// A `{/italic/}` or `{*bold*}` simple inline node etc
    SimpleInline {
        name: &'a str,
        body: Vec<Block<'a>>,
    },
    Container {
        body: Vec<Block<'a>>,
    },
    /// A single pure text node
    Text(&'a str),
    /// A `{% comment %}` comment
    Comment {
        body: String,
    },
    Whitespace,
    Newline,
    Unknown,
    EOF,
}

#[derive(Debug)]
pub struct Document<'a> {
    pub metadata: Option<String>,
    pub body: Vec<Block<'a>>,
}

impl<'a> Default for Document<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Document<'a> {
    pub fn new() -> Self {
        Document {
            metadata: None,
            body: Vec::new(),
        }
    }

    pub fn add_metadata(&mut self, metadata: Block) {
        match metadata {
            Block::Metadata(body) => self.metadata = Some(body),
            _ => panic!("invalid metadata block"),
        }
    }

    pub fn add_body(&mut self, body: &mut Vec<Block<'a>>) {
        self.body.append(body);
    }
}

pub fn parse<'a>(input: Vec<Token<'a>>) -> Document<'a> {
    let mut cursor = TokenCursor::new(input);
    let mut doc = Document::new();
    let mut body = Vec::new();

    cursor.eat_while(|t| {
        t.is_some_and(|k| matches!(k.kind, TokenKind::Newline | TokenKind::Whitespace))
    });

    if cursor
        .peek()
        .is_some_and(|t| t.kind == TokenKind::MetadataMarker)
    {
        doc.add_metadata(cursor.parse_metadata());
    };

    body.extend(std::iter::from_fn(move || match cursor.advance_token() {
        Block::Unknown | Block::EOF => None,
        tok => Some(tok),
    }));

    doc.add_body(&mut body);

    doc
}

impl<'a> TokenCursor<'a> {
    pub fn advance_token(&mut self) -> Block<'a> {
        match self.peek_kind() {
            Some(TokenKind::EOF) => {
                self.advance();
                Block::EOF
            }
            Some(TokenKind::Unknown) => {
                self.advance();
                Block::Unknown
            }
            Some(TokenKind::Whitespace) => {
                self.advance();
                Block::Whitespace
            }
            Some(TokenKind::Newline) => {
                self.advance();
                Block::Newline
            }
            Some(TokenKind::OpenCurly) => self.parse_container(),
            Some(TokenKind::Bang) => self.parse_block(),
            Some(TokenKind::Dash) => self.parse_comment(),
            None => Block::EOF,
            _ => self.parse_container(),
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
            parse_attributes(self)
        } else {
            vec![]
        };

        debug_assert!(self.peek_kind() == Some(TokenKind::OpenBrace));
        self.advance();

        let mut body = Vec::new();
        while self.peek_kind() != Some(TokenKind::CloseBrace) && !self.is_at_end() {
            match parse_text(self) {
                Some(b) => body.push(b),
                None => break,
            }
        }

        debug_assert!(self.peek_kind() == Some(TokenKind::CloseBrace));
        self.advance();

        Block::Block {
            name,
            attributes,
            body,
        }
    }

    fn parse_container(&mut self) -> Block<'a> {
        let mut body = Vec::new();
        while !matches!(self.peek_kind(), Some(TokenKind::Newline | TokenKind::EOF)) {
            match parse_text(self) {
                Some(b) => body.push(b),
                None => break,
            }
        }
        Block::Container { body }
    }

    fn parse_comment(&mut self) -> Block<'a> {
        debug_assert!(self.peek_kind() == Some(TokenKind::Dash));
        self.advance();

        debug_assert!(self.peek_kind() == Some(TokenKind::Dash));
        self.advance();

        let body = self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::Newline));
        self.advance();

        Block::Comment {
            body: body.iter().map(|c| c.lexeme).collect::<String>(),
        }
    }

    fn parse_simple_inline(&mut self) -> Block<'a> {
        debug_assert!(self.peek_kind() == Some(TokenKind::OpenCurly));
        self.advance();

        let (t, name) = match self.peek_kind() {
            Some(t @ TokenKind::Slash) => (t, "italic"),
            Some(t @ TokenKind::Star) => (t, "bold"),
            Some(t @ TokenKind::Underscore) => (t, "underline"),
            Some(t @ TokenKind::Equals) => (t, "highlight"),
            Some(t @ TokenKind::Dash) => (t, "strikethrough"),
            _ => panic!("invalid short inline"),
        };

        self.advance();

        let mut body = Vec::new();
        while self.peek_kind() != Some(t) && !self.is_at_end() {
            match parse_text(self) {
                Some(b) => body.push(b),
                None => break,
            }
        }

        debug_assert!(self.peek_kind() == Some(t));
        self.advance();
        debug_assert!(self.peek_kind() == Some(TokenKind::CloseCurly));
        self.advance();

        Block::SimpleInline { name, body }
    }
}

fn parse_text<'a>(cursor: &mut TokenCursor<'a>) -> Option<Block<'a>> {
    match cursor.peek_kind() {
        Some(TokenKind::Text) => Some(Block::Text(cursor.advance().lexeme)),
        Some(TokenKind::At) => Some(parse_inline(cursor)),
        Some(TokenKind::OpenCurly) => Some(cursor.parse_simple_inline()),
        Some(TokenKind::Whitespace) => {
            cursor.advance();
            Some(Block::Whitespace)
        }
        Some(TokenKind::Newline) => {
            cursor.advance();
            Some(Block::Newline)
        }
        None => None,
        _ => {
            panic!("{:?} not yet handled in text", cursor.peek_kind());
        }
    }
}

fn parse_inline<'a>(cursor: &mut TokenCursor<'a>) -> Block<'a> {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::At));
    cursor.advance();

    let name = match cursor.peek_kind() {
        Some(TokenKind::Text) => Some(cursor.advance().lexeme),
        Some(TokenKind::OpenCurly | TokenKind::OpenBrace) => None,
        _ => panic!("invalid inline item"),
    };

    let attrs = if matches!(cursor.peek_kind(), Some(TokenKind::OpenCurly)) {
        parse_attributes(cursor)
    } else {
        vec![]
    };

    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenBrace));
    cursor.advance();

    let mut body = Vec::new();
    while cursor.peek_kind() != Some(TokenKind::CloseBrace) && !cursor.is_at_end() {
        match parse_text(cursor) {
            Some(b) => body.push(b),
            None => break,
        }
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseBrace));
    cursor.advance();

    if let Some(name) = name {
        Block::Inline {
            name,
            attributes: attrs,
            body,
        }
    } else {
        Block::NakedInline {
            attributes: attrs,
            body,
        }
    }
}

fn parse_attributes<'a>(cursor: &mut TokenCursor<'a>) -> Vec<Attribute<'a>> {
    let mut res = Vec::new();

    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenCurly));
    cursor.advance();

    loop {
        res.push(parse_attribute(cursor));

        if cursor.peek_kind() == Some(TokenKind::Comma) {
            cursor.advance();
        } else if cursor.peek_kind() == Some(TokenKind::CloseCurly) {
            cursor.advance();
            break;
        }
    }

    res
}

fn parse_attribute<'a>(cursor: &mut TokenCursor<'a>) -> Attribute<'a> {
    let name = cursor
        .advance_if(|t| t.is_some_and(|k| k.kind == TokenKind::Text))
        .lexeme;

    let value = match cursor.peek_kind() {
        Some(TokenKind::Comma) | None => AttributeValue::Boolean,
        Some(TokenKind::Equals) => {
            cursor.advance();
            AttributeValue::String(
                cursor
                    .advance_if(|t| t.is_some_and(|k| k.kind == TokenKind::Text))
                    .lexeme,
            )
        }
        Some(_) => panic!("invalid attribute value"),
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

    #[test]
    fn test_parse_multiple_attributes() {
        let lexer = tokenize("{name1=value,name2,name3=value2}").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_attributes(&mut cursor);

        assert_eq!(
            res,
            vec![
                Attribute {
                    name: "name1",
                    value: AttributeValue::String("value")
                },
                Attribute {
                    name: "name2",
                    value: AttributeValue::Boolean
                },
                Attribute {
                    name: "name3",
                    value: AttributeValue::String("value2")
                },
            ]
        );
    }
}
