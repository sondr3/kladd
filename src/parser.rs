use crate::{
    ast::{
        Attribute, AttributeValue, Block, BlockNode, Document, Inline, InlineKind, InlineNode,
        Inlines, Node, NodeBuilder,
    },
    lexer::{Token, TokenKind},
    token_cursor::TokenCursor,
};

pub fn parse<'a>(input: Vec<Token<'a>>) -> Document<'a> {
    let mut cursor = TokenCursor::new(input);

    cursor.eat_while(|t| {
        t.is_some_and(|k| matches!(k.kind, TokenKind::Newline | TokenKind::Whitespace))
    });

    let metadata = if cursor
        .peek()
        .is_some_and(|t| t.kind == TokenKind::MetadataMarker)
    {
        Some(cursor.parse_metadata())
    } else {
        None
    };

    let body = Vec::from_iter(std::iter::from_fn(move || cursor.advance_token()));

    Document { metadata, body }
}

impl<'a> TokenCursor<'a> {
    pub fn advance_token(&mut self) -> Option<BlockNode<'a>> {
        match self.peek_kind() {
            // Some(TokenKind::EOF) => {
            //     self.advance();
            //     Block::EOF
            // }
            // Some(TokenKind::Unknown) => {
            //     self.advance();
            //     Block::Unknown
            // }
            // Some(TokenKind::Whitespace) => {
            //     self.advance();
            //     Block::Whitespace
            // }
            // Some(TokenKind::Newline) => {
            //     self.advance();
            //     Block::Newline
            // }
            // Some(TokenKind::OpenCurly) => self.parse_container(),
            // Some(TokenKind::Bang) => self.parse_block(),
            Some(TokenKind::Dash) => {
                self.parse_comment();
                None
            }
            None => None,
            _ => None, // _ => self.parse_container(),
        }
    }

    fn parse_metadata(&mut self) -> String {
        debug_assert!(self.peek_kind() == Some(TokenKind::MetadataMarker));
        self.advance();

        let body = self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::MetadataMarker));
        debug_assert!(self.peek_kind() == Some(TokenKind::MetadataMarker));
        self.advance();

        body.iter().map(|t| t.lexeme).collect()
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

        todo!()
    }

    fn parse_container(&mut self) -> Block<'a> {
        let mut body = Vec::new();
        while !matches!(self.peek_kind(), Some(TokenKind::Newline | TokenKind::EOF)) {
            match parse_text(self) {
                Some(b) => body.push(b),
                None => break,
            }
        }
        todo!()
    }

    fn parse_comment(&mut self) {
        debug_assert!(self.peek_kind() == Some(TokenKind::Dash));
        self.advance();

        debug_assert!(self.peek_kind() == Some(TokenKind::Dash));
        self.advance();

        self.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::Newline));
        self.advance();
    }
}

pub fn parse_text<'a>(cursor: &mut TokenCursor<'a>) -> Option<BlockNode<'a>> {
    todo!()
    // match cursor.peek_kind() {
    //     Some(TokenKind::Text) => Some(Block::Text {
    //         body: cursor.advance().lexeme,
    //         quote: None,
    //     }),
    //     Some(TokenKind::Comma | TokenKind::DoubleQuote | TokenKind::SingleQoute) => {
    //         Some(Block::Text {
    //             body: cursor.advance().lexeme,
    //             quote: None,
    //         })
    //     }
    //     Some(TokenKind::At) => Some(parse_inline(cursor)),
    //     Some(TokenKind::OpenCurly) => Some(parse_simple_inline(cursor)),
    //     Some(TokenKind::Whitespace) => {
    //         cursor.advance();
    //         Some(Block::Whitespace)
    //     }
    //     Some(TokenKind::Newline) => {
    //         cursor.advance();
    //         Some(Block::Newline)
    //     }
    //     None => None,
    //     _ => {
    //         panic!("{:?} not yet handled in text", cursor.peek_kind());
    //     }
    // }
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

    todo!()
    // if let Some(name) = name {
    //     Block::Inline {
    //         name,
    //         attributes: attrs,
    //         body,
    //     }
    // } else {
    //     Block::NakedInline {
    //         attributes: attrs,
    //         body,
    //     }
    // }
}

pub fn parse_inlines<'a>(cursor: &mut TokenCursor<'a>) -> InlineNode<'a> {
    match cursor.peek_kind() {
        Some(TokenKind::Comma) => Node::new(Inline::Text(cursor.advance().lexeme), None),
        Some(TokenKind::Text) => Node::new(Inline::Text(cursor.advance().lexeme), None),
        Some(TokenKind::OpenCurly) => parse_simple_inline(cursor),
        Some(TokenKind::Whitespace) => {
            cursor.advance();
            Node::new(Inline::Softbreak, None)
        }
        t => panic!("{:?} is not yet handled", t),
    }
}

pub fn parse_simple_inline<'a>(cursor: &mut TokenCursor<'a>) -> InlineNode<'a> {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenCurly));
    cursor.advance();

    let mut node_builder = NodeBuilder::new();

    let (t, kind) = match cursor.peek_kind() {
        Some(t @ TokenKind::Slash) => (t, InlineKind::Italic),
        Some(t @ TokenKind::Star) => (t, InlineKind::Strong),
        Some(t @ TokenKind::Underscore) => (t, InlineKind::Underline),
        Some(t @ TokenKind::Equals) => (t, InlineKind::Highlight),
        Some(t @ TokenKind::Tilde) => (t, InlineKind::Strikethrough),
        _ => panic!("invalid short inline"),
    };

    cursor.advance();

    let mut body = Vec::new();
    while cursor.peek_kind() != Some(t) && !cursor.is_at_end() {
        body.push(parse_inlines(cursor));
    }

    node_builder.with_node(Inline::from_kind(kind, body));

    debug_assert!(cursor.peek_kind() == Some(t));
    cursor.advance();
    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseCurly));
    cursor.advance();

    node_builder.build()
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

fn is_heading<'a>(cursor: &mut TokenCursor<'a>) -> bool {
    match cursor.peek() {
        Some(Token {
            kind: TokenKind::Bang,
            lexeme,
        }) => matches!(
            *lexeme,
            "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "title" | "section" | "subsection"
        ),
        _ => false,
    }
}

fn parse_heading<'a>(cursor: &mut TokenCursor<'a>) -> BlockNode<'a> {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::Bang));
    cursor.advance();

    let mut builder = NodeBuilder::new();

    debug_assert!(cursor.peek_kind() == Some(TokenKind::Text));
    let level = match cursor.advance() {
        Token {
            kind: TokenKind::Text,
            lexeme,
        } => match lexeme {
            "h1" | "title" => 1,
            "h2" | "section" => 2,
            "h3" | "subsection" => 3,
            "h4" => 4,
            "h5" => 5,
            "h6" => 6,
            e => panic!("{} is not a valid heading level", e),
        },
        t => panic!("{:?} not a valid heading name", t),
    };

    if cursor.peek_kind() == Some(TokenKind::OpenCurly) {
        builder.with_attributes(parse_attributes(cursor));
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenBrace));
    cursor.advance();

    let mut body = Vec::new();
    while cursor.peek_kind() != Some(TokenKind::CloseBrace) && !cursor.is_at_end() {
        body.push(parse_inlines(cursor));
    }

    builder.with_node(Block::Heading { level, body });

    builder.build()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    fn map_inlines<'a, T, const N: usize>(nodes: [T; N]) -> Vec<Node<'a, T>> {
        nodes.into_iter().map(Node::from_node).collect()
    }

    #[test]
    fn test_parse_single_inline() {
        let lexer = tokenize("{*bold *}").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_simple_inline(&mut cursor);

        assert_eq!(
            res,
            InlineNode::new(
                Inline::Strong(vec![InlineNode::new(Inline::Text("bold "), None)]),
                None
            )
        );
    }

    #[test]
    fn test_parse_nested_inlines() {
        let lexer = tokenize("{*bold {/italic {~struck~}/} {=highlit=}*}").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_simple_inline(&mut cursor);

        assert_eq!(
            res,
            InlineNode::from_node(Inline::Strong(vec![
                InlineNode::from_node(Inline::Text("bold ")),
                InlineNode::from_node(Inline::Italic(vec![
                    InlineNode::from_node(Inline::Text("italic ")),
                    InlineNode::from_node(Inline::Strikethrough(vec![InlineNode::from_node(
                        Inline::Text("struck")
                    )])),
                ]),),
                InlineNode::from_node(Inline::Softbreak,),
                InlineNode::from_node(Inline::Highlight(vec![InlineNode::from_node(
                    Inline::Text("highlit"),
                ),]),)
            ]),)
        )
    }

    #[test]
    fn test_parse_headlines() {
        let inputs = vec![
            (
                "!title[Hello, world]",
                BlockNode::from_node(Block::Heading {
                    level: 1,
                    body: map_inlines([
                        Inline::Text("Hello"),
                        Inline::Text(","),
                        Inline::Softbreak,
                        Inline::Text("world"),
                    ]),
                }),
            ),
            (
                "!h2{class=red}[Red title]",
                BlockNode::new(
                    Block::Heading {
                        level: 2,
                        body: map_inlines([Inline::Text("Red title")]),
                    },
                    Some(vec![Attribute::new("class", AttributeValue::String("red"))]),
                ),
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_heading(&mut cursor);

            assert_eq!(res, expected);
        }
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
        let lexer = tokenize("{name1=value,name2,name3=value2 and more}").collect::<Vec<_>>();
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
                    value: AttributeValue::String("value2 and more")
                },
            ]
        );
    }
}
