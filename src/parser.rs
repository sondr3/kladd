use crate::{
    ast::{
        Attribute, AttributeValue, Block, BlockNode, Document, Inline, InlineKind, InlineNode,
        Node, NodeBuilder,
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
        Some(parse_metadata(&mut cursor))
    } else {
        None
    };

    let mut body = Vec::new();
    loop {
        match dbg!(cursor.advance_token()) {
            Some(Some(t)) => body.push(t),
            Some(None) => continue,
            None => break,
        }
    }

    Document { metadata, body }
}

impl<'a> TokenCursor<'a> {
    pub fn advance_token(&mut self) -> Option<Option<BlockNode<'a>>> {
        skip_whitespace(self);
        match self.peek_kind() {
            Some(TokenKind::EOF) => {
                self.advance();
                None
            }
            Some(TokenKind::Dash) => {
                parse_comment(self);
                Some(None)
            }
            None => None,
            _ => Some(parse_section(self)),
        }
    }
}

fn skip_whitespace<'a>(cursor: &mut TokenCursor<'a>) {
    while let Some(TokenKind::Newline | TokenKind::Whitespace) = cursor.peek_kind() {
        cursor.advance();
    }
}

fn parse_comment<'a>(cursor: &mut TokenCursor<'a>) {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::Dash));
    cursor.advance();

    debug_assert!(cursor.peek_kind() == Some(TokenKind::Dash));
    cursor.advance();

    cursor.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::Newline));
    cursor.advance();
}

fn parse_metadata<'a>(cursor: &mut TokenCursor<'a>) -> String {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::MetadataMarker));
    cursor.advance();

    let body = cursor.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::MetadataMarker));
    debug_assert!(cursor.peek_kind() == Some(TokenKind::MetadataMarker));
    cursor.advance();

    body.iter().map(|t| t.lexeme).collect()
}

pub fn parse_inlines<'a>(cursor: &mut TokenCursor<'a>) -> InlineNode<'a> {
    match cursor.peek_kind() {
        Some(TokenKind::Comma) => Node::new(Inline::Text(cursor.advance().lexeme), None),
        Some(TokenKind::Text) => Node::new(Inline::Text(cursor.advance().lexeme), None),
        Some(TokenKind::OpenCurly) => parse_simple_inline(cursor),
        Some(TokenKind::At) => parse_inline(cursor),
        Some(TokenKind::Whitespace) => {
            cursor.advance();
            Node::new(Inline::Softbreak, None)
        }
        t => panic!("{:?} is not yet handled", t),
    }
}

pub fn parse_section<'a>(cursor: &mut TokenCursor<'a>) -> Option<BlockNode<'a>> {
    let mut builder = NodeBuilder::new();

    skip_whitespace(cursor);
    if !is_heading(cursor) {
        return None;
    }

    let mut body = Vec::new();

    body.push(parse_heading(cursor));
    skip_whitespace(cursor);

    while !is_heading(cursor) && !cursor.is_at_end() {
        dbg!(cursor.peek());
        body.push(parse_paragraph(cursor));
        skip_whitespace(cursor);
    }

    builder.with_node(Block::Section(body));
    Some(builder.build())
}

fn is_double_newline<'a>(cursor: &mut TokenCursor<'a>) -> bool {
    match cursor.peek() {
        Some(Token {
            kind: TokenKind::Newline,
            lexeme,
        }) => lexeme.len() >= 2,
        _ => false,
    }
}

pub fn parse_paragraph<'a>(cursor: &mut TokenCursor<'a>) -> BlockNode<'a> {
    let mut builder = NodeBuilder::new();

    let mut body = Vec::new();
    while !is_heading(cursor) && !is_double_newline(cursor) && !cursor.is_at_end() {
        body.push(parse_inlines(cursor));
    }

    builder.with_node(Block::Paragraph(body));

    builder.build()
}

pub fn parse_inline<'a>(cursor: &mut TokenCursor<'a>) -> InlineNode<'a> {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::At));
    cursor.advance();

    let mut builder = NodeBuilder::new();

    let kind = match cursor.peek() {
        Some(Token {
            kind: TokenKind::Text,
            lexeme,
        }) => match *lexeme {
            "bold" | "strong" | "b" => InlineKind::Strong,
            "italic" | "i" => InlineKind::Italic,
            "underline" | "ul" => InlineKind::Underline,
            "highlight" | "hl" => InlineKind::Highlight,
            "strikethrough" | "strike" | "st" => InlineKind::Strikethrough,
            "superscript" | "sup" => InlineKind::Superscript,
            "subscript" | "sub" => InlineKind::Subscript,
            ident => InlineKind::Custom(ident),
        },
        Some(Token {
            kind: TokenKind::OpenCurly | TokenKind::OpenBrace,
            ..
        }) => InlineKind::Naked,
        e => panic!("{:?} cannot be a inline name", e),
    };

    if kind != InlineKind::Naked {
        cursor.advance();
    }

    if cursor.peek_kind() == Some(TokenKind::OpenCurly) {
        builder.with_attributes(parse_attributes(cursor));
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenBrace));
    cursor.advance();

    let mut body = Vec::new();
    while cursor.peek_kind() != Some(TokenKind::CloseBrace) && !cursor.is_at_end() {
        body.push(parse_inlines(cursor));
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseBrace));
    cursor.advance();

    builder.with_node(Inline::from_kind(kind, body));

    builder.build()
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

fn is_heading<'a>(cursor: &TokenCursor<'a>) -> bool {
    match (cursor.peek(), cursor.peek_nth(1)) {
        (
            Some(Token {
                kind: TokenKind::Bang,
                ..
            }),
            Some(Token {
                kind: TokenKind::Text,
                lexeme,
            }),
        ) => matches!(
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

    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseBrace));
    cursor.advance();

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
    fn test_single_inline() {
        let lexer = tokenize("@bold[body]").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_inline(&mut cursor);

        assert_eq!(
            res,
            InlineNode::new(
                Inline::Strong(vec![InlineNode::new(Inline::Text("body"), None)]),
                None
            )
        );
    }

    #[test]
    fn test_parse_inlines() {
        let inputs = vec![
            (
                "@{class=huge}[Huge]",
                InlineNode::new(
                    Inline::Naked(map_inlines([Inline::Text("Huge")])),
                    Some(vec![Attribute::new(
                        "class",
                        AttributeValue::String("huge"),
                    )]),
                ),
            ),
            (
                "@sup[super]",
                InlineNode::from_node(Inline::Superscript(map_inlines([Inline::Text("super")]))),
            ),
            (
                "@custom[yeet]",
                InlineNode::from_node(Inline::Custom {
                    name: "custom",
                    body: map_inlines([Inline::Text("yeet")]),
                }),
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_inline(&mut cursor);

            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_single_simple_inline() {
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
    fn test_parse_nested_simple_inlines() {
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
