use crate::{
    ast::{
        Attribute, AttributeValue, Block, BlockNode, Document, Inline, InlineKind, InlineNode,
        Node, NodeBuilder,
    },
    lexer::{Token, TokenKind},
    token_cursor::TokenCursor,
};

pub fn parse(input: Vec<Token>) -> Document {
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
        match cursor.advance_token() {
            ParseResult::Parsed(t) => body.push(t),
            ParseResult::Skipped => continue,
            ParseResult::Nothing => break,
            ParseResult::Error(e) => panic!("{}", e),
        }
    }

    Document { metadata, body }
}

#[derive(Debug, Clone)]
pub enum ParseResult<T> {
    /// Something was successfully parsed
    Parsed(T),
    /// Whatever was parsed was skipped
    Skipped,
    /// The parser didn't parse anything
    Nothing,
    /// Ruh-roh, things went boom
    Error(String),
}

impl<T> ParseResult<T> {
    pub fn unwrap(self) -> T {
        match self {
            ParseResult::Parsed(t) => t,
            ParseResult::Skipped => panic!("tried to unwrap on a Skipped"),
            ParseResult::Nothing => panic!("tried to unwrap on a Nothing"),
            ParseResult::Error(_) => panic!("tried to unwrap on a Error"),
        }
    }
}

impl TokenCursor<'_> {
    pub fn advance_token(&mut self) -> ParseResult<BlockNode> {
        skip_whitespace(self);
        match self.peek_kind() {
            Some(TokenKind::Eof) => {
                self.advance();
                ParseResult::Nothing
            }
            Some(TokenKind::Dash) => {
                parse_comment(self);
                ParseResult::Skipped
            }
            None => ParseResult::Nothing,
            Some(TokenKind::Bang) => parse_bang_node(self),
            Some(TokenKind::Hashbang) => barse_hash_block(self),
            _ => parse_block(self),
        }
    }
}

fn skip_whitespace(cursor: &mut TokenCursor) {
    while let Some(TokenKind::Newline | TokenKind::Whitespace) = cursor.peek_kind() {
        cursor.advance();
    }
}

fn parse_comment(cursor: &mut TokenCursor) {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::Dash));
    cursor.advance();

    debug_assert!(cursor.peek_kind() == Some(TokenKind::Dash));
    cursor.advance();

    cursor.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::Newline));
    cursor.advance();
}

fn parse_metadata(cursor: &mut TokenCursor) -> String {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::MetadataMarker));
    cursor.advance();

    let body = cursor.eat_while(|t| t.is_some_and(|i| i.kind != TokenKind::MetadataMarker));
    debug_assert!(cursor.peek_kind() == Some(TokenKind::MetadataMarker));
    cursor.advance();

    body.iter().map(|t| t.lexeme).collect()
}

pub fn parse_inlines(cursor: &mut TokenCursor) -> ParseResult<InlineNode> {
    match cursor.peek_kind() {
        Some(
            TokenKind::Comma
            | TokenKind::Text
            | TokenKind::Whitespace
            | TokenKind::DoubleQuote
            | TokenKind::SingleQoute
            | TokenKind::Bang,
        ) => ParseResult::Parsed(Node::new(
            Inline::Text(cursor.advance().lexeme.to_string()),
            None,
        )),
        Some(TokenKind::OpenCurly) => ParseResult::Parsed(parse_simple_inline(cursor).unwrap()),
        Some(TokenKind::At) => parse_inline(cursor),
        Some(TokenKind::Newline) => {
            cursor.advance();
            ParseResult::Parsed(Node::new(Inline::Softbreak, None))
        }
        t => ParseResult::Error(format!("{:?} is not yet handled", t)),
    }
}

pub fn parse_bang_node(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    if is_heading(cursor) {
        parse_heading(cursor)
    } else {
        todo!()
    }
}

pub fn barse_hash_block(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    if is_section(cursor) {
        parse_section(cursor)
    } else {
        todo!()
    }
}

fn is_section(cursor: &TokenCursor) -> bool {
    matches!((cursor.peek(), cursor.peek_nth(1)), (
            Some(Token {
                kind: TokenKind::Hashbang,
                ..
            }),
            Some(Token {
                kind: TokenKind::Text,
                lexeme,
            }),
        ) if *lexeme == "section")
}

fn is_block_end(cursor: &mut TokenCursor) -> bool {
    matches!(
        (cursor.peek(), cursor.peek_nth(1), cursor.peek_nth(2)),
        (
            Some(Token {
                kind: TokenKind::Hashbang,
                ..
            }),
            Some(Token {
                kind: TokenKind::Text,
                ..
            }),
            Some(Token {
                kind: TokenKind::Newline,
                ..
            }),
        )
    )
}

fn is_section_end(cursor: &mut TokenCursor, curr_section: &str) -> bool {
    matches!((cursor.peek(), cursor.peek_nth(1), cursor.peek_nth(2)), (
            Some(Token {
                kind: TokenKind::Hashbang,
                ..
            }),
            Some(Token {
                kind: TokenKind::Text,
                lexeme,
            }),
            Some(Token {
                kind: TokenKind::Newline,
                ..
            }),
        ) if *lexeme == curr_section)
}

fn parse_section(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    if cursor.peek_kind() != Some(TokenKind::Hashbang) {
        return ParseResult::Nothing;
    }

    cursor.advance();
    let section = cursor.advance().lexeme;

    let mut builder = NodeBuilder::new();
    if cursor.peek_kind() == Some(TokenKind::OpenCurly) {
        builder.with_attributes(parse_attributes(cursor));
    }

    skip_whitespace(cursor);
    let mut body = Vec::new();

    while !is_section_end(cursor, section) && !cursor.is_at_end() {
        match try_parsers(vec![parse_paragraph, parse_heading], cursor) {
            ParseResult::Parsed(p) => body.push(p),
            ParseResult::Skipped => continue,
            ParseResult::Nothing => continue,
            ParseResult::Error(e) => panic!("{:?}", e),
        }
        skip_whitespace(cursor);
    }

    if !is_section_end(cursor, section) {
        return ParseResult::Error("No section end found".into());
    }

    cursor.advance();
    cursor.advance();

    builder.with_node(Block::Section(body));

    ParseResult::Parsed(builder.build())
}

pub fn parse_block(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    let mut builder = NodeBuilder::new();

    skip_whitespace(cursor);
    let mut body = Vec::new();

    while !is_heading(cursor) && !cursor.is_at_end() {
        match parse_paragraph(cursor) {
            ParseResult::Parsed(p) => body.push(p),
            _ => todo!(),
        }
        skip_whitespace(cursor);
    }

    builder.with_node(Block::Block(body));
    ParseResult::Parsed(builder.build())
}

fn is_double_newline(cursor: &mut TokenCursor) -> bool {
    match cursor.peek() {
        Some(Token {
            kind: TokenKind::Newline,
            lexeme,
        }) => lexeme.len() >= 2,
        _ => false,
    }
}

fn is_special_token(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::At | TokenKind::OpenCurly | TokenKind::Newline | TokenKind::Hashbang
    )
}

fn parse_text(cursor: &mut TokenCursor) -> ParseResult<InlineNode> {
    let text: String = cursor
        .advance_while(|c| c.peek_kind().is_some_and(|k| !is_special_token(k)))
        .iter()
        .map(|t| t.lexeme)
        .collect();

    if text.is_empty() {
        ParseResult::Skipped
    } else {
        ParseResult::Parsed(InlineNode::from_node(Inline::Text(text)))
    }
}

fn parse_newline(cursor: &mut TokenCursor) -> ParseResult<InlineNode> {
    match cursor.peek() {
        Some(Token {
            kind: TokenKind::Newline,
            lexeme: "\n",
        }) => {
            cursor.advance();
            ParseResult::Parsed(InlineNode::from_node(Inline::Softbreak))
        }
        _ => ParseResult::Nothing,
    }
}

fn try_parsers<F, T>(parsers: Vec<F>, cursor: &mut TokenCursor) -> ParseResult<T>
where
    F: Fn(&mut TokenCursor) -> ParseResult<T>,
{
    for parser in parsers {
        match parser(cursor) {
            r @ ParseResult::Parsed(_) => return r,
            ParseResult::Skipped => continue,
            ParseResult::Nothing => continue,
            e @ ParseResult::Error(_) => return e,
        }
    }

    ParseResult::Nothing
}

pub fn parse_paragraph(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    if is_heading(cursor) {
        return ParseResult::Nothing;
    }

    let mut builder = NodeBuilder::new();

    let mut body = Vec::new();
    while !is_block_end(cursor) && !is_double_newline(cursor) && !cursor.is_at_end() {
        match try_parsers(
            vec![parse_text, parse_simple_inline, parse_inline, parse_newline],
            cursor,
        ) {
            ParseResult::Parsed(p) => body.push(p),
            ParseResult::Skipped => continue,
            ParseResult::Nothing => continue,
            ParseResult::Error(e) => return ParseResult::Error(e),
        }
    }

    builder.with_node(Block::Paragraph(body));

    ParseResult::Parsed(builder.build())
}

pub fn parse_inline(cursor: &mut TokenCursor) -> ParseResult<InlineNode> {
    if cursor.peek_kind() != Some(TokenKind::At) {
        return ParseResult::Nothing;
    }

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
            ident => InlineKind::Custom(ident.to_string()),
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
        match parse_inlines(cursor) {
            ParseResult::Parsed(p) => body.push(p),
            ParseResult::Skipped => todo!(),
            ParseResult::Nothing => todo!(),
            ParseResult::Error(_) => todo!(),
        }
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseBrace));
    cursor.advance();

    builder.with_node(Inline::from_kind(kind, body));

    ParseResult::Parsed(builder.build())
}

fn is_valid_simple_inline(kind: Option<TokenKind>) -> bool {
    matches!(
        kind,
        Some(
            TokenKind::Slash
                | TokenKind::Star
                | TokenKind::Underscore
                | TokenKind::Equals
                | TokenKind::Tilde
        )
    )
}

pub fn parse_simple_inline(cursor: &mut TokenCursor) -> ParseResult<InlineNode> {
    if cursor.peek_kind() != Some(TokenKind::OpenCurly)
        && !is_valid_simple_inline(cursor.peek_nth_kind(1))
    {
        return ParseResult::Nothing;
    }

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
        match parse_inlines(cursor) {
            ParseResult::Parsed(p) => body.push(p),
            ParseResult::Skipped => todo!(),
            ParseResult::Nothing => todo!(),
            ParseResult::Error(_) => todo!(),
        }
    }

    node_builder.with_node(Inline::from_kind(kind, body));

    debug_assert!(cursor.peek_kind() == Some(t));
    cursor.advance();
    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseCurly));
    cursor.advance();

    ParseResult::Parsed(node_builder.build())
}

fn parse_attributes(cursor: &mut TokenCursor) -> Vec<Attribute> {
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

fn parse_attribute(cursor: &mut TokenCursor) -> Attribute {
    let name = cursor
        .advance_if(|t| t.is_some_and(|k| k.kind == TokenKind::Text))
        .lexeme
        .to_string();

    let value = match cursor.peek_kind() {
        Some(TokenKind::Comma) | None => AttributeValue::Boolean,
        Some(TokenKind::Equals) => {
            cursor.advance();
            AttributeValue::String(
                cursor
                    .advance_if(|t| t.is_some_and(|k| k.kind == TokenKind::Text))
                    .lexeme
                    .to_string(),
            )
        }
        Some(_) => panic!("invalid attribute value"),
    };

    Attribute { name, value }
}

fn is_heading(cursor: &TokenCursor) -> bool {
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
        ) => matches!(*lexeme, "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "title"),
        _ => false,
    }
}

fn parse_heading(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
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
            "h2" => 2,
            "h3" => 3,
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
        match parse_inlines(cursor) {
            ParseResult::Parsed(p) => body.push(p),
            ParseResult::Skipped => todo!(),
            ParseResult::Nothing => todo!(),
            ParseResult::Error(_) => todo!(),
        }
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseBrace));
    cursor.advance();

    builder.with_node(Block::Heading { level, body });

    ParseResult::Parsed(builder.build())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    fn map_inlines<T, const N: usize>(nodes: [T; N]) -> Vec<Node<T>> {
        nodes.into_iter().map(Node::from_node).collect()
    }

    #[test]
    fn test_single_inline() {
        let lexer = tokenize("@bold[body]").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_inline(&mut cursor).unwrap();

        assert!(cursor.is_at_end());
        assert_eq!(
            res,
            InlineNode::new(
                Inline::Strong(vec![InlineNode::new(
                    Inline::Text("body".to_string()),
                    None
                )]),
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
                    Inline::Naked(map_inlines([Inline::Text("Huge".to_string())])),
                    Some(vec![Attribute::new(
                        "class".to_string(),
                        AttributeValue::String("huge".to_string()),
                    )]),
                ),
            ),
            (
                "@sup[super]",
                InlineNode::from_node(Inline::Superscript(map_inlines([Inline::Text(
                    "super".to_string(),
                )]))),
            ),
            (
                "@custom[yeet]",
                InlineNode::from_node(Inline::Custom {
                    name: "custom".to_string(),
                    body: map_inlines([Inline::Text("yeet".to_string())]),
                }),
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_inline(&mut cursor).unwrap();

            assert!(cursor.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_single_simple_inline() {
        let lexer = tokenize("{*bold *}").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_simple_inline(&mut cursor).unwrap();

        assert!(cursor.is_at_end());
        assert_eq!(
            res,
            InlineNode::new(
                Inline::Strong(vec![InlineNode::new(
                    Inline::Text("bold ".to_string()),
                    None
                )]),
                None
            )
        );
    }

    #[test]
    fn test_parse_nested_simple_inlines() {
        let lexer = tokenize("{*bold {/italic {~struck~}/} {=highlit=}*}").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_simple_inline(&mut cursor).unwrap();

        assert!(cursor.is_at_end());
        assert_eq!(
            res,
            InlineNode::from_node(Inline::Strong(vec![
                InlineNode::from_node(Inline::Text("bold ".to_string())),
                InlineNode::from_node(Inline::Italic(vec![
                    InlineNode::from_node(Inline::Text("italic ".to_string())),
                    InlineNode::from_node(Inline::Strikethrough(vec![InlineNode::from_node(
                        Inline::Text("struck".to_string())
                    )])),
                ]),),
                InlineNode::from_node(Inline::Text(" ".to_string())),
                InlineNode::from_node(Inline::Highlight(vec![InlineNode::from_node(
                    Inline::Text("highlit".to_string()),
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
                        Inline::Text("Hello".to_string()),
                        Inline::Text(",".to_string()),
                        Inline::Text(" ".to_string()),
                        Inline::Text("world".to_string()),
                    ]),
                }),
            ),
            (
                "!h2{class=red}[Red title]",
                BlockNode::new(
                    Block::Heading {
                        level: 2,
                        body: map_inlines([Inline::Text("Red title".to_string())]),
                    },
                    Some(vec![Attribute::new(
                        "class".to_string(),
                        AttributeValue::String("red".to_string()),
                    )]),
                ),
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_heading(&mut cursor).unwrap();

            assert!(cursor.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_attributes() {
        let attributes = vec![
            ("name", AttributeValue::Boolean),
            ("name2=value", AttributeValue::String("value".to_string())),
        ];
        for (attr, expected) in attributes {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_attribute(&mut cursor);

            assert!(cursor.is_at_end());
            assert_eq!(res.value, expected);
        }
    }

    #[test]
    fn test_parse_multiple_attributes() {
        let lexer = tokenize("{name1=value,name2,name3=value2 and more}").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_attributes(&mut cursor);

        assert!(cursor.is_at_end());
        assert_eq!(
            res,
            vec![
                Attribute {
                    name: "name1".to_string(),
                    value: AttributeValue::String("value".to_string())
                },
                Attribute {
                    name: "name2".to_string(),
                    value: AttributeValue::Boolean
                },
                Attribute {
                    name: "name3".to_string(),
                    value: AttributeValue::String("value2 and more".to_string())
                },
            ]
        );
    }
}
