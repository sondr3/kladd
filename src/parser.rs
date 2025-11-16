use crate::{
    ast::{
        Attribute, AttributeKind, AttributeValue, Attributes, Block, BlockNode, Document, Inline,
        InlineKind, InlineNode, Inlines, Node, NodeBuilder,
    },
    error::ParsingError,
    lexer::{Token, TokenKind},
    token_cursor::TokenCursor,
};

pub fn parse(input: Vec<Token>) -> Result<Document, ParsingError> {
    let mut cursor = TokenCursor::new(input);

    cursor.eat_while(|k| matches!(k.kind, TokenKind::Newline | TokenKind::Whitespace));

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
            Ok(Parsed::Some(t)) => body.push(t),
            Ok(Parsed::Skipped) => continue,
            Ok(Parsed::Nothing) => break,
            Err(e) => return Err(e),
        }
    }

    Ok(Document { metadata, body })
}

type ParseResult<T> = Result<Parsed<T>, ParsingError>;

#[derive(Debug, Clone)]
pub enum Parsed<T> {
    /// Something was successfully parsed
    Some(T),
    /// Whatever was parsed was skipped
    Skipped,
    /// The parser didn't parse anything
    Nothing,
}

impl<T> Parsed<T> {
    pub fn unwrap(self) -> T {
        match self {
            Parsed::Some(t) => t,
            Parsed::Skipped => panic!("tried to unwrap on a Skipped"),
            Parsed::Nothing => panic!("tried to unwrap on a Nothing"),
        }
    }
}

impl TokenCursor<'_> {
    pub fn advance_token(&mut self) -> ParseResult<BlockNode> {
        skip_whitespace(self);
        match self.peek_kind() {
            Some(TokenKind::Eof) => {
                self.advance();
                Ok(Parsed::Nothing)
            }
            Some(TokenKind::Dash) => {
                parse_comment(self);
                Ok(Parsed::Skipped)
            }
            None => Ok(Parsed::Nothing),
            Some(TokenKind::Bang) => parse_bang_node(self),
            Some(TokenKind::ForwardSlash) => parse_named_block(self),
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

    cursor.eat_while(|t| t.kind != TokenKind::Newline);
    cursor.advance();
}

fn parse_metadata(cursor: &mut TokenCursor) -> String {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::MetadataMarker));
    cursor.advance();

    let body = cursor.eat_while(|t| t.kind != TokenKind::MetadataMarker);
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
            | TokenKind::Bang
            | TokenKind::Dot,
        ) => Ok(Parsed::Some(Node::new(
            Inline::Text(cursor.advance().lexeme.to_string()),
            None,
        ))),
        Some(TokenKind::OpenCurly) => parse_simple_inline(cursor),
        Some(TokenKind::At) => parse_inline(cursor),
        Some(TokenKind::Newline) => {
            cursor.advance();
            Ok(Parsed::Some(Node::new(Inline::Softbreak, None)))
        }
        Some(t) => Err(ParsingError::UnexpectedTokenKind(t, "inline")),
        None => Err(ParsingError::UnexpectedEnd),
    }
}

pub fn parse_bang_node(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    if is_heading(cursor) {
        parse_heading(cursor)
    } else {
        todo!()
    }
}

pub fn parse_named_block(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    if is_named_block(cursor) {
        parse_section(cursor)
    } else {
        Ok(Parsed::Nothing)
    }
}

fn is_named_block(cursor: &TokenCursor) -> bool {
    match cursor.peek_kind() {
        Some(TokenKind::ForwardSlash) => matches!(
            cursor.peek_nth(1),
            Some(Token {
                kind: TokenKind::Text,
                ..
            })
        ),
        _ => false,
    }
}

fn is_block_end(cursor: &mut TokenCursor, is_lexeme: Option<&str>) -> bool {
    match cursor.peek_kind() {
        Some(TokenKind::BackwardSlash) => matches!((cursor.peek_nth(1), cursor.peek_nth_kind(2)), (
                Some(Token {
                    kind: TokenKind::Text,
                    lexeme,
                }),
                Some(TokenKind::Newline | TokenKind::Eof),
            ) if is_lexeme.is_none_or(|v| v == *lexeme)),
        _ => false,
    }
}

fn parse_section(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    if cursor.peek_kind() != Some(TokenKind::ForwardSlash) {
        return Ok(Parsed::Nothing);
    }

    cursor.advance();
    let name = cursor.advance().lexeme;

    let mut builder = NodeBuilder::new();
    if cursor.peek_kind() == Some(TokenKind::OpenCurly) {
        builder.with_attributes(parse_attributes(cursor)?);
    }

    skip_whitespace(cursor);
    let mut body = Vec::new();

    while !is_block_end(cursor, Some(name)) && !cursor.is_at_end() {
        match try_parsers(vec![parse_paragraph, parse_heading], cursor) {
            Ok(Parsed::Some(p)) => body.push(p),
            Ok(Parsed::Skipped) => continue,
            Ok(Parsed::Nothing) => continue,
            Err(e) => return Err(e),
        }
        skip_whitespace(cursor);
    }

    if !is_block_end(cursor, Some(name)) {
        return Err(ParsingError::MissingBlockEnd(name.to_owned()));
    }

    cursor.advance();
    cursor.advance();

    match name {
        "section" => builder.with_node(Block::Section(body)),
        _ => builder.with_node(Block::Named {
            name: name.to_owned(),
            body,
        }),
    };

    Ok(Parsed::Some(builder.build()))
}

pub fn parse_block(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    let mut builder = NodeBuilder::new();

    skip_whitespace(cursor);
    let mut body = Vec::new();

    while !is_heading(cursor) && !cursor.is_at_end() {
        match parse_paragraph(cursor) {
            Ok(Parsed::Some(p)) => body.push(p),
            _ => todo!(),
        }
        skip_whitespace(cursor);
    }

    builder.with_node(Block::Block(body));
    Ok(Parsed::Some(builder.build()))
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
        Ok(Parsed::Skipped)
    } else {
        Ok(Parsed::Some(InlineNode::from_node(Inline::Text(text))))
    }
}

fn parse_newline(cursor: &mut TokenCursor) -> ParseResult<InlineNode> {
    match cursor.peek() {
        Some(Token {
            kind: TokenKind::Newline,
            lexeme: "\n",
        }) => {
            cursor.advance();
            Ok(Parsed::Some(InlineNode::from_node(Inline::Softbreak)))
        }
        _ => Ok(Parsed::Nothing),
    }
}

fn try_parsers<F, T>(parsers: Vec<F>, cursor: &mut TokenCursor) -> ParseResult<T>
where
    F: Fn(&mut TokenCursor) -> ParseResult<T>,
{
    for parser in parsers {
        match parser(cursor) {
            Ok(r @ Parsed::Some(_)) => return Ok(r),
            Ok(Parsed::Skipped) => continue,
            Ok(Parsed::Nothing) => continue,
            Err(e) => return Err(e),
        }
    }

    Ok(Parsed::Nothing)
}

pub fn parse_paragraph(cursor: &mut TokenCursor) -> ParseResult<BlockNode> {
    if is_heading(cursor) {
        return Ok(Parsed::Nothing);
    }

    let mut builder = NodeBuilder::new();

    let mut body = Vec::new();
    while !is_block_end(cursor, None) && !is_double_newline(cursor) && !cursor.is_at_end() {
        match try_parsers(
            vec![parse_text, parse_simple_inline, parse_inline, parse_newline],
            cursor,
        ) {
            Ok(Parsed::Some(p)) => body.push(p),
            Ok(Parsed::Skipped) => continue,
            Ok(Parsed::Nothing) => continue,
            Err(e) => return Err(e),
        }
    }

    builder.with_node(Block::Paragraph(body));

    Ok(Parsed::Some(builder.build()))
}

pub fn parse_inline(cursor: &mut TokenCursor) -> ParseResult<InlineNode> {
    if cursor.peek_kind() != Some(TokenKind::At) {
        return Ok(Parsed::Nothing);
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
            "code" => InlineKind::Code,
            "link" | "a" => InlineKind::Link,
            ident => InlineKind::Custom(ident.to_string()),
        },
        Some(Token {
            kind: TokenKind::OpenCurly | TokenKind::OpenBrace,
            ..
        }) => InlineKind::Naked,
        Some(e) => {
            return Err(ParsingError::UnexpectedToken(
                e.kind,
                e.lexeme.to_string(),
                "inline",
            ));
        }
        None => return Err(ParsingError::UnexpectedEnd),
    };

    if kind != InlineKind::Naked {
        cursor.advance();
    }

    if cursor.peek_kind() == Some(TokenKind::OpenCurly) {
        builder.with_attributes(parse_attributes(cursor)?);
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenBrace));
    cursor.advance();

    match kind {
        InlineKind::Code => {
            let body = parse_code_inline(cursor);
            builder.with_node(Inline::Code(body));
        }
        InlineKind::Link => {
            if !builder.has_attributes() || !builder.has_attribute_by_kind(AttributeKind::Href) {
                return Err(ParsingError::MissingAttribute("href", "link"));
            }

            let body = parse_inline_body(cursor)?.unwrap();
            builder.with_node(Inline::from_kind(kind, body));
        }
        _ => {
            let body = parse_inline_body(cursor)?.unwrap();
            builder.with_node(Inline::from_kind(kind, body));
        }
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseBrace));
    cursor.advance();

    Ok(Parsed::Some(builder.build()))
}

fn parse_inline_body(cursor: &mut TokenCursor) -> ParseResult<Inlines> {
    let mut body = Vec::new();
    while cursor.peek_kind() != Some(TokenKind::CloseBrace) && !cursor.is_at_end() {
        match parse_inlines(cursor) {
            Ok(Parsed::Some(p)) => body.push(p),
            Ok(Parsed::Skipped) => todo!(),
            Ok(Parsed::Nothing) => todo!(),
            Err(e) => return Err(e),
        }
    }

    Ok(Parsed::Some(body))
}

fn parse_code_inline(cursor: &mut TokenCursor) -> String {
    let mut body = String::new();
    let mut counter = 1;

    while let Some(tok) = cursor.peek() {
        match tok.kind {
            TokenKind::OpenBrace => {
                counter += 1;
            }
            TokenKind::CloseBrace => {
                counter -= 1;
            }
            _ => {}
        }

        if counter == 0 {
            break;
        }

        body.push_str(tok.lexeme);
        cursor.advance();
    }

    body
}

fn is_valid_simple_inline(kind: Option<TokenKind>) -> bool {
    matches!(
        kind,
        Some(
            TokenKind::ForwardSlash
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
        return Ok(Parsed::Nothing);
    }

    cursor.advance();

    let mut node_builder = NodeBuilder::new();

    let (t, kind) = match cursor.peek_kind() {
        Some(t @ TokenKind::ForwardSlash) => (t, InlineKind::Italic),
        Some(t @ TokenKind::Star) => (t, InlineKind::Strong),
        Some(t @ TokenKind::Underscore) => (t, InlineKind::Underline),
        Some(t @ TokenKind::Equals) => (t, InlineKind::Highlight),
        Some(t @ TokenKind::Tilde) => (t, InlineKind::Strikethrough),
        Some(t) => return Err(ParsingError::UnexpectedTokenKind(t, "simple inlines")),
        _ => return Err(ParsingError::UnexpectedEnd),
    };

    cursor.advance();

    let mut body = Vec::new();
    while cursor.peek_kind() != Some(t) && !cursor.is_at_end() {
        match parse_inlines(cursor) {
            Ok(Parsed::Some(p)) => body.push(p),
            Ok(Parsed::Skipped) => todo!(),
            Ok(Parsed::Nothing) => todo!(),
            Err(e) => return Err(e),
        }
    }

    node_builder.with_node(Inline::from_kind(kind, body));

    debug_assert!(cursor.peek_kind() == Some(t));
    cursor.advance();
    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseCurly));
    cursor.advance();

    Ok(Parsed::Some(node_builder.build()))
}

fn parse_attributes(cursor: &mut TokenCursor) -> Result<Attributes, ParsingError> {
    let mut res = Vec::new();

    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenCurly));
    cursor.advance();

    loop {
        skip_whitespace(cursor);
        res.push(parse_attribute(cursor)?);

        if matches!(cursor.peek_kind(), Some(TokenKind::Comma)) {
            cursor.advance();
        } else if cursor.peek_kind() == Some(TokenKind::CloseCurly) {
            cursor.advance();
            break;
        }
    }

    Ok(res)
}

fn parse_attribute(cursor: &mut TokenCursor) -> Result<Attribute, ParsingError> {
    let tok = cursor.advance();
    let (kind, short) = match tok.kind {
        TokenKind::Hashbang => (AttributeKind::Id, true),
        TokenKind::Dot => (AttributeKind::Class, true),
        TokenKind::Text => {
            let res = match tok.lexeme {
                "href" => AttributeKind::Href,
                "class" => AttributeKind::Class,
                "id" => AttributeKind::Id,
                _ => AttributeKind::Attr(tok.lexeme.to_owned()),
            };
            (res, false)
        }
        _ => return Err(ParsingError::InvalidAttributeKind(tok.kind)),
    };

    let value = if short {
        parse_attribute_value(cursor)
    } else {
        match cursor.peek_kind() {
            Some(TokenKind::Comma) | None => AttributeValue::Boolean,
            Some(TokenKind::Equals) => {
                cursor.advance();
                parse_attribute_value(cursor)
            }
            Some(t) => return Err(ParsingError::InvalidAttribute(t)),
        }
    };

    skip_whitespace(cursor);
    Ok(Attribute { kind, value })
}

fn parse_attribute_value(cursor: &mut TokenCursor) -> AttributeValue {
    AttributeValue::String(
        cursor
            .eat_while(|k| {
                !matches!(
                    k.kind,
                    TokenKind::Comma | TokenKind::CloseCurly | TokenKind::Whitespace
                )
            })
            .iter()
            .filter(|k| !matches!(k.kind, TokenKind::DoubleQuote | TokenKind::SingleQoute))
            .map(|k| k.lexeme.trim())
            .collect(),
    )
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
            e => return Err(ParsingError::InvalidHeading(e.to_string())),
        },
        t => {
            return Err(ParsingError::UnexpectedToken(
                t.kind,
                t.lexeme.to_string(),
                "parse_heading",
            ));
        }
    };

    if cursor.peek_kind() == Some(TokenKind::OpenCurly) {
        builder.with_attributes(parse_attributes(cursor)?);
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenBrace));
    cursor.advance();

    let mut body = Vec::new();
    while cursor.peek_kind() != Some(TokenKind::CloseBrace) && !cursor.is_at_end() {
        match parse_inlines(cursor) {
            Ok(Parsed::Some(p)) => body.push(p),
            Ok(Parsed::Skipped) => todo!(),
            Ok(Parsed::Nothing) => todo!(),
            Err(e) => return Err(e),
        }
    }

    debug_assert!(cursor.peek_kind() == Some(TokenKind::CloseBrace));
    cursor.advance();

    builder.with_node(Block::Heading { level, body });

    Ok(Parsed::Some(builder.build()))
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
        let res = parse_inline(&mut cursor).unwrap().unwrap();

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
                        AttributeKind::Class,
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
            let res = parse_inline(&mut cursor).unwrap().unwrap();

            assert!(cursor.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_single_simple_inline() {
        let lexer = tokenize("{*bold *}").collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_simple_inline(&mut cursor).unwrap().unwrap();

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
        let res = parse_simple_inline(&mut cursor).unwrap().unwrap();

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
                        AttributeKind::Class,
                        AttributeValue::String("red".to_string()),
                    )]),
                ),
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_heading(&mut cursor).unwrap().unwrap();

            assert!(cursor.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_code() {
        let inputs = vec![
            (
                "@code[fn hello(world: &str) -> {}]",
                InlineNode::from_node(Inline::Code("fn hello(world: &str) -> {}".to_owned())),
            ),
            (
                "@code{language=css}[p[data-attr] {  }]",
                InlineNode::new(
                    Inline::Code("p[data-attr] {  }".to_owned()),
                    Some(vec![Attribute::new(
                        AttributeKind::Attr("language".to_owned()),
                        AttributeValue::String("css".to_owned()),
                    )]),
                ),
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_inline(&mut cursor).unwrap().unwrap();

            assert!(cursor.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_links() {
        let inputs = vec![
            (
                "@link{href=https://github.com/sondr3}[my GitHub]",
                InlineNode::new(
                    Inline::Link(map_inlines([Inline::Text("my GitHub".to_owned())])),
                    Some(vec![Attribute::new(
                        AttributeKind::Href,
                        AttributeValue::String("https://github.com/sondr3".to_owned()),
                    )]),
                ),
            ),
            (
                "@link{href=/relative/url/}[{/URL/}]",
                InlineNode::new(
                    Inline::Link(map_inlines([Inline::Italic(map_inlines([Inline::Text(
                        "URL".to_owned(),
                    )]))])),
                    Some(vec![Attribute::new(
                        AttributeKind::Href,
                        AttributeValue::String("/relative/url/".to_owned()),
                    )]),
                ),
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_inline(&mut cursor).unwrap().unwrap();

            assert!(cursor.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_link_without_href() {
        let input = "@link[without href]";
        let lexer = tokenize(input).collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        assert!(parse_inline(&mut cursor).is_err());
    }

    #[test]
    fn test_parse_attributes() {
        let attributes = vec![
            (
                "name",
                Attribute::new("name".into(), AttributeValue::Boolean),
            ),
            (
                "name2=value",
                Attribute::new("name2".into(), AttributeValue::String("value".to_string())),
            ),
            (
                "href=/some/other/page.html",
                Attribute::new(
                    AttributeKind::Href,
                    AttributeValue::String("/some/other/page.html".to_string()),
                ),
            ),
            (
                ".class",
                Attribute::new(
                    AttributeKind::Class,
                    AttributeValue::String("class".to_owned()),
                ),
            ),
            (
                "#id",
                Attribute::new(AttributeKind::Id, AttributeValue::String("id".to_owned())),
            ),
        ];
        for (attr, expected) in attributes {
            let lexer = tokenize(attr).collect::<Vec<_>>();
            let mut cursor = TokenCursor::new(lexer);
            let res = parse_attribute(&mut cursor).unwrap();

            assert!(cursor.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_multiple_attributes() {
        let lexer = tokenize(
            r#"{.some-long-class,  #and-long-id  , name1="value",name2, name3=value2 and more}"#,
        )
        .collect::<Vec<_>>();
        let mut cursor = TokenCursor::new(lexer);
        let res = parse_attributes(&mut cursor).unwrap();

        assert!(cursor.is_at_end());
        assert_eq!(
            res,
            vec![
                Attribute::new(
                    AttributeKind::Class,
                    AttributeValue::String("some-long-class".to_string())
                ),
                Attribute::new(
                    AttributeKind::Id,
                    AttributeValue::String("and-long-id".to_string())
                ),
                Attribute::new("name1".into(), AttributeValue::String("value".to_string())),
                Attribute::new("name2".into(), AttributeValue::Boolean),
                Attribute::new(
                    "name3".into(),
                    AttributeValue::String("value2 and more".to_string())
                ),
            ]
        );
    }
}
