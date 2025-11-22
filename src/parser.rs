#[cfg(feature = "serde")]
use serde::de::DeserializeOwned;

use crate::{
    ast::{
        AstAttributes, AstNode, Attribute, AttributeKind, AttributeValue, Attributes, CodeNode,
        Document, HeadingNode, LinkNode, NamedNode, NodeKind, Quote, QuotedNode,
    },
    error::ParsingError,
    lexer::{Token, TokenKind},
    token_cursor::TokenCursor,
};

#[cfg(not(feature = "serde"))]
pub fn parse(input: Vec<Token>) -> Result<(Document, Option<String>), ParsingError> {
    parse_inner(input)
}

#[cfg(feature = "serde")]
pub fn parse<T>(input: Vec<Token>) -> Result<(Document, Option<T>), ParsingError>
where
    T: DeserializeOwned,
{
    let (doc, meta) = parse_inner(input)?;
    let meta = if let Some(inner) = meta {
        match toml::from_str::<T>(&inner) {
            Ok(res) => Some(res),
            Err(e) => return Err(e.into()),
        }
    } else {
        None
    };
    Ok((doc, meta))
}

pub fn parse_inner(input: Vec<Token>) -> Result<(Document, Option<String>), ParsingError> {
    let mut parser = Parser::new(input);
    parser.skip_whitespace();

    let metadata = if parser
        .cursor
        .peek()
        .is_some_and(|t| t.kind == TokenKind::MetadataMarker)
    {
        Some(parse_metadata(&mut parser.cursor))
    } else {
        None
    };

    parser.parse()?;
    Ok((Document { body: parser.nodes }, metadata))
}

pub fn parse_simple(input: Vec<Token>) -> Result<Vec<AstNode>, ParsingError> {
    let mut parser = Parser::new(input);
    parser.parse()?;
    Ok(parser.nodes)
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Parsed {
    /// Something was successfully parsed
    Ok,
    /// Whatever was parsed was skipped
    Skipped,
    /// The parser didn't parse anything
    Nothing,
}

type ParseResult = Result<Parsed, ParsingError>;

#[derive(Debug)]
pub(crate) struct Parser<'a> {
    pub(crate) nodes: Vec<AstNode>,
    pub(crate) cursor: TokenCursor<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: Vec<Token<'a>>) -> Self {
        Parser {
            nodes: Vec::new(),
            cursor: TokenCursor::new(input),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(TokenKind::Newline | TokenKind::Whitespace) = self.cursor.peek_kind() {
            self.cursor.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        self.cursor.is_at_end()
    }

    fn start(&mut self, node: NodeKind) {
        self.nodes.push(AstNode::start(node));
    }

    fn end(&mut self, node: NodeKind) {
        self.nodes.push(AstNode::end(node));
    }

    fn start_with_attrs(&mut self, node: NodeKind, attrs: AstAttributes) {
        self.nodes.push(AstNode::start_attrs(node, attrs));
    }

    pub fn parse(&mut self) -> ParseResult {
        while self.cursor.peek_kind() != Some(TokenKind::Eof) {
            skip_whitespace(&mut self.cursor)?;
            match self.cursor.peek_kind() {
                Some(TokenKind::Eof) => {
                    self.cursor.advance();
                }
                Some(TokenKind::Dash) => {
                    parse_comment(&mut self.cursor)?;
                }
                Some(TokenKind::Bang) => {
                    parse_bang_node(self)?;
                }
                Some(TokenKind::ForwardSlash) => {
                    parse_named_block(self)?;
                }
                None => break,
                _ => {
                    parse_block(self)?;
                }
            }
        }

        Ok(Parsed::Ok)
    }
}

fn skip_whitespace(cursor: &mut TokenCursor) -> ParseResult {
    while let Some(TokenKind::Newline | TokenKind::Whitespace) = cursor.peek_kind() {
        cursor.advance();
    }

    Ok(Parsed::Skipped)
}

fn parse_comment(cursor: &mut TokenCursor) -> ParseResult {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::Dash));
    cursor.advance();

    debug_assert!(cursor.peek_kind() == Some(TokenKind::Dash));
    cursor.advance();

    cursor.eat_while(|t| t.kind != TokenKind::Newline);
    cursor.advance();

    Ok(Parsed::Skipped)
}

fn parse_metadata(cursor: &mut TokenCursor) -> String {
    debug_assert!(cursor.peek_kind() == Some(TokenKind::MetadataMarker));
    cursor.advance();

    let body = cursor.eat_while(|t| t.kind != TokenKind::MetadataMarker);
    debug_assert!(cursor.peek_kind() == Some(TokenKind::MetadataMarker));
    cursor.advance();

    body.iter().map(|t| t.lexeme).collect()
}

fn parse_inline_text(parser: &mut Parser) -> ParseResult {
    let mut body = String::new();
    while let Some(tok) = parser.cursor.peek_kind() {
        match tok {
            TokenKind::Comma
            | TokenKind::Text
            | TokenKind::Whitespace
            | TokenKind::Bang
            | TokenKind::Dot
            | TokenKind::Dash => body.push_str(parser.cursor.advance().lexeme),
            _ => break,
        };
    }

    if body.is_empty() {
        return Ok(Parsed::Nothing);
    }

    parser.start(NodeKind::Text(body));
    Ok(Parsed::Ok)
}

fn parse_quoted(parser: &mut Parser) -> ParseResult {
    let quote = match parser.cursor.peek_kind() {
        Some(t @ (TokenKind::SingleQuote | TokenKind::DoubleQuote)) => t,
        _ => return Ok(Parsed::Nothing),
    };

    let is_start = match parser.cursor.prev() {
        None => true,
        Some(Token {
            kind: TokenKind::Whitespace | TokenKind::Newline,
            ..
        }) => true,
        Some(Token {
            kind: TokenKind::Text,
            lexeme,
        }) => lexeme.ends_with(' '),
        _ => false,
    };

    if !is_start {
        let lexeme = parser.cursor.advance().lexeme;
        parser.start(NodeKind::Text(lexeme.to_owned()));
        return Ok(Parsed::Ok);
    }

    debug_assert!(matches!(
        parser.cursor.peek_kind(),
        Some(TokenKind::DoubleQuote | TokenKind::SingleQuote)
    ));
    parser.cursor.advance();

    let node = QuotedNode {
        quote: Quote::from(quote),
    };
    parser.start(NodeKind::Quoted(node));
    while parser.cursor.peek_kind() != Some(quote) {
        match parse_inlines(parser) {
            Ok(Parsed::Ok) => continue,
            Ok(_) => break,
            e => return e,
        }
    }

    debug_assert!(parser.cursor.peek_kind() == Some(quote));
    parser.cursor.advance();

    parser.end(NodeKind::Quoted(node));
    Ok(Parsed::Ok)
}

fn parse_inlines(parser: &mut Parser) -> ParseResult {
    if let Ok(Parsed::Ok) = parse_inline_text(parser) {
        return Ok(Parsed::Ok);
    };

    match parser.cursor.peek_kind() {
        Some(TokenKind::OpenCurly) => parse_simple_inline(parser),
        Some(TokenKind::At) => parse_inline(parser),
        Some(TokenKind::Newline) => {
            parser.cursor.advance();
            parser.start(NodeKind::Softbreak);
            Ok(Parsed::Ok)
        }
        Some(TokenKind::DoubleQuote | TokenKind::SingleQuote) => parse_quoted(parser),
        Some(t) => Err(ParsingError::UnexpectedTokenKind(t, "inline")),
        None => Err(ParsingError::UnexpectedEnd),
    }
}

pub fn parse_bang_node(parser: &mut Parser) -> ParseResult {
    if is_heading(&parser.cursor) {
        parse_heading(parser)
    } else {
        todo!()
    }
}

pub fn parse_named_block(parser: &mut Parser) -> ParseResult {
    match is_named_block(&parser.cursor) {
        Some("code") => parse_code(parser),
        Some(_) => parse_section(parser),
        None => Ok(Parsed::Nothing),
    }
}

fn is_named_block<'a>(cursor: &'a TokenCursor) -> Option<&'a str> {
    match cursor.peek_kind() {
        Some(TokenKind::ForwardSlash) => match cursor.peek_nth(1) {
            Some(Token {
                kind: TokenKind::Text,
                lexeme,
            }) => Some(lexeme),
            _ => None,
        },
        _ => None,
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

fn maybe_parse_attributes(cursor: &mut TokenCursor) -> Result<AstAttributes, ParsingError> {
    if cursor.peek_kind() == Some(TokenKind::OpenCurly) {
        match parse_attributes(cursor) {
            Ok(attrs) => Ok(AstAttributes::new(attrs)),
            Err(e) => Err(e),
        }
    } else {
        Ok(AstAttributes::empty())
    }
}

fn parse_code(parser: &mut Parser) -> ParseResult {
    if parser.cursor.peek_kind() != Some(TokenKind::ForwardSlash) {
        return Ok(Parsed::Nothing);
    }

    parser.cursor.advance();
    let name = parser.cursor.advance().lexeme;

    let mut attrs = maybe_parse_attributes(&mut parser.cursor)?;
    parser.skip_whitespace();

    let mut body = String::new();

    let mut counter = 1;
    loop {
        if is_named_block(&parser.cursor).is_some_and(|v| v == name) {
            counter += 1;
        } else if is_block_end(&mut parser.cursor, Some(name)) {
            counter -= 1;
        }

        if counter == 0 {
            break;
        }
        body.push_str(parser.cursor.advance().lexeme);

        if parser.cursor.is_at_end() {
            break;
        }
    }

    if !is_block_end(&mut parser.cursor, Some(name)) {
        return Err(ParsingError::MissingBlockEnd(name.to_owned()));
    }

    parser.cursor.advance();
    parser.cursor.advance();

    let language = attrs
        .pop_if(|p| p.kind == AttributeKind::Attr("language".to_owned()))
        .map(|v| v.inner());

    let node = CodeNode { language, body };
    parser.start_with_attrs(NodeKind::CodeBlock(node), attrs);

    Ok(Parsed::Ok)
}

fn parse_section(parser: &mut Parser) -> ParseResult {
    if parser.cursor.peek_kind() != Some(TokenKind::ForwardSlash) {
        return Ok(Parsed::Nothing);
    }

    parser.cursor.advance();
    let name = parser.cursor.advance().lexeme;
    let kind = match name {
        "section" => NodeKind::Section,
        _ => NodeKind::NamedBlock(NamedNode {
            name: name.to_owned(),
        }),
    };

    let attrs = maybe_parse_attributes(&mut parser.cursor)?;
    parser.skip_whitespace();

    parser.start_with_attrs(kind.clone(), attrs);
    while !is_block_end(&mut parser.cursor, Some(name)) && !parser.is_at_end() {
        try_parsers(vec![parse_paragraph, parse_heading], parser)?;
        parser.skip_whitespace();
    }

    if !is_block_end(&mut parser.cursor, Some(name)) {
        return Err(ParsingError::MissingBlockEnd(name.to_owned()));
    }

    parser.cursor.advance();
    parser.cursor.advance();

    parser.end(kind.clone());
    Ok(Parsed::Ok)
}

pub fn parse_block(parser: &mut Parser) -> ParseResult {
    parser.skip_whitespace();

    parser.start(NodeKind::Block);
    while !is_heading(&parser.cursor) && !parser.is_at_end() {
        parse_paragraph(parser)?;
        parser.skip_whitespace();
    }
    parser.end(NodeKind::Block);

    Ok(Parsed::Ok)
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

fn parse_newline(parser: &mut Parser) -> ParseResult {
    match parser.cursor.peek() {
        Some(Token {
            kind: TokenKind::Newline,
            lexeme: "\n",
        }) => {
            parser.cursor.advance();
            parser.start(NodeKind::Softbreak);
            Ok(Parsed::Ok)
        }
        _ => Ok(Parsed::Nothing),
    }
}

fn try_parsers<F>(parsers: Vec<F>, parser: &mut Parser) -> ParseResult
where
    F: Fn(&mut Parser) -> ParseResult,
{
    for pars in parsers {
        match pars(parser) {
            Ok(Parsed::Ok) => return Ok(Parsed::Ok),
            Ok(Parsed::Nothing | Parsed::Skipped) => continue,
            e => return e,
        }
    }

    Ok(Parsed::Nothing)
}

pub fn parse_paragraph(parser: &mut Parser) -> ParseResult {
    if is_heading(&parser.cursor) {
        return Ok(Parsed::Nothing);
    }

    parser.start(NodeKind::Paragraph);
    while !is_block_end(&mut parser.cursor, None)
        && !is_double_newline(&mut parser.cursor)
        && !parser.is_at_end()
    {
        try_parsers(vec![parse_inlines, parse_newline], parser)?;
    }

    parser.end(NodeKind::Paragraph);
    Ok(Parsed::Ok)
}

pub fn parse_inline_code(parser: &mut Parser) -> ParseResult {
    parser.cursor.advance();
    let mut attrs = maybe_parse_attributes(&mut parser.cursor)?;

    let language = attrs
        .pop_if(|p| p.kind == AttributeKind::Attr("language".to_owned()))
        .map(|i| i.inner());

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::OpenBrace));
    parser.cursor.advance();

    let body = parse_code_inline(&mut parser.cursor);

    parser.start_with_attrs(NodeKind::Code(CodeNode { language, body }), attrs);

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::CloseBrace));
    parser.cursor.advance();

    Ok(Parsed::Ok)
}

pub fn parse_inline_link(parser: &mut Parser) -> ParseResult {
    parser.cursor.advance();
    let mut attrs = maybe_parse_attributes(&mut parser.cursor)?;

    let href = match attrs.pop_if(|p| p.kind == AttributeKind::Href) {
        Some(a) => a.inner(),
        None => return Err(ParsingError::MissingAttribute("href", "link")),
    };

    parser.start_with_attrs(NodeKind::Link(LinkNode { href }), attrs);

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::OpenBrace));
    parser.cursor.advance();

    parse_inline_body(parser)?;
    parser.end(NodeKind::Link(LinkNode {
        href: String::new(),
    }));

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::CloseBrace));
    parser.cursor.advance();

    Ok(Parsed::Ok)
}

pub fn parse_inline(parser: &mut Parser) -> ParseResult {
    if parser.cursor.peek_kind() != Some(TokenKind::At) {
        return Ok(Parsed::Nothing);
    }

    parser.cursor.advance();

    let kind = match parser.cursor.peek() {
        Some(Token {
            kind: TokenKind::Text,
            lexeme,
        }) => match *lexeme {
            "bold" | "strong" | "b" => NodeKind::Strong,
            "italic" | "i" => NodeKind::Italic,
            "underline" | "ul" => NodeKind::Underline,
            "highlight" | "hl" => NodeKind::Highlight,
            "strikethrough" | "strike" | "st" => NodeKind::Strikethrough,
            "superscript" | "sup" => NodeKind::Superscript,
            "subscript" | "sub" => NodeKind::Subscript,
            "code" => return parse_inline_code(parser),
            "link" | "a" => return parse_inline_link(parser),
            ident => NodeKind::Custom(NamedNode {
                name: ident.to_string(),
            }),
        },
        Some(Token {
            kind: TokenKind::OpenCurly | TokenKind::OpenBrace,
            ..
        }) => NodeKind::Naked,
        Some(e) => {
            return Err(ParsingError::UnexpectedToken(
                e.kind,
                e.lexeme.to_string(),
                "inline",
            ));
        }
        None => return Err(ParsingError::UnexpectedEnd),
    };

    if kind != NodeKind::Naked {
        parser.cursor.advance();
    }

    let attrs = maybe_parse_attributes(&mut parser.cursor)?;

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::OpenBrace));
    parser.cursor.advance();

    parser.start_with_attrs(kind.clone(), attrs);
    parse_inline_body(parser)?;
    parser.end(kind);

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::CloseBrace));
    parser.cursor.advance();

    Ok(Parsed::Ok)
}

fn parse_inline_body(parser: &mut Parser) -> ParseResult {
    while parser.cursor.peek_kind() != Some(TokenKind::CloseBrace) && !parser.is_at_end() {
        parse_inlines(parser)?;
    }

    Ok(Parsed::Ok)
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
                | TokenKind::SingleQuote
        )
    )
}

pub fn parse_simple_inline(parser: &mut Parser) -> ParseResult {
    if parser.cursor.peek_kind() != Some(TokenKind::OpenCurly)
        && !is_valid_simple_inline(parser.cursor.peek_nth_kind(1))
    {
        return Ok(Parsed::Nothing);
    }

    parser.cursor.advance();

    let start = match parser.cursor.peek_kind() {
        Some(
            t @ (TokenKind::ForwardSlash
            | TokenKind::Star
            | TokenKind::Underscore
            | TokenKind::Equals
            | TokenKind::Tilde
            | TokenKind::SingleQuote),
        ) => t,
        Some(t) => return Err(ParsingError::UnexpectedTokenKind(t, "simple inlines")),
        _ => return Err(ParsingError::UnexpectedEnd),
    };

    parser.cursor.advance();
    parser.start(NodeKind::from_simple_inline(start));

    while (parser.cursor.peek_kind() != Some(start)
        && parser.cursor.peek_nth_kind(1) != Some(TokenKind::CloseCurly))
        && !parser.is_at_end()
    {
        parse_inlines(parser)?;
    }

    debug_assert!(parser.cursor.peek_kind() == Some(start));
    parser.cursor.advance();
    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::CloseCurly));
    parser.cursor.advance();
    parser.end(NodeKind::from_simple_inline(start));

    Ok(Parsed::Ok)
}

fn parse_attributes(cursor: &mut TokenCursor) -> Result<Attributes, ParsingError> {
    let mut res: Attributes = Vec::new();

    debug_assert!(cursor.peek_kind() == Some(TokenKind::OpenCurly));
    cursor.advance();

    loop {
        skip_whitespace(cursor)?;
        let attr = parse_attribute(cursor)?;
        if res.iter().any(|a| a.kind == attr.kind) {
            return Err(ParsingError::DuplicateAttribute(attr.kind));
        } else {
            res.push(attr);
        }

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
            Some(TokenKind::Comma | TokenKind::Eof) | None => AttributeValue::Boolean,
            Some(TokenKind::Equals) => {
                cursor.advance();
                parse_attribute_value(cursor)
            }
            Some(t) => return Err(ParsingError::InvalidAttribute(t)),
        }
    };

    skip_whitespace(cursor)?;
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
            .filter(|k| !matches!(k.kind, TokenKind::DoubleQuote | TokenKind::SingleQuote))
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

fn parse_heading(parser: &mut Parser) -> ParseResult {
    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::Bang));
    parser.cursor.advance();

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::Text));
    let level = match parser.cursor.advance() {
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

    let attrs = maybe_parse_attributes(&mut parser.cursor)?;
    let node = HeadingNode { level };

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::OpenBrace));
    parser.cursor.advance();

    parser.start_with_attrs(NodeKind::Heading(node), attrs);
    while parser.cursor.peek_kind() != Some(TokenKind::CloseBrace) && !parser.is_at_end() {
        parse_inlines(parser)?;
    }
    parser.end(NodeKind::Heading(node));

    debug_assert!(parser.cursor.peek_kind() == Some(TokenKind::CloseBrace));
    parser.cursor.advance();

    Ok(Parsed::Ok)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn test_single_inline() {
        let lexer = tokenize("@bold[body]");
        let mut parser = Parser::new(lexer);
        parse_inline(&mut parser).unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            parser.nodes,
            vec![
                AstNode::start(NodeKind::Strong),
                AstNode::start(NodeKind::Text("body".to_owned())),
                AstNode::end(NodeKind::Strong),
            ]
        );
    }

    #[test]
    fn test_parse_inlines() {
        let inputs = vec![
            (
                "@{class=huge}[Huge]",
                vec![
                    AstNode::start_attrs(
                        NodeKind::Naked,
                        AstAttributes::new(vec![Attribute::new(
                            AttributeKind::Class,
                            AttributeValue::String("huge".to_string()),
                        )]),
                    ),
                    AstNode::start(NodeKind::Text("Huge".to_owned())),
                    AstNode::end(NodeKind::Naked),
                ],
            ),
            (
                "@sup[super]",
                vec![
                    AstNode::start(NodeKind::Superscript),
                    AstNode::start(NodeKind::Text("super".to_owned())),
                    AstNode::end(NodeKind::Superscript),
                ],
            ),
            (
                "@custom[yeet]",
                vec![
                    AstNode::start(NodeKind::Custom(NamedNode {
                        name: "custom".to_owned(),
                    })),
                    AstNode::start(NodeKind::Text("yeet".to_owned())),
                    AstNode::end(NodeKind::Custom(NamedNode {
                        name: "custom".to_owned(),
                    })),
                ],
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr);
            let mut parser = Parser::new(lexer);
            parse_inline(&mut parser).unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.nodes, expected);
        }
    }

    #[test]
    fn test_parse_single_simple_inline() {
        let lexer = tokenize("{*bold *}");
        let mut parser = Parser::new(lexer);
        parse_simple_inline(&mut parser).unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            parser.nodes,
            vec![
                AstNode::start(NodeKind::Strong),
                AstNode::start(NodeKind::Text("bold ".to_owned())),
                AstNode::end(NodeKind::Strong),
            ]
        );
    }

    #[test]
    fn test_parse_nested_simple_inlines() {
        let lexer = tokenize("{*bold {/italic {~struck~}/} {=highlit=}*}");
        let mut parser = Parser::new(lexer);
        parse_simple_inline(&mut parser).unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            parser.nodes,
            vec![
                AstNode::start(NodeKind::Strong),
                AstNode::start(NodeKind::Text("bold ".to_owned())),
                AstNode::start(NodeKind::Italic),
                AstNode::start(NodeKind::Text("italic ".to_owned())),
                AstNode::start(NodeKind::Strikethrough),
                AstNode::start(NodeKind::Text("struck".to_owned())),
                AstNode::end(NodeKind::Strikethrough),
                AstNode::end(NodeKind::Italic),
                AstNode::start(NodeKind::Text(" ".to_owned())),
                AstNode::start(NodeKind::Highlight),
                AstNode::start(NodeKind::Text("highlit".to_owned())),
                AstNode::end(NodeKind::Highlight),
                AstNode::end(NodeKind::Strong),
            ]
        )
    }

    #[test]
    fn test_parse_headlines() {
        let inputs = vec![
            (
                "!title[Hello, world]",
                vec![
                    AstNode::start(NodeKind::Heading(HeadingNode { level: 1 })),
                    AstNode::start(NodeKind::Text("Hello, world".to_owned())),
                    AstNode::end(NodeKind::Heading(HeadingNode { level: 1 })),
                ],
            ),
            (
                "!h2{class=red}[Red title]",
                vec![
                    AstNode::start_attrs(
                        NodeKind::Heading(HeadingNode { level: 2 }),
                        AstAttributes::new(vec![Attribute::new(
                            AttributeKind::Class,
                            AttributeValue::String("red".to_string()),
                        )]),
                    ),
                    AstNode::start(NodeKind::Text("Red title".to_owned())),
                    AstNode::end(NodeKind::Heading(HeadingNode { level: 2 })),
                ],
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr);
            let mut parser = Parser::new(lexer);
            parse_heading(&mut parser).unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.nodes, expected);
        }
    }

    #[test]
    fn test_parse_inline_code() {
        let inputs = vec![
            (
                "@code[fn hello(world: &str) -> {}]",
                vec![AstNode::start(NodeKind::Code(CodeNode {
                    language: None,
                    body: "fn hello(world: &str) -> {}".to_owned(),
                }))],
            ),
            (
                "@code{language=css}[p[data-attr] {  }]",
                vec![AstNode::start(NodeKind::Code(CodeNode {
                    language: Some("css".to_owned()),
                    body: "p[data-attr] {  }".to_owned(),
                }))],
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr);
            let mut parser = Parser::new(lexer);
            parse_inline(&mut parser).unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.nodes, expected);
        }
    }

    #[test]
    fn test_parse_code_block() {
        let input = r#"/code{language=rust}
fn main() {
  println!("Hello, world!");
}
\code"#;

        let lexer = tokenize(input);
        let mut parser = Parser::new(lexer);
        parse_code(&mut parser).unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            parser.nodes,
            vec![AstNode::start(NodeKind::CodeBlock(CodeNode {
                language: Some("rust".to_owned()),
                body: r#"fn main() {
  println!("Hello, world!");
}
"#
                .to_owned(),
            }))]
        );
    }

    #[test]
    fn test_parse_links() {
        let inputs = vec![
            (
                "@link{href=https://github.com/sondr3}[my GitHub]",
                vec![
                    AstNode::start(NodeKind::Link(LinkNode {
                        href: "https://github.com/sondr3".to_owned(),
                    })),
                    AstNode::start(NodeKind::Text("my GitHub".to_owned())),
                    AstNode::end(NodeKind::Link(LinkNode {
                        href: String::new(),
                    })),
                ],
            ),
            (
                "@link{href=/relative/url/}[{/URL/}]",
                vec![
                    AstNode::start(NodeKind::Link(LinkNode {
                        href: "/relative/url/".to_owned(),
                    })),
                    AstNode::start(NodeKind::Italic),
                    AstNode::start(NodeKind::Text("URL".to_owned())),
                    AstNode::end(NodeKind::Italic),
                    AstNode::end(NodeKind::Link(LinkNode {
                        href: String::new(),
                    })),
                ],
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr);
            let mut parser = Parser::new(lexer);
            parse_inline(&mut parser).unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.nodes, expected);
        }
    }

    #[test]
    fn test_parse_link_without_href() {
        let input = "@link[without href]";
        let lexer = tokenize(input);
        let mut parser = Parser::new(lexer);
        assert!(parse_inline(&mut parser).is_err());
    }

    #[test]
    fn test_parse_quoted_text() {
        let inputs = vec![
            (
                r#"isn't quoted"#,
                vec![
                    AstNode::start(NodeKind::Paragraph),
                    AstNode::start(NodeKind::Text("isn".to_string())),
                    AstNode::start(NodeKind::Text("'".to_string())),
                    AstNode::start(NodeKind::Text("t quoted".to_string())),
                    AstNode::end(NodeKind::Paragraph),
                ],
            ),
            (
                r#" "this is double quoted" "#,
                vec![
                    AstNode::start(NodeKind::Paragraph),
                    AstNode::start(NodeKind::Text(" ".to_string())),
                    AstNode::start(NodeKind::Quoted(QuotedNode {
                        quote: Quote::Double,
                    })),
                    AstNode::start(NodeKind::Text("this is double quoted".to_string())),
                    AstNode::end(NodeKind::Quoted(QuotedNode {
                        quote: Quote::Double,
                    })),
                    AstNode::start(NodeKind::Text(" ".to_string())),
                    AstNode::end(NodeKind::Paragraph),
                ],
            ),
            (
                r#" 'this is single quoted' "#,
                vec![
                    AstNode::start(NodeKind::Paragraph),
                    AstNode::start(NodeKind::Text(" ".to_string())),
                    AstNode::start(NodeKind::Quoted(QuotedNode {
                        quote: Quote::Single,
                    })),
                    AstNode::start(NodeKind::Text("this is single quoted".to_string())),
                    AstNode::end(NodeKind::Quoted(QuotedNode {
                        quote: Quote::Single,
                    })),
                    AstNode::start(NodeKind::Text(" ".to_string())),
                    AstNode::end(NodeKind::Paragraph),
                ],
            ),
        ];

        for (attr, expected) in inputs {
            let lexer = tokenize(attr);
            let mut parser = Parser::new(lexer);
            parse_paragraph(&mut parser).unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.nodes, expected);
        }
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
            (
                "class=some spaced class",
                Attribute::new(
                    AttributeKind::Class,
                    AttributeValue::String("some spaced class".to_owned()),
                ),
            ),
            (
                r#"id="quoted id""#,
                Attribute::new(
                    AttributeKind::Id,
                    AttributeValue::String("quoted id".to_owned()),
                ),
            ),
        ];
        for (attr, expected) in attributes {
            let lexer = tokenize(attr);
            let mut parser = Parser::new(lexer);
            let res = parse_attribute(&mut parser.cursor).unwrap();

            assert!(parser.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_multiple_attributes() {
        let lexer = tokenize(
            r#"{.some-long-class,  #and-long-id  , name1="value",name2, name3=value2 and more}"#,
        );
        let mut parser = Parser::new(lexer);
        let res = parse_attributes(&mut parser.cursor).unwrap();

        assert!(parser.is_at_end());
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

    #[test]
    fn test_parse_duplicate_attributes() {
        let lexer = tokenize(r#"{.some-class, class=duplicate}"#);
        let mut parser = Parser::new(lexer);
        assert_eq!(
            parse_attributes(&mut parser.cursor),
            Err(ParsingError::DuplicateAttribute(AttributeKind::Class))
        );
    }
}
