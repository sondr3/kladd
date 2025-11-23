use std::collections::BTreeMap;

use serde::de::DeserializeOwned;

use crate::{
    ast::{
        AstAttributes, AstNode, AttributeKind, AttributeValue, Attributes, CodeNode, Document,
        HeadingNode, LinkNode, NamedNode, NodeKind, Quote, QuotedNode,
    },
    error::ParsingError,
    lexer::{Token, TokenKind, tokenize},
    token_cursor::TokenCursor,
};

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
        Some(TokenKind::BackwardSlash) => {
            matches!((cursor.peek_nth(1), cursor.peek_nth_kind(2)), (
                Some(Token {
                    kind: TokenKind::Text,
                    lexeme,
                }),
                Some(TokenKind::Newline | TokenKind::Eof),
            ) if is_lexeme.is_none_or(|v| v == *lexeme))
        }
        _ => false,
    }
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

#[derive(Debug, Clone, Copy)]
pub enum Parsed {
    /// Something was successfully parsed
    Ok,
    /// Whatever was parsed was skipped
    Skipped,
    /// The parser didn't parse anything
    Nothing,
}

type ParseResult = Result<Parsed, ParsingError>;

#[derive(Debug)]
pub struct Parser<'a, T: DeserializeOwned> {
    pub(crate) document: Document,
    pub(crate) metadata: Option<T>,
    pub(crate) cursor: TokenCursor<'a>,
}

impl<'a, T: DeserializeOwned> Parser<'a, T> {
    pub fn new(input: &'a str) -> Result<Self, ParsingError> {
        let mut parser = Parser {
            document: Document::new(),
            metadata: None,
            cursor: TokenCursor::new(tokenize(input)),
        };

        parser.parse()?;
        Ok(parser)
    }

    pub fn finish(self) -> (Document, Option<T>) {
        (self.document, self.metadata)
    }

    #[cfg(test)]
    pub(crate) fn test(input: &'a str) -> Self {
        Parser {
            document: Document::new(),
            metadata: None,
            cursor: TokenCursor::new(tokenize(input)),
        }
    }

    fn parse(&mut self) -> ParseResult {
        self.skip_whitespace();

        self.metadata = if self
            .cursor
            .peek()
            .is_some_and(|t| t.kind == TokenKind::MetadataMarker)
        {
            Some(self.parse_metadata()?)
        } else {
            None
        };

        self.consume()?;
        Ok(Parsed::Ok)
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
        self.document.body.push(AstNode::start(node));
    }

    fn end(&mut self, node: NodeKind) {
        self.document.body.push(AstNode::end(node));
    }

    fn start_with_attrs(&mut self, node: NodeKind, attrs: AstAttributes) {
        self.document.body.push(AstNode::start_attrs(node, attrs));
    }

    fn consume(&mut self) -> ParseResult {
        while self.cursor.peek_kind() != Some(TokenKind::Eof) {
            self.skip_whitespace();
            match self.cursor.peek_kind() {
                Some(TokenKind::Eof) => {
                    self.cursor.advance();
                }
                Some(TokenKind::Dash) => {
                    self.parse_comment()?;
                }
                Some(TokenKind::Bang) => {
                    self.parse_bang_node()?;
                }
                Some(TokenKind::ForwardSlash) => {
                    self.parse_named_block()?;
                }
                None => break,
                _ => {
                    self.parse_block()?;
                }
            }
        }

        Ok(Parsed::Ok)
    }

    fn parse_comment(&mut self) -> ParseResult {
        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::Dash));
        self.cursor.advance();

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::Dash));
        self.cursor.advance();

        self.cursor.eat_while(|t| t.kind != TokenKind::Newline);
        self.cursor.advance();

        Ok(Parsed::Skipped)
    }

    fn parse_metadata(&mut self) -> Result<T, ParsingError>
    where
        T: DeserializeOwned,
    {
        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::MetadataMarker));
        self.cursor.advance();

        let body = self
            .cursor
            .eat_while(|t| t.kind != TokenKind::MetadataMarker);
        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::MetadataMarker));
        self.cursor.advance();

        let text: String = body.iter().map(|t| t.lexeme).collect();
        Ok(toml::from_str::<T>(&text)?)
    }

    pub(crate) fn parse_inline_text(&mut self) -> ParseResult {
        let mut body = String::new();
        while let Some(tok) = self.cursor.peek_kind() {
            match tok {
                TokenKind::Comma
                | TokenKind::Text
                | TokenKind::Whitespace
                | TokenKind::Bang
                | TokenKind::Dot
                | TokenKind::Dash => body.push_str(self.cursor.advance().lexeme),
                _ => break,
            };
        }

        if body.is_empty() {
            return Ok(Parsed::Nothing);
        }

        self.start(NodeKind::Text(body));
        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_quoted(&mut self) -> ParseResult {
        let quote = match self.cursor.peek_kind() {
            Some(t @ (TokenKind::SingleQuote | TokenKind::DoubleQuote)) => t,
            _ => return Ok(Parsed::Nothing),
        };

        let is_start = match self.cursor.prev() {
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
            let lexeme = self.cursor.advance().lexeme;
            self.start(NodeKind::Text(lexeme.to_owned()));
            return Ok(Parsed::Ok);
        }

        debug_assert!(matches!(
            self.cursor.peek_kind(),
            Some(TokenKind::DoubleQuote | TokenKind::SingleQuote)
        ));
        self.cursor.advance();

        let node = QuotedNode {
            quote: Quote::from(quote),
        };
        self.start(NodeKind::Quoted(node));
        while self.cursor.peek_kind() != Some(quote) {
            match self.parse_inlines() {
                Ok(Parsed::Ok) => continue,
                Ok(_) => break,
                e => return e,
            }
        }

        debug_assert!(self.cursor.peek_kind() == Some(quote));
        self.cursor.advance();

        self.end(NodeKind::Quoted(node));
        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_inlines(&mut self) -> ParseResult {
        if let Ok(Parsed::Ok) = self.parse_inline_text() {
            return Ok(Parsed::Ok);
        };

        match self.cursor.peek_kind() {
            Some(TokenKind::OpenCurly) => self.parse_simple_inline(),
            Some(TokenKind::At) => self.parse_inline(),
            Some(TokenKind::Newline) => {
                self.cursor.advance();
                self.start(NodeKind::Softbreak);
                Ok(Parsed::Ok)
            }
            Some(TokenKind::DoubleQuote | TokenKind::SingleQuote) => self.parse_quoted(),
            Some(t) => Err(ParsingError::UnexpectedTokenKind(t, "inline")),
            None => Err(ParsingError::UnexpectedEnd),
        }
    }

    pub(crate) fn parse_bang_node(&mut self) -> ParseResult {
        if is_heading(&self.cursor) {
            self.parse_heading()
        } else {
            todo!()
        }
    }

    pub(crate) fn parse_named_block(&mut self) -> ParseResult {
        match is_named_block(&self.cursor) {
            Some("code") => self.parse_code(),
            Some(_) => self.parse_section(),
            None => Ok(Parsed::Nothing),
        }
    }

    pub(crate) fn maybe_parse_attributes(&mut self) -> Result<AstAttributes, ParsingError> {
        if self.cursor.peek_kind() == Some(TokenKind::OpenCurly) {
            self.parse_attributes()
        } else {
            Ok(AstAttributes::empty())
        }
    }

    pub(crate) fn parse_code(&mut self) -> ParseResult {
        if self.cursor.peek_kind() != Some(TokenKind::ForwardSlash) {
            return Ok(Parsed::Nothing);
        }

        self.cursor.advance();
        let name = self.cursor.advance().lexeme;

        let mut attrs = self.maybe_parse_attributes()?;
        self.skip_whitespace();

        let mut body = String::new();

        let mut counter = 1;
        loop {
            if is_named_block(&self.cursor).is_some_and(|v| v == name) {
                counter += 1;
            } else if is_block_end(&mut self.cursor, Some(name)) {
                counter -= 1;
            }

            if counter == 0 {
                break;
            }
            body.push_str(self.cursor.advance().lexeme);

            if self.cursor.is_at_end() {
                break;
            }
        }

        if !is_block_end(&mut self.cursor, Some(name)) {
            return Err(ParsingError::MissingBlockEnd(name.to_owned()));
        }

        self.cursor.advance();
        self.cursor.advance();

        let language = attrs
            .remove(AttributeKind::Attr("language".to_owned()))
            .map(|v| v.inner());

        let node = CodeNode { language, body };
        self.start_with_attrs(NodeKind::CodeBlock(node), attrs);

        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_section(&mut self) -> ParseResult {
        if self.cursor.peek_kind() != Some(TokenKind::ForwardSlash) {
            return Ok(Parsed::Nothing);
        }

        self.cursor.advance();
        let name = self.cursor.advance().lexeme;
        let kind = match name {
            "section" => NodeKind::Section,
            _ => NodeKind::NamedBlock(NamedNode {
                name: name.to_owned(),
            }),
        };

        let attrs = self.maybe_parse_attributes()?;
        self.skip_whitespace();

        self.start_with_attrs(kind.clone(), attrs);
        while !is_block_end(&mut self.cursor, Some(name)) && !self.is_at_end() {
            self.try_parsers(&[Self::parse_paragraph, Self::parse_heading])?;
            self.skip_whitespace();
        }

        if !is_block_end(&mut self.cursor, Some(name)) {
            return Err(ParsingError::MissingBlockEnd(name.to_owned()));
        }

        self.cursor.advance();
        self.cursor.advance();

        self.end(kind.clone());
        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_block(&mut self) -> ParseResult {
        self.skip_whitespace();

        self.start(NodeKind::Block);
        while !is_heading(&self.cursor) && !self.is_at_end() {
            self.parse_paragraph()?;
            self.skip_whitespace();
        }
        self.end(NodeKind::Block);

        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_newline(&mut self) -> ParseResult {
        match self.cursor.peek() {
            Some(Token {
                kind: TokenKind::Newline,
                lexeme: "\n",
            }) => {
                self.cursor.advance();
                self.start(NodeKind::Softbreak);
                Ok(Parsed::Ok)
            }
            _ => Ok(Parsed::Nothing),
        }
    }

    fn try_parsers(&mut self, parsers: &[fn(&mut Self) -> ParseResult]) -> ParseResult {
        for pars in parsers {
            match pars(self) {
                Ok(Parsed::Ok) => return Ok(Parsed::Ok),
                Ok(Parsed::Nothing | Parsed::Skipped) => continue,
                e => return e,
            }
        }

        Ok(Parsed::Nothing)
    }

    pub(crate) fn parse_paragraph(&mut self) -> ParseResult {
        if is_heading(&self.cursor) {
            return Ok(Parsed::Nothing);
        }

        self.start(NodeKind::Paragraph);
        while !is_block_end(&mut self.cursor, None)
            && !is_double_newline(&mut self.cursor)
            && !self.is_at_end()
        {
            self.try_parsers(&[Self::parse_inlines, Self::parse_newline])?;
        }

        self.end(NodeKind::Paragraph);
        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_inline_code(&mut self) -> ParseResult {
        self.cursor.advance();
        let mut attrs = self.maybe_parse_attributes()?;

        let language = attrs
            .remove(AttributeKind::Attr("language".to_owned()))
            .map(|i| i.inner());

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::OpenBrace));
        self.cursor.advance();

        let body = self.parse_code_inline();

        self.start_with_attrs(NodeKind::Code(CodeNode { language, body }), attrs);

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::CloseBrace));
        self.cursor.advance();

        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_inline_link(&mut self) -> ParseResult {
        self.cursor.advance();
        let mut attrs = self.maybe_parse_attributes()?;

        let href = match attrs.remove(AttributeKind::Href) {
            Some(a) => a.inner(),
            None => return Err(ParsingError::MissingAttribute("href", "link")),
        };

        self.start_with_attrs(NodeKind::Link(LinkNode { href }), attrs);

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::OpenBrace));
        self.cursor.advance();

        self.parse_inline_body()?;
        self.end(NodeKind::Link(LinkNode {
            href: String::new(),
        }));

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::CloseBrace));
        self.cursor.advance();

        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_inline(&mut self) -> ParseResult {
        if self.cursor.peek_kind() != Some(TokenKind::At) {
            return Ok(Parsed::Nothing);
        }

        self.cursor.advance();

        let kind = match self.cursor.peek() {
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
                "code" => return self.parse_inline_code(),
                "link" | "a" => return self.parse_inline_link(),
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
            self.cursor.advance();
        }

        let attrs = self.maybe_parse_attributes()?;

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::OpenBrace));
        self.cursor.advance();

        self.start_with_attrs(kind.clone(), attrs);
        self.parse_inline_body()?;
        self.end(kind);

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::CloseBrace));
        self.cursor.advance();

        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_inline_body(&mut self) -> ParseResult {
        while self.cursor.peek_kind() != Some(TokenKind::CloseBrace) && !self.is_at_end() {
            self.parse_inlines()?;
        }

        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_code_inline(&mut self) -> String {
        let mut body = String::new();
        let mut counter = 1;

        while let Some(tok) = self.cursor.peek() {
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
            self.cursor.advance();
        }

        body
    }

    pub(crate) fn parse_simple_inline(&mut self) -> ParseResult {
        if self.cursor.peek_kind() != Some(TokenKind::OpenCurly)
            && !is_valid_simple_inline(self.cursor.peek_nth_kind(1))
        {
            return Ok(Parsed::Nothing);
        }

        self.cursor.advance();

        let start = match self.cursor.peek_kind() {
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

        self.cursor.advance();
        self.start(NodeKind::from_simple_inline(start));

        while (self.cursor.peek_kind() != Some(start)
            && self.cursor.peek_nth_kind(1) != Some(TokenKind::CloseCurly))
            && !self.is_at_end()
        {
            self.parse_inlines()?;
        }

        debug_assert!(self.cursor.peek_kind() == Some(start));
        self.cursor.advance();
        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::CloseCurly));
        self.cursor.advance();
        self.end(NodeKind::from_simple_inline(start));

        Ok(Parsed::Ok)
    }

    pub(crate) fn parse_attributes(&mut self) -> Result<AstAttributes, ParsingError> {
        let mut attrs: Attributes = BTreeMap::new();

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::OpenCurly));
        self.cursor.advance();

        loop {
            self.skip_whitespace();
            let (kind, value) = self.parse_attribute()?;
            attrs.insert(kind, value);
            if matches!(self.cursor.peek_kind(), Some(TokenKind::Comma)) {
                self.cursor.advance();
            } else if self.cursor.peek_kind() == Some(TokenKind::CloseCurly) {
                self.cursor.advance();
                break;
            }
        }

        Ok(AstAttributes::new(attrs))
    }

    pub(crate) fn parse_attribute(
        &mut self,
    ) -> Result<(AttributeKind, AttributeValue), ParsingError> {
        let tok = self.cursor.advance();
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
            self.parse_attribute_value()
        } else {
            match self.cursor.peek_kind() {
                Some(TokenKind::Comma | TokenKind::Eof) | None => AttributeValue::Boolean,
                Some(TokenKind::Equals) => {
                    self.cursor.advance();
                    self.parse_attribute_value()
                }
                Some(t) => return Err(ParsingError::InvalidAttribute(t)),
            }
        };

        self.skip_whitespace();
        Ok((kind, value))
    }

    pub(crate) fn parse_attribute_value(&mut self) -> AttributeValue {
        AttributeValue::String(
            self.cursor
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

    pub(crate) fn parse_heading(&mut self) -> ParseResult {
        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::Bang));
        self.cursor.advance();

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::Text));
        let level = match self.cursor.advance() {
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

        let attrs = self.maybe_parse_attributes()?;
        let node = HeadingNode { level };

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::OpenBrace));
        self.cursor.advance();

        self.start_with_attrs(NodeKind::Heading(node), attrs);
        while self.cursor.peek_kind() != Some(TokenKind::CloseBrace) && !self.is_at_end() {
            self.parse_inlines()?;
        }
        self.end(NodeKind::Heading(node));

        debug_assert!(self.cursor.peek_kind() == Some(TokenKind::CloseBrace));
        self.cursor.advance();

        Ok(Parsed::Ok)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    fn lex_and_parse<'a>(input: &'a str) -> Parser<'a, ()> {
        Parser::<()>::test(input)
    }

    #[test]
    fn test_single_inline() {
        let mut parser = lex_and_parse("@bold[body]");
        parser.parse_inline().unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            parser.document.body,
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
                        AstAttributes::from([(
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
            let mut parser = lex_and_parse(attr);
            parser.parse_inline().unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.document.body, expected);
        }
    }

    #[test]
    fn test_parse_single_simple_inline() {
        let mut parser = lex_and_parse("{*bold *}");
        parser.parse_simple_inline().unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            parser.document.body,
            vec![
                AstNode::start(NodeKind::Strong),
                AstNode::start(NodeKind::Text("bold ".to_owned())),
                AstNode::end(NodeKind::Strong),
            ]
        );
    }

    #[test]
    fn test_parse_nested_simple_inlines() {
        let mut parser = lex_and_parse("{*bold {/italic {~struck~}/} {=highlit=}*}");
        parser.parse_simple_inline().unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            parser.document.body,
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
                        AstAttributes::from([(
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
            let mut parser = lex_and_parse(attr);
            parser.parse_heading().unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.document.body, expected);
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
            let mut parser = lex_and_parse(attr);
            parser.parse_inline().unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.document.body, expected);
        }
    }

    #[test]
    fn test_parse_code_block() {
        let input = r#"/code{language=rust}
fn main() {
  println!("Hello, world!");
}
\code"#;

        let mut parser = lex_and_parse(input);
        parser.parse_code().unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            parser.document.body,
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
            let mut parser = lex_and_parse(attr);
            parser.parse_inline().unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.document.body, expected);
        }
    }

    #[test]
    fn test_parse_link_without_href() {
        let mut parser = lex_and_parse("@link[without href]");
        assert!(parser.parse_inline().is_err());
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
            let mut parser = lex_and_parse(attr);
            parser.parse_paragraph().unwrap();

            assert!(parser.is_at_end());
            assert_eq!(parser.document.body, expected);
        }
    }

    #[test]
    fn test_parse_attributes() {
        let attributes = vec![
            ("name", ("name".into(), AttributeValue::Boolean)),
            (
                "name2=value",
                ("name2".into(), AttributeValue::String("value".to_string())),
            ),
            (
                "href=/some/other/page.html",
                (
                    AttributeKind::Href,
                    AttributeValue::String("/some/other/page.html".to_string()),
                ),
            ),
            (
                ".class",
                (
                    AttributeKind::Class,
                    AttributeValue::String("class".to_owned()),
                ),
            ),
            (
                "#id",
                (AttributeKind::Id, AttributeValue::String("id".to_owned())),
            ),
            (
                "class=some spaced class",
                (
                    AttributeKind::Class,
                    AttributeValue::String("some spaced class".to_owned()),
                ),
            ),
            (
                r#"id="quoted id""#,
                (
                    AttributeKind::Id,
                    AttributeValue::String("quoted id".to_owned()),
                ),
            ),
        ];
        for (attr, expected) in attributes {
            let mut parser = lex_and_parse(attr);
            let res = parser.parse_attribute().unwrap();

            assert!(parser.is_at_end());
            assert_eq!(res, expected);
        }
    }

    #[test]
    fn test_parse_multiple_attributes() {
        let mut parser = lex_and_parse(
            r#"{.some-long-class,  #and-long-id  , name1="value",name2, name3=value2 and more}"#,
        );
        let res = parser.parse_attributes().unwrap();

        assert!(parser.is_at_end());
        assert_eq!(
            res,
            AstAttributes::from([
                (
                    AttributeKind::Class,
                    AttributeValue::String("some-long-class".to_string())
                ),
                (
                    AttributeKind::Id,
                    AttributeValue::String("and-long-id".to_string())
                ),
                ("name1".into(), AttributeValue::String("value".to_string())),
                ("name2".into(), AttributeValue::Boolean),
                (
                    "name3".into(),
                    AttributeValue::String("value2 and more".to_string())
                ),
            ])
        );
    }

    #[test]
    fn test_parse_duplicate_attributes() {
        let mut parser = lex_and_parse(r#"{.some-class, class=duplicate}"#);
        assert_eq!(
            parser.parse_attributes().unwrap(),
            AstAttributes::from([(
                AttributeKind::Class,
                AttributeValue::String("duplicate".to_owned())
            )])
        );
    }
}
