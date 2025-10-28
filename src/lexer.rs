use crate::cursor::Cursor;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    MetadataMarker,
    Bang,
    At,
    OpenCurly,
    CloseCurly,
    OpenBrace,
    CloseBrace,
    Equals,
    Percent,
    Text,
    Newline,
    Whitespace,
    EOF,
    Unknown,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    len: usize,
}

impl Token {
    pub(crate) fn new(kind: TokenKind, len: usize) -> Self {
        Token { kind, len }
    }
}

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || match cursor.advance_token() {
        Token {
            kind: TokenKind::EOF,
            ..
        } => None,
        tok => Some(tok),
    })
}

fn _is_whitespace(c: Option<char>) -> bool {
    c.is_some_and(char::is_whitespace)
}

pub fn is_horizontal_whitespace(c: Option<char>) -> bool {
    matches!(c, Some('\t' | ' '))
}

fn is_newline(c: Option<char>) -> bool {
    c.is_some_and(|c| c == '\n')
}

impl Cursor<'_> {
    pub fn advance_token(&mut self) -> Token {
        let Some(first_char) = self.advance() else {
            return Token::new(TokenKind::EOF, 0);
        };

        let token = match first_char {
            '\n' => self.newline(),
            c if is_horizontal_whitespace(Some(c)) => self.whitespace(),
            '+' if self.as_str().starts_with("++") => self.metadata(),
            '!' => TokenKind::Bang,
            '@' => TokenKind::At,
            '{' => TokenKind::OpenCurly,
            '}' => TokenKind::CloseCurly,
            '[' => TokenKind::OpenBrace,
            ']' => TokenKind::CloseBrace,
            '=' => TokenKind::Equals,
            '%' => TokenKind::Percent,
            '"' => self.quoted_text(),
            _ => self.text(),
        };

        let res = Token::new(token, self.token_pos());
        self.reset_token_pos();
        res
    }

    fn whitespace(&mut self) -> TokenKind {
        self.eat_while(is_horizontal_whitespace);
        TokenKind::Whitespace
    }

    fn newline(&mut self) -> TokenKind {
        self.eat_while(is_newline);
        TokenKind::Newline
    }

    fn metadata(&mut self) -> TokenKind {
        debug_assert!(self.prev == Some('+'));
        self.eat_while(|c| c.is_some_and(|i| i == '+'));
        TokenKind::MetadataMarker
    }

    fn quoted_text(&mut self) -> TokenKind {
        debug_assert!(self.prev == Some('"'));
        while let Some(c) = self.advance() {
            match c {
                '"' => return TokenKind::Text,
                '\\' if self.peek() == Some('\\') || self.peek() == Some('"') => {
                    self.advance();
                }
                _ => (),
            }
        }

        TokenKind::Unknown
    }

    fn text(&mut self) -> TokenKind {
        while self.peek().is_some_and(char::is_alphanumeric) {
            self.advance();
        }

        TokenKind::Text
    }
}

#[cfg(debug_assertions)]
mod debug_utils {}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &'static str = r#"
+++
metadata = things
+++

!h1{some=value}[With] a body
 "#;

    #[test]
    fn it_works() {
        let tokens = tokenize(TEST_INPUT).collect::<Vec<_>>();
        assert!(!tokens.is_empty());
        insta::assert_debug_snapshot!(tokens);
    }
}
