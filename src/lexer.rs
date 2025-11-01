use crate::char_cursor::CharCursor;

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
    Slash,
    Star,
    Underscore,
    Dash,
    Percent,
    Comma,
    Text,
    Newline,
    Whitespace,
    EOF,
    Unknown,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str) -> Self {
        Token { kind, lexeme }
    }

    pub fn dummy() -> Self {
        Token::new(TokenKind::Unknown, "")
    }
}

pub fn tokenize<'a>(input: &'a str) -> impl Iterator<Item = Token<'a>> {
    let mut cursor = CharCursor::new(input);
    std::iter::from_fn(move || {
        cursor.start = cursor.curr;
        match cursor.advance_token() {
            Token {
                kind: TokenKind::EOF,
                ..
            } => None,
            tok => Some(tok),
        }
    })
}

pub fn is_horizontal_whitespace(c: Option<char>) -> bool {
    matches!(c, Some('\t' | ' '))
}

fn is_newline(c: Option<char>) -> bool {
    c.is_some_and(|c| c == '\n')
}

impl<'a> CharCursor<'a> {
    pub fn advance_token(&mut self) -> Token<'a> {
        let Some(first_char) = self.advance() else {
            return Token::new(TokenKind::EOF, "");
        };

        let token = match first_char {
            '\n' => self.newline(),
            c if is_horizontal_whitespace(Some(c)) => self.whitespace(),
            '+' if self.as_str().starts_with("++") => self.metadata(),
            '!' => TokenKind::Bang,
            ',' => TokenKind::Comma,
            '@' => TokenKind::At,
            '{' => TokenKind::OpenCurly,
            '}' => TokenKind::CloseCurly,
            '[' => TokenKind::OpenBrace,
            ']' => TokenKind::CloseBrace,
            '=' => TokenKind::Equals,
            '/' => TokenKind::Slash,
            '*' => TokenKind::Star,
            '_' => TokenKind::Underscore,
            '-' => TokenKind::Dash,
            '%' => TokenKind::Percent,
            '"' => self.quoted_text(),
            _ => self.text(),
        };

        Token::new(token, self.lexeme())
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

#[cfg(test)]
mod tests {}
