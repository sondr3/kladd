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
    Tilde,
    Percent,
    Comma,
    DoubleQuote,
    SingleQoute,
    Text,
    Newline,
    Whitespace,
    EOF,
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
            '~' => TokenKind::Tilde,
            '%' => TokenKind::Percent,
            '"' => TokenKind::DoubleQuote,
            '\'' => TokenKind::SingleQoute,
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

    fn text(&mut self) -> TokenKind {
        while self
            .peek()
            .is_some_and(|c| c.is_alphanumeric() || is_horizontal_whitespace(Some(c)))
        {
            self.advance();
        }

        TokenKind::Text
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        char_cursor::CharCursor,
        lexer::{Token, TokenKind},
    };

    #[test]
    fn lex_quoted_text() {
        let cases = vec![(
            "this is some text",
            Token::new(TokenKind::Text, "this is some text"),
        )];

        for (input, expected) in cases {
            let res = CharCursor::new(input).advance_token();
            assert_eq!(expected, res);
        }
    }
}
