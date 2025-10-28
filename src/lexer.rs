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
    Percent,
    Text,
    Newline,
    Whitespace,
    EOF,
    Unknown,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token {
    pub(crate) kind: TokenKind,
    len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Self {
        Token { kind, len }
    }

    pub fn dummy() -> Self {
        Token::new(TokenKind::Unknown, 0)
    }
}

pub fn tokenize(input: &str) -> impl Iterator<Item = Token> {
    let mut cursor = CharCursor::new(input);
    std::iter::from_fn(move || match cursor.advance_token() {
        Token {
            kind: TokenKind::EOF,
            ..
        } => None,
        tok => Some(tok),
    })
}

#[cfg(debug_assertions)]
pub fn visualize(input: &str) {
    use crate::lexer::debug_utils::LexerVisualizer;

    let mut visualizer = LexerVisualizer::new(input);
    println!("{}", visualizer.visualize());
}

pub fn is_horizontal_whitespace(c: Option<char>) -> bool {
    matches!(c, Some('\t' | ' '))
}

fn is_newline(c: Option<char>) -> bool {
    c.is_some_and(|c| c == '\n')
}

impl CharCursor<'_> {
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

#[cfg(any(test, debug_assertions))]
mod debug_utils {
    use super::*;

    fn tokenkind_to_str(kind: TokenKind) -> &'static str {
        match kind {
            TokenKind::MetadataMarker => "METAMARKER",
            TokenKind::Bang => "BANG",
            TokenKind::At => "AT",
            TokenKind::OpenCurly => "OPENCURLY",
            TokenKind::CloseCurly => "CLOSECURLY",
            TokenKind::OpenBrace => "OPENBRACE",
            TokenKind::CloseBrace => "CLOSEBRACE",
            TokenKind::Equals => "EQUALS",
            TokenKind::Percent => "PERCENT",
            TokenKind::Text => "TEXT",
            TokenKind::Newline => "",
            TokenKind::Whitespace => "",
            TokenKind::EOF => "EOF",
            TokenKind::Unknown => "UNKNOWN",
        }
    }

    fn print_token<'a>(token: &'a Token, tester: &'a LexerVisualizer) -> String {
        format!(
            "{} {}..{} \"{}\" ",
            tokenkind_to_str(token.kind),
            tester.pos,
            tester.pos + token.len,
            &tester.input[tester.pos..tester.pos + token.len]
        )
    }

    pub struct LexerVisualizer<'a> {
        input: &'a str,
        tokens: Vec<Token>,
        pos: usize,
    }

    impl<'a> LexerVisualizer<'a> {
        pub fn new(input: &'a str) -> Self {
            let tokens = tokenize(input).collect();

            Self {
                input,
                tokens,
                pos: 0,
            }
        }

        pub fn visualize(&mut self) -> String {
            let mut buf = String::new();

            for tok in &self.tokens {
                match tok.kind {
                    TokenKind::Newline => buf.push('\n'),
                    TokenKind::Whitespace => buf.push(' '),
                    _ => buf.push_str(&print_token(tok, self)),
                }
                self.pos += tok.len;
            }

            buf
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &'static str = r#"
+++
metadata = things
+++

!h1{some=value}[Header] 

With a @bold[body]
 "#;

    #[test]
    fn it_works() {
        let tokens = tokenize(TEST_INPUT).collect::<Vec<_>>();
        assert!(!tokens.is_empty());
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn it_looks_correct() {
        let mut tester = debug_utils::LexerVisualizer::new(TEST_INPUT);
        let res = tester.visualize();
        insta::assert_snapshot!(res);
    }
}
