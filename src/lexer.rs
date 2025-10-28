use std::{error::Error, str::Chars};

use crate::cursor::Cursor;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType<'a> {
    MetadataMarker,
    Bang,
    At,
    OpenCurly,
    CloseCurly,
    OpenBrace,
    CloseBrace,
    Equals,
    Newline,
    Percent,
    Text(&'a str),
    EOF,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token<'a> {
    token_type: TokenType<'a>,
    line: usize,
    lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub(crate) fn new(token_type: TokenType<'a>, line: usize, lexeme: &'a str) -> Self {
        Token {
            token_type,
            line,
            lexeme,
        }
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    pub(crate) tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            cursor: Cursor::new(source),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn lex_tokens(&mut self) -> Result<(), Box<dyn Error>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.lex()?
        }

        self.tokens.push(Token::new(TokenType::EOF, 0, ""));
        Ok(())
    }

    fn lex(&mut self) -> Result<(), Box<dyn Error>> {
        let char = self.advance();

        match char {
            Some('!') => self.add_token(TokenType::Bang),
            Some('@') => self.add_token(TokenType::At),
            Some('{') => self.add_token(TokenType::OpenCurly),
            Some('}') => self.add_token(TokenType::CloseCurly),
            Some('[') => self.add_token(TokenType::OpenBrace),
            Some(']') => self.add_token(TokenType::CloseBrace),
            Some('=') => self.add_token(TokenType::Equals),
            Some('\n') => {
                self.line += 1;
                self.add_token(TokenType::Newline)
            }
            Some('%') => self.add_token(TokenType::Percent),
            Some('"') => self.quoted_text(),
            Some(' ') => Ok(()),
            Some('+') => {
                if self.match_char('+') && self.match_char('+') {
                    self.add_token(TokenType::MetadataMarker)
                } else {
                    self.text()
                }
            }
            None => self.add_token(TokenType::EOF),
            _ => self.text(),
        }
    }

    fn quoted_text(&mut self) -> Result<(), Box<dyn Error>> {
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            todo!()
            // return Err(Error::LexerError(
            //     self.line,
            //     "Unterminated string.".to_string(),
            //     "".to_string(),
            // ));
        }

        self.advance();

        let value = &self.cursor.as_str()[self.start + 1..self.current - 1];
        self.add_token(TokenType::Text(value))
    }

    fn text(&mut self) -> Result<(), Box<dyn Error>> {
        while self.peek().is_some_and(|c| c.is_alphanumeric()) {
            self.advance();
        }

        let text = &self.cursor.as_str()[self.start..self.current];
        self.add_token(TokenType::Text(text))
    }

    fn add_token(&mut self, token: TokenType<'a>) -> Result<(), Box<dyn Error>> {
        let text = &self.cursor.as_str()[self.start..self.current];
        self.tokens.push(Token::new(token, self.line, text));

        Ok(())
    }

    fn match_char(&mut self, char: char) -> bool {
        if self.is_at_end() || self.cursor.chars().nth(self.current) != Some(char) {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.cursor.len()
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.cursor.chars().nth(self.current - 1)
    }

    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            self.cursor.chars().nth(self.current)
        }
    }

    fn _peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.cursor.len() {
            None
        } else {
            self.cursor.chars().nth(self.current + 1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &'static str = r#"
+++
metadata: things
+++

!h1{some=value}[With] a body
    "#;

    #[test]
    fn it_works() {
        let mut lexer = Lexer::new(TEST_INPUT);
        assert!(lexer.lex_tokens().is_ok());
        assert!(!lexer.tokens.is_empty());
    }
}
