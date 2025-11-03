use crate::{ast::Document, lexer::tokenize, parser::parse};

pub mod ast;
mod char_cursor;
pub mod html;
mod lexer;
mod parser;
mod token_cursor;

pub fn parse_kladd(input: &str) -> Document {
    let tokens = tokenize(input).collect::<Vec<_>>();
    parse(tokens)
}

#[cfg(test)]
pub mod test_utils;

#[cfg(test)]
mod tests {
    use crate::{html::to_html, lexer::tokenize, parser::parse, test_utils::TEST_INPUT};

    #[test]
    fn it_works() {
        let tokens = tokenize(TEST_INPUT).collect::<Vec<_>>();
        assert!(!tokens.is_empty());
        insta::assert_debug_snapshot!("tokenizer", tokens);

        let ast = parse(tokens);
        insta::assert_debug_snapshot!("ast", ast);

        let html = to_html(&ast);
        insta::assert_snapshot!("html", html);
    }
}
