use crate::{ast::Document, error::KladdError, lexer::tokenize, parser::parse};

pub mod ast;
pub mod ast_visualizer;
mod char_cursor;
pub mod error;
pub mod html;
mod lexer;
mod parser;
mod token_cursor;

pub fn parse_kladd(input: String) -> Result<Document, KladdError> {
    let tokens = tokenize(&input);
    Ok(parse(tokens)?)
}

#[cfg(test)]
pub mod test_utils;

#[cfg(test)]
mod tests {
    use crate::{
        ast_visualizer::visualize, html::to_html, lexer::tokenize, parser::parse,
        test_utils::TEST_INPUT,
    };

    #[test]
    fn it_works() {
        let tokens = tokenize(TEST_INPUT);
        assert!(!tokens.is_empty());
        insta::assert_debug_snapshot!("tokenizer", tokens);

        let ast = parse(tokens).unwrap();
        insta::assert_debug_snapshot!("ast", ast);

        let vizualised = visualize(&ast);
        insta::assert_snapshot!("visualize", vizualised);

        let html = to_html(&ast);
        insta::assert_snapshot!("html", html);
    }
}
