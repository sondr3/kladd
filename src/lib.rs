pub mod ast;
pub mod char_cursor;
pub mod html;
pub mod lexer;
pub mod parser;
pub mod token_cursor;

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

        let html = to_html(ast);
        insta::assert_snapshot!("html", html);
    }
}
