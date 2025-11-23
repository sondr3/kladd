use serde::de::DeserializeOwned;

use crate::{
    ast::{AstNode, Document},
    error::KladdError,
    lexer::tokenize,
    parser::{parse, parse_simple},
};

pub mod ast;
pub mod ast_visualizer;
mod char_cursor;
pub mod error;
pub mod html;
mod lexer;
mod parser;
mod token_cursor;

pub fn parse_kladd_document<T>(input: &str) -> Result<(Document, Option<T>), KladdError>
where
    T: DeserializeOwned,
{
    let tokens = tokenize(input);
    let res = parse(tokens)?;
    Ok(res)
}

pub fn parse_kladd(input: &str) -> Result<Vec<AstNode>, KladdError> {
    let tokens = tokenize(input);
    Ok(parse_simple(tokens)?)
}

#[cfg(test)]
pub mod test_utils;

#[cfg(test)]
mod tests {
    use serde::Deserialize;

    use crate::{
        ast_visualizer::{visualize_document, visualize_nodes},
        html::{HtmlOptions, to_html_with_options},
        lexer::tokenize,
        parse_kladd,
        parser::parse,
        test_utils::TEST_INPUT,
    };

    #[derive(Deserialize)]
    struct Metadata {
        metadata: String,
    }

    #[test]
    fn it_works() {
        let tokens = tokenize(TEST_INPUT);
        assert!(!tokens.is_empty());
        insta::assert_debug_snapshot!("tokenizer", tokens);
        let (ast, meta) = parse::<Metadata>(tokens).unwrap();
        assert_eq!(meta.unwrap().metadata, "things".to_owned());

        insta::assert_debug_snapshot!("ast", ast);

        let vizualised = visualize_document(&ast);
        insta::assert_snapshot!("visualize", vizualised);

        let html = to_html_with_options(&ast, HtmlOptions::default()).unwrap();
        insta::assert_snapshot!("html", html);
    }

    #[test]
    fn test_parse_simple() {
        let input = r#"
This is some {*simple*} input without metadata.

It goes @italic[across] multiple paragraphs"#;

        let ast = parse_kladd(input).unwrap();
        let vizualised = visualize_nodes(&ast);
        insta::assert_snapshot!(vizualised);
    }
}
