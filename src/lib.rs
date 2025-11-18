#[cfg(feature = "serde")]
use serde::de::DeserializeOwned;

use crate::{ast::Document, error::KladdError, lexer::tokenize, parser::parse};

pub mod ast;
pub mod ast_visualizer;
mod char_cursor;
pub mod error;
pub mod html;
mod lexer;
mod parser;
mod token_cursor;

#[cfg(not(feature = "serde"))]
pub fn parse_kladd(input: String) -> Result<(Document, Option<String>), KladdError> {
    let tokens = tokenize(&input);
    let res = parse(tokens)?;
    Ok(res)
}

#[cfg(feature = "serde")]
pub fn parse_kladd<T>(input: String) -> Result<(Document, Option<T>), KladdError>
where
    T: DeserializeOwned,
{
    let tokens = tokenize(&input);
    let res = parse(tokens)?;
    Ok(res)
}

#[cfg(test)]
pub mod test_utils;

#[cfg(test)]
mod tests {
    use crate::{
        ast_visualizer::visualize,
        html::{HtmlOptions, to_html_with_options},
        lexer::tokenize,
        parser::parse,
        test_utils::TEST_INPUT,
    };
    #[cfg(feature = "serde")]
    use serde::Deserialize;

    #[cfg_attr(feature = "serde", derive(Deserialize))]
    #[cfg(feature = "serde")]
    struct Metadata {
        metadata: String,
    }

    #[test]
    fn it_works() {
        let tokens = tokenize(TEST_INPUT);
        assert!(!tokens.is_empty());
        insta::assert_debug_snapshot!("tokenizer", tokens);

        #[cfg(not(feature = "serde"))]
        let (ast, _) = parse(tokens).unwrap();

        #[cfg(feature = "serde")]
        let (ast, meta) = parse::<Metadata>(tokens).unwrap();

        #[cfg(feature = "serde")]
        assert_eq!(meta.unwrap().metadata, "things".to_owned());

        insta::assert_debug_snapshot!("ast", ast);

        let vizualised = visualize(&ast);
        insta::assert_snapshot!("visualize", vizualised);

        let html = to_html_with_options(&ast, HtmlOptions::default());
        insta::assert_snapshot!("html", html);
    }
}
