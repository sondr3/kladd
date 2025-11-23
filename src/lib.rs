pub mod ast;
pub mod ast_visualizer;
mod char_cursor;
pub mod error;
pub mod html;
mod lexer;
pub mod parser;
mod token_cursor;

#[cfg(test)]
pub mod test_utils;

#[cfg(test)]
mod tests {
    use serde::Deserialize;

    use crate::{
        ast::{AstNode, NodeKind, NodeTag},
        ast_visualizer::{visualize_document, visualize_nodes},
        html::{HtmlOptions, to_html_with_options},
        lexer::tokenize,
        parser::Parser,
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

        let parser = Parser::<Metadata>::new(TEST_INPUT).unwrap();
        let (doc, metadata) = parser.finish();

        assert_eq!(metadata.unwrap().metadata, "things".to_owned());

        insta::assert_debug_snapshot!("ast", doc);

        let vizualised = visualize_document(&doc);
        insta::assert_snapshot!("visualize", vizualised);

        let html = to_html_with_options(&doc, HtmlOptions::default()).unwrap();
        insta::assert_snapshot!("html", html);
    }

    #[test]
    fn test_parse_simple() {
        let input = r#"
This is some {*simple*} input without metadata.

It goes @italic[across] multiple paragraphs"#;

        let (ast, _) = Parser::<()>::new(input).unwrap().finish();
        let vizualised = visualize_nodes(&ast.body);
        insta::assert_snapshot!(vizualised);
    }

    #[test]
    fn test_document_iter() {
        let input = r"This is not {*bold*}";
        let (mut ast, _) = Parser::<()>::new(input).unwrap().finish();
        for n in ast.iter_mut() {
            match n.kind {
                NodeKind::Strong if n.tag == NodeTag::Start => {
                    *n = AstNode::start_attrs(NodeKind::Underline, n.attributes.clone());
                }
                _ => {}
            }
        }

        let vizualised = visualize_nodes(&ast.body);
        insta::assert_snapshot!(vizualised);
    }
}
