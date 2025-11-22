use htmlize::{escape_attribute, escape_text};

use crate::{
    ast::{
        AstNode, Attribute, AttributeKind, AttributeValue, Attributes, CodeNode, Document,
        HeadingNode, LinkNode, NamedNode, NodeKind, NodeTag, Quote, QuotedNode,
    },
    error::{HtmlRenderError, KladdError},
};

#[derive(Debug, Clone, Copy)]
pub struct HtmlOptions {
    /// Render newlines as actual newlines in the HTML
    preserve_newlines: bool,
    /// Convert things like `...` into `…`
    smart_punctuation: bool,
}

impl Default for HtmlOptions {
    fn default() -> Self {
        Self {
            preserve_newlines: false,
            smart_punctuation: true,
        }
    }
}

pub fn to_html(doc: &Document) -> Result<String, KladdError> {
    Ok(inner_to_html(doc, None)?)
}

pub fn to_html_with_options(doc: &Document, options: HtmlOptions) -> Result<String, KladdError> {
    Ok(inner_to_html(doc, Some(options))?)
}

fn inner_to_html(
    Document { body, .. }: &Document,
    options: Option<HtmlOptions>,
) -> Result<String, HtmlRenderError> {
    let mut buf = String::new();

    for node in body {
        htmlify_node(node, options, &mut buf)?;
    }

    Ok(buf)
}

fn level_to_heading(level: u8) -> &'static str {
    match level {
        1 => "h1",
        2 => "h2",
        3 => "h3",
        4 => "h4",
        5 => "h5",
        _ => "h6",
    }
}

fn write_attribute(attr: &Attribute, buf: &mut String) {
    buf.push(' ');
    attr.kind.write_html(buf);

    match &attr.value {
        AttributeValue::String(v) => {
            buf.push('=');
            buf.push('"');
            buf.push_str(&escape_attribute::<&str>(v));
            buf.push('"');
        }
        AttributeValue::Boolean => todo!(),
    }
}

fn write_attributes(attrs: &Attributes, buf: &mut String) {
    for attr in attrs {
        write_attribute(attr, buf);
    }
}

type HtmlResult = std::result::Result<(), HtmlRenderError>;

fn htmlify_node(node: &AstNode, options: Option<HtmlOptions>, buf: &mut String) -> HtmlResult {
    use NodeKind::*;
    use NodeTag::*;

    match (&node.kind, node.tag) {
        (Heading(HeadingNode { level }), Start) => {
            buf.push('<');
            buf.push_str(level_to_heading(*level));

            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('>');
        }
        (Heading(HeadingNode { level }), End) => {
            buf.push_str("</");
            buf.push_str(level_to_heading(*level));
            buf.push('>');
            buf.push('\n');
        }
        (Paragraph, Start) => buf.push_str("<p>"),
        (Paragraph, End) => buf.push_str("</p>"),
        (Block, _) => {}
        (Section, Start) => {
            buf.push_str("<section");

            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('>');
            buf.push('\n');
        }
        (Section, End) => {
            buf.push_str("</section>");
            buf.push('\n');
        }
        (NamedBlock(NamedNode { name }), Start) => {
            buf.push_str("<div");

            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }

            let mut class_seen = false;
            if let Some(attrs) = &node.attributes.inner() {
                for attr in attrs.iter() {
                    if attr.kind == AttributeKind::Class {
                        class_seen = true;
                        let attr = dbg!(Attribute::new(
                            AttributeKind::Class,
                            attr.value.concat(AttributeValue::String(name.to_owned())),
                        ));
                        write_attribute(&attr, buf);
                    }

                    write_attribute(attr, buf);
                }
            }

            if !class_seen {
                write_attribute(
                    &Attribute::new(
                        AttributeKind::Class,
                        AttributeValue::String(name.to_owned()),
                    ),
                    buf,
                );
            }

            buf.push('>');
            buf.push('\n');
        }
        (NamedBlock(_), End) => {
            buf.push_str("</div>");
            buf.push('\n');
        }
        (CodeBlock(CodeNode { language, body }), _) => {
            buf.push_str("<pre><code");

            if let Some(lang) = language {
                buf.push_str(" data-lang=");
                buf.push('"');
                buf.push_str(lang);
                buf.push('"');
            }

            if let Some(attrs) = &node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('>');
            buf.push('\n');

            buf.push_str(body);

            buf.push_str("</pre></code>");
            buf.push('\n');
        }
        (Text(str), _) => {
            if options.is_some_and(|o| o.smart_punctuation) {
                buf.push_str(
                    &escape_text(str)
                        .replace("...", "&hellip;")
                        .replace("---", "&mdash;")
                        .replace("--", "&ndash;"),
                );
            } else {
                buf.push_str(&escape_text(str));
            }
        }
        (Strong, Start) => buf.push_str("<strong>"),
        (Strong, End) => buf.push_str("</strong>"),
        (Italic, Start) => buf.push_str("<em>"),
        (Italic, End) => buf.push_str("</em>"),
        (Underline, Start) => buf.push_str(r#"<span class="underline">"#),
        (Underline, End) => buf.push_str("</span>"),
        (Highlight, Start) => buf.push_str("<mark>"),
        (Highlight, End) => buf.push_str("</mark>"),
        (Strikethrough, Start) => buf.push_str("<s>"),
        (Strikethrough, End) => buf.push_str("</s>"),
        (Superscript, Start) => buf.push_str("<sup>"),
        (Superscript, End) => buf.push_str("</sup>"),
        (Subscript, Start) => buf.push_str("<sub>"),
        (Subscript, End) => buf.push_str("</sub>"),
        (Naked, Start) => {
            buf.push_str("<span");

            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('>');
        }
        (Naked, End) => buf.push_str("</span>"),
        (Quoted(QuotedNode { quote }), _) => {
            match (quote, options.is_some_and(|o| o.smart_punctuation)) {
                (Quote::Single, true) => buf.push('‘'),
                (Quote::Double, true) => buf.push('“'),
                (Quote::Single, false) => buf.push('\''),
                (Quote::Double, false) => buf.push('"'),
            }
        }
        (Code(CodeNode { language, body }), _) => {
            buf.push_str("<code");

            if let Some(lang) = language {
                buf.push_str(" data-lang=");
                buf.push('"');
                buf.push_str(lang);
                buf.push('"');
            }

            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('>');

            buf.push_str(body);
            buf.push_str("</code>");
        }
        (Link(LinkNode { href }), Start) => {
            buf.push_str("<a");

            write_attribute(
                &Attribute::new(AttributeKind::Href, AttributeValue::String(href.to_owned())),
                buf,
            );
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('>');
        }
        (Link(_), End) => buf.push_str("</a>"),
        (Custom(_), Start) => {
            buf.push_str("<span");

            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('>');
        }
        (Custom(_), End) => buf.push_str("</span>"),
        (Softbreak, _) => {
            if options.is_some_and(|o| o.preserve_newlines) {
                buf.push('\n')
            } else {
                buf.push(' ');
            }
        }
    }

    Ok(())
}
