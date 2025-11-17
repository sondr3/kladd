use htmlize::{escape_attribute, escape_text};

use crate::ast::{
    Attribute, AttributeKind, AttributeValue, Attributes, Block, BlockNode, Document, Inline,
    InlineNode,
};

pub fn to_html(Document { body, .. }: &Document) -> String {
    let mut res = String::new();

    for elem in body {
        htmlify_block(elem, &mut res);
    }

    res
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

fn htmlify_block(node: &BlockNode, buf: &mut String) {
    match &node.node {
        Block::Heading { level, body } => {
            buf.push('<');
            buf.push_str(level_to_heading(*level));

            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('>');

            for node in body {
                htmlify_inline(node, buf);
            }

            buf.push_str("</");
            buf.push_str(level_to_heading(*level));
            buf.push('>');
            buf.push('\n');
        }
        Block::Paragraph(nodes) => {
            buf.push_str("<p>");

            for node in nodes {
                htmlify_inline(node, buf);
            }

            buf.push_str("</p>");
        }
        Block::Block(nodes) => {
            for node in nodes {
                htmlify_block(node, buf);
            }
        }
        Block::Section(nodes) => {
            buf.push_str("<section");

            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('>');
            buf.push('\n');

            for node in nodes {
                htmlify_block(node, buf);
            }

            buf.push_str("</section>");
            buf.push('\n');
        }
        Block::Named { name, body } => {
            buf.push_str("<div");

            let mut class_seen = false;
            if let Some(attrs) = &node.attributes {
                for attr in attrs {
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

            for node in body {
                htmlify_block(node, buf);
            }

            buf.push_str("</div>");
            buf.push('\n');
        }
        Block::Code { language, body } => {
            buf.push_str("<pre><code");

            if let Some(lang) = language {
                buf.push_str(" data-lang=");
                buf.push('"');
                buf.push_str(lang);
                buf.push('"');
            }

            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('>');
            buf.push('\n');

            buf.push_str(body);

            buf.push_str("</pre></code>");

            buf.push('\n');
        }
    }
}

fn htmlify_inline(node: &InlineNode, buf: &mut String) {
    match &node.node {
        Inline::Text(str) => buf.push_str(&escape_text(str)),
        Inline::Strong(nodes) => {
            buf.push_str("<strong>");
            for node in nodes {
                htmlify_inline(node, buf);
            }
            buf.push_str("</strong>");
        }
        Inline::Italic(nodes) => {
            buf.push_str("<em>");
            for node in nodes {
                htmlify_inline(node, buf);
            }
            buf.push_str("</em>");
        }
        Inline::Underline(nodes) => {
            buf.push_str(r#"<span class="underline">"#);
            for node in nodes {
                htmlify_inline(node, buf);
            }
            buf.push_str("</span>");
        }
        Inline::Highlight(nodes) => {
            buf.push_str("<mark>");
            for node in nodes {
                htmlify_inline(node, buf);
            }
            buf.push_str("</mark>");
        }
        Inline::Strikethrough(nodes) => {
            buf.push_str("<s>");
            for node in nodes {
                htmlify_inline(node, buf);
            }
            buf.push_str("</s>");
        }
        Inline::Superscript(nodes) => {
            buf.push_str("<sup>");
            for node in nodes {
                htmlify_inline(node, buf);
            }
            buf.push_str("</sup>");
        }
        Inline::Subscript(nodes) => {
            buf.push_str("<sub>");
            for node in nodes {
                htmlify_inline(node, buf);
            }
            buf.push_str("</sub>");
        }
        Inline::Naked(nodes) => {
            buf.push_str("<span");

            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('>');

            for node in nodes {
                htmlify_inline(node, buf);
            }
            buf.push_str("</span>");
        }
        Inline::Quoted(_quote, nodes) => {
            for node in nodes {
                htmlify_inline(node, buf);
            }
        }
        Inline::Code { language, body } => {
            buf.push_str("<code");

            if let Some(lang) = language {
                buf.push_str(" data-lang=");
                buf.push('"');
                buf.push_str(lang);
                buf.push('"');
            }

            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('>');

            buf.push_str(body);
            buf.push_str("</code>");
        }
        Inline::Link { href, body } => {
            buf.push_str("<a");

            write_attribute(
                &Attribute::new(AttributeKind::Href, AttributeValue::String(href.to_owned())),
                buf,
            );
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('>');

            for node in body {
                htmlify_inline(node, buf);
            }
            buf.push_str("</a>");
        }
        Inline::Custom { body, .. } => {
            buf.push_str("<span");

            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('>');

            for node in body {
                htmlify_inline(node, buf);
            }
            buf.push_str("</span>");
        }
        Inline::Softbreak => buf.push('\n'),
    }
}

#[cfg(test)]
mod tests {}
