use crate::ast::{
    Attribute, AttributeKind, AttributeValue, Attributes, Block, BlockNode, Document, Inline,
    InlineNode,
};

pub fn visualize(doc: &Document) -> String {
    let mut buf = String::new();

    buf.push_str("doc");
    buf.push('\n');
    for node in &doc.body {
        visualize_block(node, &mut buf, 2);
    }

    buf
}

fn write_attribute(attr: &Attribute, buf: &mut String) {
    buf.push(' ');
    attr.kind.write_ast(buf);
    buf.push('=');
    match &attr.value {
        AttributeValue::String(v) => buf.push_str(v),
        AttributeValue::Boolean => buf.push_str("true"),
    }
}

fn write_attributes(attrs: &Attributes, buf: &mut String) {
    let len = attrs.len();
    for (i, attr) in attrs.iter().enumerate() {
        write_attribute(attr, buf);
        if i < len {
            buf.push(',');
        }
    }
}

fn visualize_block(node: &BlockNode, buf: &mut String, indent: usize) {
    match &node.node {
        Block::Heading { level, body } => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("heading level=");
            buf.push_str(&level.to_string());
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            for node in body {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Block::Paragraph(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push('p');
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Block::Block(nodes) => {
            for node in nodes {
                visualize_block(node, buf, indent);
            }
        }
        Block::Section(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("section");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            for node in nodes {
                visualize_block(node, buf, indent + 2);
            }
        }
        Block::Named { name, body } => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str(name);

            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            for node in body {
                visualize_block(node, buf, indent + 2);
            }
        }
        Block::Code { language, .. } => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("code");

            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            buf.push_str(&" ".repeat(indent + 2));
            match language {
                Some(lang) => buf.push_str(lang),
                None => buf.push_str("unknown language"),
            };
            buf.push('\n');
        }
    }
}

fn visualize_inline(node: &InlineNode, buf: &mut String, indent: usize) {
    match &node.node {
        Inline::Text(str) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("str text = \"");
            buf.push_str(str);
            buf.push_str("\"\n");
        }
        Inline::Strong(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("strong");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Italic(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("emph");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Underline(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("under");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Highlight(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("mark");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Strikethrough(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("strike");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Superscript(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("sup");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Subscript(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("sub");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Naked(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push('@');
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Quoted(_quote, nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("quote");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Code { language, .. } => {
            buf.push_str(&" ".repeat(indent));
            buf.push('@');
            buf.push_str("code");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            match language {
                Some(lang) => buf.push_str(lang),
                None => buf.push_str("unknown language"),
            }
        }
        Inline::Link { href, body } => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("link");

            write_attribute(
                &Attribute::new(AttributeKind::Href, AttributeValue::String(href.to_owned())),
                buf,
            );
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in body {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Custom { body, name } => {
            buf.push_str(&" ".repeat(indent));
            buf.push('@');
            buf.push_str(name);
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }
            buf.push('\n');

            for node in body {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Inline::Softbreak => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("softbreak");
            buf.push('\n')
        }
    }
}
