use crate::ast::{AttributeValue, Attributes, Block, BlockNode, Document, Inline, InlineNode};

pub fn visualize(doc: &Document) -> String {
    let mut buf = String::new();

    buf.push_str("doc");
    buf.push('\n');
    for node in &doc.body {
        visualize_block(node, &mut buf, 2);
    }

    buf
}

fn write_attributes(attrs: &Attributes, buf: &mut String) {
    let len = attrs.len();
    for (i, attr) in attrs.iter().enumerate() {
        buf.push_str(&attr.name);
        buf.push('=');
        match &attr.value {
            AttributeValue::String(v) => buf.push_str(v),
            AttributeValue::Boolean => buf.push_str("true"),
        }

        if i < len {
            buf.push_str(", ");
        }
    }
}

fn visualize_block(node: &BlockNode, buf: &mut String, indent: usize) {
    match &node.node {
        Block::Heading { level, body } => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("heading level=");
            buf.push_str(&level.to_string());
            buf.push(' ');
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
            buf.push_str("p ");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            for node in nodes {
                visualize_inline(node, buf, indent + 2);
            }
        }
        Block::Section(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("section ");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            for node in nodes {
                visualize_block(node, buf, indent + 2);
            }
        }
        Block::Div(nodes) => {
            buf.push_str(&" ".repeat(indent));
            buf.push_str("div ");
            if let Some(attrs) = &node.attributes {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            for node in nodes {
                visualize_block(node, buf, indent + 2);
            }
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
        Inline::Hardbreak => todo!(),
    }
}
