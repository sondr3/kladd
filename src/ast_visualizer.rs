use crate::ast::{
    AstNode, AttributeKind, AttributeValue, Attributes, CodeNode, Document, HeadingNode, LinkNode,
    NamedNode, NodeKind, NodeTag, Quote, QuotedNode,
};

pub struct Visualizer {
    buf: String,
    depth: usize,
}

impl Visualizer {
    fn new() -> Self {
        Visualizer {
            buf: String::new(),
            depth: 2,
        }
    }

    fn depth_from_tag(&mut self, tag: NodeTag) {
        match tag {
            NodeTag::Start => self.depth += 2,
            NodeTag::End => self.depth -= 2,
        }
    }

    fn indent(&mut self) {
        self.push_str(&" ".repeat(self.depth));
    }

    pub(crate) fn push_str(&mut self, str: &str) {
        self.buf.push_str(str);
    }

    fn push(&mut self, char: char) {
        self.buf.push(char);
    }
}

pub fn visualize_document(doc: &Document) -> String {
    let mut vis = Visualizer::new();

    vis.push_str("doc");
    vis.push('\n');
    for node in &doc.body {
        visualize_node_inner(node, &mut vis);
    }

    vis.buf
}

pub fn visualize_nodes(nodes: &[AstNode]) -> String {
    let mut vis = Visualizer::new();
    vis.depth = 0;

    for node in nodes {
        visualize_node_inner(node, &mut vis);
    }

    vis.buf
}

fn write_attribute((kind, value): (&AttributeKind, &AttributeValue), vis: &mut Visualizer) {
    vis.push(' ');
    kind.write_ast(vis);
    vis.push('=');
    match value {
        AttributeValue::String(v) => vis.push_str(v),
        AttributeValue::Boolean => vis.push_str("true"),
    }
}

fn write_attributes(attrs: &Attributes, vis: &mut Visualizer) {
    let len = attrs.len();
    for (i, attr) in attrs.iter().enumerate() {
        write_attribute(attr, vis);
        if i < len {
            vis.push(',');
        }
    }
}

fn visualize_node_inner(node: &AstNode, buf: &mut Visualizer) {
    use NodeKind::*;
    use NodeTag::*;

    match (&node.kind, node.tag) {
        (Heading(HeadingNode { level }), Start) => {
            buf.indent();
            buf.push_str("heading level=");
            buf.push_str(&level.to_string());
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Heading(_), End) => buf.depth_from_tag(node.tag),
        (Paragraph, Start) => {
            buf.indent();
            buf.push('p');
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Paragraph, End) => buf.depth_from_tag(node.tag),
        (Block, _) => {}
        (Section, Start) => {
            buf.indent();
            buf.push_str("section");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Section, End) => buf.depth_from_tag(node.tag),
        (NamedBlock(NamedNode { name }), Start) => {
            buf.indent();
            buf.push_str(name);
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (NamedBlock(_), End) => buf.depth_from_tag(node.tag),
        (CodeBlock(CodeNode { language, .. }), _) => {
            buf.indent();
            buf.push_str("code");

            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }

            buf.push('\n');
            buf.depth_from_tag(NodeTag::Start);
            buf.indent();
            match language {
                Some(lang) => buf.push_str(lang),
                None => buf.push_str("unknown language"),
            };
            buf.depth_from_tag(NodeTag::End);
            buf.push('\n');
        }
        (Text(str), _) => {
            buf.indent();
            buf.push_str("str text = \"");
            buf.push_str(str);
            buf.push_str("\"\n");
        }
        (Strong, Start) => {
            buf.indent();
            buf.push_str("strong");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Strong, End) => buf.depth_from_tag(node.tag),
        (Italic, Start) => {
            buf.indent();
            buf.push_str("emph");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Italic, End) => buf.depth_from_tag(node.tag),
        (Underline, Start) => {
            buf.indent();
            buf.push_str("under");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Underline, End) => buf.depth_from_tag(node.tag),
        (Highlight, Start) => {
            buf.indent();
            buf.push_str("mark");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Highlight, End) => buf.depth_from_tag(node.tag),
        (Strikethrough, Start) => {
            buf.indent();
            buf.push_str("strike");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Strikethrough, End) => buf.depth_from_tag(node.tag),
        (Superscript, Start) => {
            buf.indent();
            buf.push_str("sup");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Superscript, End) => buf.depth_from_tag(node.tag),
        (Subscript, Start) => {
            buf.indent();
            buf.push_str("sub");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Subscript, End) => buf.depth_from_tag(node.tag),
        (Naked, Start) => {
            buf.indent();
            buf.push_str("@");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Naked, End) => buf.depth_from_tag(node.tag),
        (Quoted(QuotedNode { quote }), Start) => {
            buf.indent();
            buf.push_str(match quote {
                Quote::Single => "single_quoted",
                Quote::Double => "double_quoted",
            });

            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Quoted(_), End) => buf.depth_from_tag(node.tag),
        (Code(CodeNode { language, .. }), _) => {
            buf.indent();
            buf.push('@');
            buf.push_str("code");
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.indent();
            match language {
                Some(lang) => buf.push_str(lang),
                None => buf.push_str("unknown language"),
            }
        }
        (Link(LinkNode { href }), Start) => {
            buf.indent();
            buf.push_str("link");

            let attr = (
                &AttributeKind::Href,
                &AttributeValue::String(href.to_owned()),
            );
            write_attribute(attr, buf);
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Link(_), End) => buf.depth_from_tag(node.tag),
        (Custom(NamedNode { name }), Start) => {
            buf.indent();
            buf.push_str("@");
            buf.push_str(name);
            if let Some(attrs) = node.attributes.inner() {
                write_attributes(attrs, buf);
            }
            buf.push('\n');
            buf.depth_from_tag(node.tag);
        }
        (Custom(_), End) => buf.depth_from_tag(node.tag),
        (Softbreak, Start) => {}
        (Softbreak, End) => buf.depth_from_tag(node.tag),
    }
}
