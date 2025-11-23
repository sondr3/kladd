use std::collections::{BTreeMap, btree_map::Keys};

use crate::{ast_visualizer::Visualizer, lexer::TokenKind};

#[derive(Debug)]
pub struct Document {
    pub(crate) body: Vec<AstNode>,
    // pub references: HashMap<String, String>,
    // pub footnotes: HashMap<String, String>,
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}

impl Document {
    pub fn new() -> Self {
        Document { body: Vec::new() }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AttributeValue {
    String(String),
    Boolean,
}

impl AttributeValue {
    pub fn concat(&self, other: Self) -> Self {
        match (self, other) {
            (AttributeValue::String(a), AttributeValue::String(b)) => {
                AttributeValue::String(format!("{a} {b}"))
            }
            (AttributeValue::Boolean, AttributeValue::Boolean) => AttributeValue::Boolean,
            _ => panic!("Attempt at concating two different attribute values"),
        }
    }

    pub fn inner(&self) -> String {
        match self {
            Self::String(v) => v.to_owned(),
            Self::Boolean => "true".to_owned(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum AttributeKind {
    /// A `.class` attribute
    Class,
    /// A `#id` attribute
    Id,
    /// A `href` attribute
    Href,
    Attr(String),
}

impl AttributeKind {
    pub fn write_html(&self, buf: &mut String) {
        match self {
            AttributeKind::Class => buf.push_str("class"),
            AttributeKind::Id => buf.push_str("id"),
            AttributeKind::Href => buf.push_str("href"),
            AttributeKind::Attr(v) => match v.as_str() {
                "alt" | "background" | "checked" | "dir" | "disabled" | "hidden" | "style"
                | "title" => buf.push_str(v),
                _ => {
                    buf.push_str("data-");
                    buf.push_str(v);
                }
            },
        }
    }

    pub(crate) fn write_ast(&self, buf: &mut Visualizer) {
        match self {
            AttributeKind::Class => buf.push_str("class"),
            AttributeKind::Id => buf.push_str("id"),
            AttributeKind::Href => buf.push_str("href"),
            AttributeKind::Attr(v) => buf.push_str(v),
        }
    }
}

#[cfg(any(debug_assertions, test))]
impl From<&str> for AttributeKind {
    fn from(value: &str) -> Self {
        match value {
            "href" => AttributeKind::Href,
            "class" | "." => AttributeKind::Class,
            "id" | "#" => AttributeKind::Id,
            _ => AttributeKind::Attr(value.to_string()),
        }
    }
}

pub type Attributes = BTreeMap<AttributeKind, AttributeValue>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstAttributes(Option<Attributes>);

impl<const N: usize> From<[(AttributeKind, AttributeValue); N]> for AstAttributes {
    fn from(value: [(AttributeKind, AttributeValue); N]) -> Self {
        AstAttributes::new(BTreeMap::from(value))
    }
}

impl AstAttributes {
    pub fn new(attrs: Attributes) -> Self {
        Self(Some(attrs))
    }

    pub fn empty() -> Self {
        Self(None)
    }

    pub fn inner(&self) -> Option<&Attributes> {
        self.0.as_ref()
    }

    pub fn keys(&self) -> Keys<'_, AttributeKind, AttributeValue> {
        self.0.as_ref().map(|a| a.keys()).unwrap_or_default()
    }

    pub fn remove(&mut self, needle: AttributeKind) -> Option<AttributeValue> {
        match self.0.as_mut() {
            Some(attrs) => {
                let value = attrs.remove(&needle);
                if attrs.is_empty() {
                    self.0 = None;
                }

                value
            }
            None => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Heading(HeadingNode),
    Paragraph,
    Block,
    Section,
    NamedBlock(NamedNode),
    CodeBlock(CodeNode),
    Text(String),
    Strong,
    Italic,
    Underline,
    Highlight,
    Strikethrough,
    Superscript,
    Subscript,
    Naked,
    Quoted(QuotedNode),
    Code(CodeNode),
    Link(LinkNode),
    Custom(NamedNode),
    Softbreak,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct HeadingNode {
    pub level: u8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeNode {
    pub language: Option<String>,
    pub body: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedNode {
    pub name: String,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct QuotedNode {
    pub quote: Quote,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkNode {
    pub href: String,
}

impl NodeKind {
    pub fn is_block(&self) -> bool {
        matches!(
            *self,
            NodeKind::Heading(..)
                | NodeKind::Paragraph
                | NodeKind::Block
                | NodeKind::Section
                | NodeKind::NamedBlock(..)
                | NodeKind::CodeBlock(..)
        )
    }

    pub fn text(&self) -> Option<&str> {
        match self {
            NodeKind::Text(text) => Some(text),
            _ => None,
        }
    }

    pub fn from_simple_inline(kind: TokenKind) -> Self {
        match kind {
            TokenKind::ForwardSlash => NodeKind::Italic,
            TokenKind::Star => NodeKind::Strong,
            TokenKind::Underscore => NodeKind::Underline,
            TokenKind::Equals => NodeKind::Highlight,
            TokenKind::Tilde => NodeKind::Strikethrough,
            TokenKind::SingleQuote => NodeKind::Quoted(QuotedNode {
                quote: Quote::Single,
            }),
            _ => panic!("invalid kind {:?} for simple inline", kind),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeTag {
    Start,
    End,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNode {
    pub kind: NodeKind,
    pub attributes: AstAttributes,
    pub tag: NodeTag,
}

impl AstNode {
    pub fn start(kind: NodeKind) -> Self {
        AstNode {
            kind,
            attributes: AstAttributes::empty(),
            tag: NodeTag::Start,
        }
    }

    pub fn start_attrs(kind: NodeKind, attributes: AstAttributes) -> Self {
        AstNode {
            kind,
            attributes,
            tag: NodeTag::Start,
        }
    }

    pub fn end(kind: NodeKind) -> Self {
        AstNode {
            kind,
            attributes: AstAttributes::empty(),
            tag: NodeTag::End,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Quote {
    Single,
    Double,
}

impl From<TokenKind> for Quote {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::DoubleQuote => Quote::Double,
            TokenKind::SingleQuote => Quote::Single,
            _ => panic!("attempting to convert {:?} into quote", value),
        }
    }
}
