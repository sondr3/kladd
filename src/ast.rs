#[derive(Debug)]
pub struct Document {
    pub metadata: Option<String>,
    pub body: Blocks,
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
        Document {
            metadata: None,
            body: Vec::new(),
        }
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

#[derive(Debug, PartialEq, Eq, Clone)]
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

    pub fn write_ast(&self, buf: &mut String) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Attribute {
    pub kind: AttributeKind,
    pub value: AttributeValue,
}

impl Attribute {
    pub fn new(kind: AttributeKind, value: AttributeValue) -> Self {
        Attribute { kind, value }
    }
}

pub type Attributes = Vec<Attribute>;

#[derive(Debug, PartialEq, Eq)]
pub struct Node<T> {
    pub node: T,
    pub attributes: Option<Attributes>,
}

impl<T> Node<T> {
    pub fn new(node: T, attributes: Option<Attributes>) -> Self {
        Node { node, attributes }
    }

    pub fn from_node(node: T) -> Self {
        Node {
            node,
            attributes: None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct NodeBuilder<T> {
    node: Option<T>,
    attributes: Option<Attributes>,
}

impl<T> Default for NodeBuilder<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> NodeBuilder<T> {
    pub fn new() -> Self {
        NodeBuilder {
            node: None,
            attributes: None,
        }
    }

    pub fn with_node(&mut self, node: T) -> &mut Self {
        self.node = Some(node);
        self
    }

    pub fn with_attributes(&mut self, attrs: Attributes) -> &mut Self {
        self.attributes = Some(attrs);
        self
    }

    pub fn pop_attribute(
        &mut self,
        predicate: impl FnOnce(&mut Attribute) -> bool,
    ) -> Option<Attribute> {
        if let Some(attrs) = &mut self.attributes {
            let res = attrs.pop_if(predicate);
            if self.attributes.as_ref().is_some_and(|v| v.is_empty()) {
                self.attributes = None;
            }
            return res;
        }

        None
    }

    pub fn has_attributes(&self) -> bool {
        self.attributes.is_some()
    }

    pub fn has_attribute_by_kind(&self, needle: AttributeKind) -> bool {
        self.attributes
            .as_ref()
            .is_some_and(|a| a.iter().any(|i| i.kind == needle))
    }

    pub fn build(self) -> Node<T> {
        assert!(self.node.is_some());
        Node {
            node: self.node.unwrap(),
            attributes: self.attributes,
        }
    }
}

pub type BlockNode = Node<Block>;
pub type Blocks = Vec<BlockNode>;

#[derive(Debug, PartialEq, Eq)]
pub enum Block {
    Heading {
        level: u8,
        body: Inlines,
    },
    Paragraph(Inlines),
    Block(Blocks),
    Section(Blocks),
    Named {
        name: String,
        body: Blocks,
    },
    Code {
        language: Option<String>,
        body: String,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Quote {
    Single,
    Double,
}

#[derive(Debug, PartialEq, Eq)]
pub enum InlineKind {
    Strong,
    Italic,
    Underline,
    Highlight,
    Strikethrough,
    Superscript,
    Subscript,
    Naked,
    Code,
    Link,
    Custom(String),
}

pub type InlineNode = Node<Inline>;
pub type Inlines = Vec<InlineNode>;

#[derive(Debug, PartialEq, Eq)]
pub enum Inline {
    Text(String),
    Strong(Inlines),
    Italic(Inlines),
    Underline(Inlines),
    Highlight(Inlines),
    Strikethrough(Inlines),
    Superscript(Inlines),
    Subscript(Inlines),
    Naked(Inlines),
    Quoted(Quote, Inlines),
    Code {
        language: Option<String>,
        body: String,
    },
    Link {
        href: String,
        body: Inlines,
    },
    Custom {
        name: String,
        body: Inlines,
    },
    Softbreak,
}

impl Inline {
    pub fn from_kind(kind: InlineKind, body: Inlines) -> Self {
        match kind {
            InlineKind::Strong => Inline::Strong(body),
            InlineKind::Italic => Inline::Italic(body),
            InlineKind::Underline => Inline::Underline(body),
            InlineKind::Highlight => Inline::Highlight(body),
            InlineKind::Strikethrough => Inline::Strikethrough(body),
            InlineKind::Superscript => Inline::Superscript(body),
            InlineKind::Subscript => Inline::Subscript(body),
            InlineKind::Naked => Inline::Naked(body),
            InlineKind::Custom(ident) => Inline::Custom { name: ident, body },
            InlineKind::Link => panic!("cannot construct a @link from this method"),
            InlineKind::Code => panic!("cannot construct a @code from this method"),
        }
    }
}
