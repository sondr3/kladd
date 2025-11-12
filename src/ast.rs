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

#[derive(Debug, PartialEq, Eq)]
pub enum AttributeValue {
    String(String),
    Boolean,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Attribute {
    pub name: String,
    pub value: AttributeValue,
}

impl Attribute {
    pub fn new(name: String, value: AttributeValue) -> Self {
        Attribute { name, value }
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
    Heading { level: u8, body: Inlines },
    Paragraph(Inlines),
    Block(Blocks),
    Section(Blocks),
    Div(Blocks),
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
    Custom { name: String, body: Inlines },
    Softbreak,
    Hardbreak,
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
        }
    }
}
