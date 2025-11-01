#[derive(Debug)]
pub struct Document<'a> {
    pub metadata: Option<String>,
    pub body: Blocks<'a>,
    // pub references: HashMap<String, String>,
    // pub footnotes: HashMap<String, String>,
}

impl<'a> Default for Document<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Document<'a> {
    pub fn new() -> Self {
        Document {
            metadata: None,
            body: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AttributeValue<'a> {
    String(&'a str),
    Boolean,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Attribute<'a> {
    pub name: &'a str,
    pub value: AttributeValue<'a>,
}

#[derive(Debug)]
pub struct Node<'a, T> {
    node: T,
    attributes: Option<Attribute<'a>>,
}

impl<'a, T> Node<'a, T> {
    pub fn new(node: T, attributes: Option<Attribute<'a>>) -> Self {
        Node { node, attributes }
    }
}

pub type BlockNode<'a> = Node<'a, Block<'a>>;
pub type Blocks<'a> = Vec<BlockNode<'a>>;

#[derive(Debug)]
pub enum Block<'a> {
    Heading { level: u8, body: Inlines<'a> },
    Span(Inlines<'a>),
    Paragraph(Inlines<'a>),
    Section(Blocks<'a>),
    Div(Blocks<'a>),
}

#[derive(Debug)]
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
}

pub type InlineNode<'a> = Node<'a, Inline<'a>>;
pub type Inlines<'a> = Vec<InlineNode<'a>>;

#[derive(Debug)]
pub enum Inline<'a> {
    Text(&'a str),
    Strong(Inlines<'a>),
    Italic(Inlines<'a>),
    Underline(Inlines<'a>),
    Highlight(Inlines<'a>),
    Strikethrough(Inlines<'a>),
    Superscript(Inlines<'a>),
    Subscript(Inlines<'a>),
    Quoted(Quote, Inlines<'a>),
    Softbreak,
    Hardbreak,
}

impl<'a> Inline<'a> {
    pub fn from_kind(kind: InlineKind, body: Inlines<'a>) -> Self {
        match kind {
            InlineKind::Strong => Inline::Strong(body),
            InlineKind::Italic => Inline::Italic(body),
            InlineKind::Underline => Inline::Underline(body),
            InlineKind::Highlight => Inline::Highlight(body),
            InlineKind::Strikethrough => Inline::Strikethrough(body),
            InlineKind::Superscript => todo!(),
            InlineKind::Subscript => todo!(),
        }
    }
}
