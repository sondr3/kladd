use crate::parser::Block;

#[derive(Debug)]
pub struct Document<'a> {
    pub metadata: Option<String>,
    pub body: Vec<Block<'a>>,
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
