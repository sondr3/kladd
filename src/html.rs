use crate::ast::{BlockNode, Document};

pub fn to_html(Document { body, .. }: Document) -> String {
    let mut res = String::new();

    for elem in body {
        htmlify_block(elem, &mut res);
    }

    res
}

fn htmlify_block(block: BlockNode, buf: &mut String) {
    todo!()
    // match block {
    //     Block::Block { name, body, .. } => {
    //         buf.push('<');
    //         buf.push_str(name);
    //         buf.push('>');
    //
    //         for elem in body {
    //             htmlify_block(elem, buf);
    //         }
    //
    //         buf.push_str("</");
    //         buf.push_str(name);
    //         buf.push('>');
    //         buf.push('\n');
    //     }
    //     Block::Inline { name, body, .. } => {
    //         buf.push('<');
    //         buf.push_str(name);
    //         buf.push('>');
    //
    //         for elem in body {
    //             htmlify_block(elem, buf);
    //         }
    //
    //         buf.push_str("</");
    //         buf.push_str(name);
    //         buf.push('>');
    //     }
    //     Block::NakedInline { body, .. } => {
    //         buf.push_str("<span>");
    //         for elem in body {
    //             htmlify_block(elem, buf);
    //         }
    //         buf.push_str("</span>");
    //     }
    //     Block::SimpleInline { name, body } => {
    //         buf.push('<');
    //         buf.push_str(name);
    //         buf.push('>');
    //
    //         for elem in body {
    //             htmlify_block(elem, buf);
    //         }
    //
    //         buf.push_str("</");
    //         buf.push_str(name);
    //         buf.push('>');
    //     }
    //     Block::Container { body } => {
    //         buf.push_str("<p>");
    //         for elem in body {
    //             htmlify_block(elem, buf);
    //         }
    //         buf.push_str("</p>");
    //     }
    //     Block::Text { body, .. } => buf.push_str(body),
    //     Block::Whitespace => buf.push(' '),
    //     Block::Newline => buf.push('\n'),
    //     Block::Comment { .. } | Block::Unknown | Block::EOF => (),
    // }
}

#[cfg(test)]
mod tests {}
