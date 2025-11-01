#[cfg(test)]
pub const TEST_INPUT: &str = r#"
+++
metadata = things
+++

-- this is a comment

{*bold {/italic/}*} 

!h1{some=value,other=value2}[Header] 

With some @bold[body] and {/italic/} content, and a "quoted" 'text'
 "#;
