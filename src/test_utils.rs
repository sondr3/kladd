#[cfg(test)]
pub const TEST_INPUT: &str = r#"
+++
metadata = things
+++

-- this is a comment

#section{class=important}
!h1{some=value,other=value2}[Header] 

{*bold {/italic/}*} 
#section

With some @bold[body] and {/italic/} content, and a "quoted" 'text', and <escaped>.
 "#;
