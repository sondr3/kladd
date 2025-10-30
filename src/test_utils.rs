#[cfg(test)]
pub mod test_utils {
    pub const TEST_INPUT: &'static str = r#"
+++
metadata = things
+++

{% comment %}

{*bold*} 

!h1{some=value,other=value2}[Header] 

With a @bold[body] and {/italic/} content
 "#;
}
