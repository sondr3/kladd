#[cfg(test)]
pub const TEST_INPUT: &str = r#"
+++
metadata = "things"
+++

-- this is a comment

/section{class=important}
!h1{some=value,other=value2}[Header -- it's dashed] 

{*bold {/italic/}*} and @link{href=https://www.eons.io}[eons.io]
\section

/warn
@italic[Warning]
\warn

/code{language=rust}
fn main() {
  println!("Hello, world!");
}
\code

With some @bold[body] and {/italic/} content, and a "quoted" {'text'}, and <escaped>.
"#;
