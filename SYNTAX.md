# Syntax

## Metadata

The beginning of the document can contain metadata. This is a TOML based configuration between `+++` separators.

## Attributes

Can either have a value or be truthy:

```text
!h1{class=bold}[...]
!h1{hidden}[...]

!block{verbatim}[...]
```

Attributes can be optionally be quoted, but unless they contain whitespace it's not required.

## Inline elements

```text
This is some @bold[text] with some @italic[nested @bold[text]]. You can also
add arbitrary @{class=red}[classes] to text.

```

### Short inlines 

- Italic: `{/italic/}`
- Bold: `{*italic*}`
- Underline: `{_underline_}`
- Highlighted: `{=highlight=}`
- Strikethrough: `{-strikethrough-}`
- Comment: `{% comment %}`

## Block elements

```text
!h1{class=bold}[This is a header]

!h2{class="can also be quoted", id=some-id}[Another header]

!code{language=rust}[
fn main() {
  println!("Hello, world!");
}
]
```
