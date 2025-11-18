use std::{error::Error, fmt::Display};

use crate::{ast::AttributeKind, lexer::TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsingError {
    ParseError(&'static str),
    UnexpectedEnd,
    UnexpectedTokenKind(TokenKind, &'static str),
    UnexpectedToken(TokenKind, String, &'static str),
    DuplicateAttribute(AttributeKind),
    InvalidAttribute(TokenKind),
    InvalidAttributeKind(TokenKind),
    MissingAttribute(&'static str, &'static str),
    InvalidHeading(String),
    MissingBlockEnd(String),
    #[cfg(feature = "serde")]
    MetadataDeserializationFailed(String),
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::ParseError(e) => write!(f, "{}", e),
            ParsingError::UnexpectedToken(token, name, desc) => {
                write!(
                    f,
                    "found {:?} with body {} in {} unexpectedly",
                    token, name, desc
                )
            }
            ParsingError::DuplicateAttribute(kind) => {
                write!(f, "found {:?} as a duplicate attribute", kind)
            }
            ParsingError::InvalidAttribute(kind) => {
                write!(f, "{:?} is not a valid attribute", kind)
            }
            ParsingError::InvalidAttributeKind(kind) => {
                write!(f, "{:?} is not valid as an attribute key", kind)
            }
            ParsingError::MissingAttribute(attr, name) => {
                write!(f, "attribute {} required in {}", attr, name)
            }
            ParsingError::InvalidHeading(_) => todo!(),
            ParsingError::UnexpectedTokenKind(token_kind, desc) => {
                write!(
                    f,
                    "found token kind {:?} in {} unexpectedly",
                    token_kind, desc
                )
            }
            ParsingError::UnexpectedEnd => write!(f, "unexpectedly reached the end of input"),
            ParsingError::MissingBlockEnd(name) => {
                write!(f, "did not reach the end of {} block", name)
            }
            #[cfg(feature = "serde")]
            ParsingError::MetadataDeserializationFailed(inner) => {
                write!(
                    f,
                    "metadata deserialization failed with serde error: {}",
                    inner
                )
            }
        }
    }
}

impl Error for ParsingError {}

impl From<ParsingError> for KladdError {
    fn from(value: ParsingError) -> Self {
        KladdError::ParsingError(value)
    }
}

#[cfg(feature = "serde")]
impl From<toml::de::Error> for ParsingError {
    fn from(value: toml::de::Error) -> Self {
        ParsingError::MetadataDeserializationFailed(value.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KladdError {
    ParsingError(ParsingError),
}

impl Display for KladdError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KladdError::ParsingError(e) => write!(f, "{}", e),
        }
    }
}

impl Error for KladdError {}
