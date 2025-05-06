use crate::lexer::tokens::{Identifier, Keyword};

pub struct Name {
    name: Box<str>,
}

impl Name {
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl From<Keyword> for Name {
    fn from(value: Keyword) -> Self {
        Self {
            name: format!("{}", value.kind()).into_boxed_str(),
        }
    }
}

impl From<Identifier> for Name {
    fn from(value: Identifier) -> Self {
        Self {
            name: value.value().into(),
        }
    }
}
