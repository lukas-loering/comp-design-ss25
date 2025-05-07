use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Kind {
    Basic(BasicKind),
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Basic(basic_kind) => write!(f, "{basic_kind}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BasicKind {
    Int,
}

impl From<BasicKind> for Kind {
    fn from(value: BasicKind) -> Self {
        Self::Basic(value)
    }
}

impl Display for BasicKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BasicKind::Int => write!(f, "int"),
        }
    }
}
