use std::fmt::Display;

pub trait HasSpan {
    fn span(&self) -> Span;
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    start: Position,
    end: Position,
}

#[derive(Debug, Copy, Clone)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
    }

    /// Returns a new span starting at this spans start and ending at the later spans end
    pub fn merge(&self, later: Span) -> Self {
        Self::new(self.start, later.end)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}|{}]", self.start, self.end)
    }
}
