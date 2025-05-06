pub enum CrowExitCodes {
    Success,
    ParsingErr,
    SemanticErr,
}

impl From<CrowExitCodes> for i32 {
    fn from(value: CrowExitCodes) -> Self {
        match value {
            CrowExitCodes::Success => 0,
            CrowExitCodes::ParsingErr => 42,
            CrowExitCodes::SemanticErr => 7,
        }
    }
}
