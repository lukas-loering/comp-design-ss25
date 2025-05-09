use super::{optimize::Optimizer, IrGraph};

pub struct GraphConstructor {
    optimizer: Box<dyn Optimizer>,
    graph: IrGraph
}

impl GraphConstructor {
    pub fn new(optimizer: impl Optimizer + 'static, name: String) -> Self {
        Self { optimizer: Box::new(optimizer), graph: IrGraph:: }
    }
}