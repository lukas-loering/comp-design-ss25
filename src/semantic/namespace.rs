use std::collections::HashMap;

use crate::parser::{ast::NameTree, symbol::Name};

#[derive(Debug, Default, Clone)]
pub struct Namespace<T: Clone> {
    content: HashMap<Name, T>,
}

impl<T: Clone> Namespace<T> {
    pub fn new() -> Self {
        Self {
            content: Default::default(),
        }
    }

    pub fn put<E>(
        &mut self,
        name: &NameTree,
        value: T,
        modify: impl Fn(&T, T) -> Result<T, E>,
    ) -> Result<(), E> {
        match self.content.get(name.name()) {
            Some(existing) => {
                let new = modify(existing, value)?;
                self.content.insert(name.name().clone(), new);
            }
            None => {
                self.content.insert(name.name().clone(), value);
            }
        }
        Ok(())
    }

    pub fn get(&self, name: &NameTree) -> Option<T> {
        self.content.get(name.name()).cloned()
    }
}
