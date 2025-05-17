use std::{alloc::alloc, collections::HashMap};

use itertools::Itertools;
use linked_hash_map::LinkedHashMap;
use strum::IntoEnumIterator;

use crate::ir::NodeId;

use super::{liveness::Liveness, Location, Register, RegisterProvider};

#[derive(Debug, Clone, Default)]
pub struct RegAlloc {
    color_to_reg: HashMap<u32, Alloc>,
    coloring: LinkedHashMap<NodeId, u32>,
}

#[derive(Debug, Clone, Copy)]
pub enum Alloc {
    Register(Register),
    StackSlot(usize),
}

impl From<Alloc> for Location {
    fn from(value: Alloc) -> Self {
        match value {
            Alloc::Register(register) => Location::Register(register),
            Alloc::StackSlot(byte_offset) if byte_offset == 0 => Location::Indirect(RegAlloc::SPILL_REGISTER),
            Alloc::StackSlot(byte_offset) => Location::IndirectDisplacement(byte_offset, RegAlloc::SPILL_REGISTER),
        }
    }
}

impl RegAlloc {
    const SPILL_REGISTER: Register = Register::R11;

    
    fn alloc(coloring: &LinkedHashMap<NodeId, u32>) -> HashMap<u32, Alloc> {
        let mut color_to_reg = HashMap::new();
        let colors = coloring.values().into_iter().sorted();
        let mut registers = Register::iter();
        let mut current_stack_slot = 0;
        'outer: for &color in colors {
            while let Some(register) = registers.next() {
                if Self::is_reserved(register) {
                    continue;
                }
                color_to_reg.insert(color, Alloc::Register(register));
                continue 'outer;
            }
            color_to_reg.insert(color, Alloc::StackSlot(current_stack_slot));
            current_stack_slot += 8; // move stack slot by 8 bytes, we target x86_64. We pretend everything is 8 bytes big for now.            

        }

        color_to_reg
    }

    
    fn is_reserved(reg: Register) -> bool {
        matches!(
            reg,
            Register::Rax | Register::Rdx | Register::Rsp | Register::Rbp
        ) || reg == Self::SPILL_REGISTER
    }    
}


impl RegisterProvider for RegAlloc {
    fn allocate(&mut self, graph: &crate::ir::IrGraph) -> Result<(), Box<dyn std::error::Error>> {
        let liveness = Liveness::generate(graph);
        self.coloring = liveness.coloring(graph);
        self.color_to_reg = Self::alloc(&self.coloring);
        Ok(())
    }

    fn register(&self, node: NodeId) -> Alloc {
        let color = self.coloring.get(&node).expect("node to be colored");
        *self.color_to_reg.get(color).expect("color to have slot")
    }
}