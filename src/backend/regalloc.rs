use std::{alloc::alloc, collections::HashMap};

use itertools::Itertools;
use linked_hash_map::LinkedHashMap;
use strum::IntoEnumIterator;

use crate::ir::NodeId;

use super::{Location, Register, RegisterProvider, liveness::Liveness};

#[derive(Debug, Clone, Default)]
pub struct RegAlloc {
    color_to_reg: LinkedHashMap<u32, Alloc>,
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
            Alloc::StackSlot(byte_offset) if byte_offset == 0 => Location::Indirect(Register::Rsp),
            Alloc::StackSlot(byte_offset) => {
                Location::IndirectDisplacement(byte_offset, Register::Rsp)
            }
        }
    }
}

impl RegAlloc {
    const SPILL_REGISTER: Register = Register::R11;
    // move stack slot by 8 bytes, we target x86_64. We pretend everything is 8 bytes big for now.
    const SLOT_SIZE: usize = 8;

    fn alloc(coloring: &LinkedHashMap<NodeId, u32>) -> LinkedHashMap<u32, Alloc> {
        let mut color_to_reg = LinkedHashMap::new();
        let colors = coloring.values().into_iter().unique().sorted();
        let mut registers = Register::iter();
        let mut current_stack_slot = 0;
        'outer: for &color in colors {
            while let Some(register) = registers.next() {
                if Self::is_reserved(register) {
                    continue;
                }
                let reg = Alloc::Register(register);
                color_to_reg.insert(color, reg);
                tracing::trace!("{color}->{reg:?}");
                continue 'outer;
            }
            let slot = Alloc::StackSlot(current_stack_slot);
            color_to_reg.insert(color, slot);
            tracing::trace!("{color}->{slot:?}");
            current_stack_slot += Self::SLOT_SIZE;
        }

        color_to_reg
    }

    fn is_reserved(reg: Register) -> bool {
        matches!(
            reg,
            // use for return
            Register::Rax |
            // callee-saved
            Register::Rbx | Register::R12 | Register::R13 | Register::R14 | Register::R15 |
            // use for floating point ops
            Register::Rdx | 
            // stack stuff
            Register::Rsp | Register::Rbp 
        ) || reg == Self::SPILL_REGISTER
    }
}

impl RegisterProvider for RegAlloc {
    fn allocate(&mut self, graph: &crate::ir::IrGraph) -> Result<(), Box<dyn std::error::Error>> {
        let liveness = Liveness::generate(graph);
        tracing::info!("completed liveness calculation");
        tracing::trace!("Liveness:\n{}", liveness.show(graph));

        self.coloring = liveness.coloring(graph);
        tracing::info!("completed coloring");
        tracing::debug!("Coloring:\n{:?}", self.coloring);
        self.color_to_reg = Self::alloc(&self.coloring);
        tracing::debug!("Reg->Color:\n{:?}", self.color_to_reg);

        Ok(())
    }

    fn register(&self, node: NodeId) -> Alloc {
        let color = self.coloring.get(&node).expect("node to be colored");
        *self.color_to_reg.get(color).expect("color to have slot")
    }

    fn required_stack_size(&self) -> usize {
        let offset = self
            .color_to_reg
            .values()
            .filter_map(|alloc| {
                if let Alloc::StackSlot(offset) = *alloc {
                    Some(offset)
                } else {
                    None
                }
            })
            .max();
        let Some(offset) = offset else {
            return 0;
        };

        // Calling conventions wants alignment to 16 bytes
        assert_eq!(8, Self::SLOT_SIZE);
        offset + (Self::SLOT_SIZE * 2)
    }

    fn spill_register(&self) -> Alloc {
        Alloc::Register(Self::SPILL_REGISTER)
    }
}
