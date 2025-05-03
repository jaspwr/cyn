use std::collections::LinkedList;

use crate::{grammar::BinaryOperation, interpreter::Value};

#[derive(Debug, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub assigns: Option<VRegID>,
    pub label: LabelID,
}

#[derive(Debug, Clone)]
pub enum Opcode {
    BinOp(BinaryOperation, ValueOperand, ValueOperand),
    Load(MemoryLocation),
    Store(MemoryLocation, ValueOperand),
    Push(ValueOperand),
    Pop(),
    ToFloat(ValueOperand),
    ToDouble(ValueOperand),
    ToString(ValueOperand),
    ToArray(ValueOperand),
    Jump(LabelID),
    JumpIf(LabelID, ValueOperand),
    LoadEnvVar(String),
}

#[derive(Debug, Clone)]
pub enum ValueOperand {
    VReg(VRegID),
    Immediate(Value),
}


#[derive(Debug, Clone)]
pub enum MemoryLocation {
    Stack(usize),
}

pub type VRegID = usize;
pub type LabelID = usize;

// If this is run multi-threaded sort this out
pub static mut VREG_COUNTER: usize = 0;

pub fn new_vreg() -> VRegID {
    unsafe {
        VREG_COUNTER += 1;
        VREG_COUNTER
    }
}

impl Opcode {
    pub fn to_inst(self) -> (VRegID, Instruction) {
        let vreg = new_vreg();
        let inst = Instruction {
            opcode: self,
            assigns: Some(vreg),
            label: new_vreg(),
        };

        (vreg, inst)
    }

    pub fn attach_as_inst(self, mut instructions: LinkedList<Instruction>) -> (LinkedList<Instruction>, Option<VRegID>) {
        let (vreg, inst) = self.to_inst();

        instructions.push_back(inst);

        (instructions, Some(vreg))
    }

    pub fn as_inst_list(self) -> (LinkedList<Instruction>, Option<VRegID>) {
        self.attach_as_inst(LinkedList::new())
    }
}


#[macro_export]
macro_rules! vreg {
    ($value:expr) => {
        ValueOperand::VReg($value)
    }
}

#[macro_export]
macro_rules! im {
    ($value:expr) => {
        ValueOperand::Immediate($value)
    }
}

#[macro_export]
macro_rules! ir {
    ($inst:expr => $($assign:ident = $opcode:ident, $($operand:expr),+);+) => {
        {
            let insts = $inst;
            let mut last_assign = 0;
            $(
                let (insts, $assign) = Opcode::$opcode(
                        $($operand),+
                    )
                    .attach_as_inst(insts);
                let $assign = $assign.unwrap();
                last_assign = $assign;
            )+

            (insts, Some(last_assign))
        }
    }
}


pub fn print_ir(ir: LinkedList<Instruction>) {
    for instruction in ir.iter() {
        println!("{:?} ", instruction.opcode);
    }
}

