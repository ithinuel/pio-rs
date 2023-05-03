use std::collections::HashMap;

pub struct Symbol {
    public: bool,
    value: i32,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolName<'i> {
    span: nom_locate::LocatedSpan<&'i str>,
}
pub type SymbolRegister<'i> = HashMap<&'i str, Symbol>;

#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
pub enum ReifyError<'i> {
    #[error("Unknown symbol {}", .0.span.fragment())]
    UnknownSymbol(SymbolName<'i>),
    #[error("Overflowing expression")]
    Overflow,
}

impl From<core::num::TryFromIntError> for ReifyError<'_> {
    fn from(value: core::num::TryFromIntError) -> Self {
        ReifyError::Overflow
    }
}

#[derive(Debug)]
pub enum Value<'i> {
    I32(i32),
    Symbol(SymbolName<'i>),
    Add(Box<Value<'i>>, Box<Value<'i>>),
    Sub(Box<Value<'i>>, Box<Value<'i>>),
    Mul(Box<Value<'i>>, Box<Value<'i>>),
    Div(Box<Value<'i>>, Box<Value<'i>>),
    Neg(Box<Value<'i>>),
    Rev(Box<Value<'i>>),
}

impl<'i> Value<'i> {
    pub fn reify(&self, reg: &SymbolRegister<'i>) -> Result<i32, ReifyError<'i>> {
        Ok(match self {
            Value::I32(v) => *v,
            Value::Symbol(s) => {
                reg.get(s.span.fragment())
                    .ok_or_else(|| ReifyError::UnknownSymbol(s.clone()))?
                    .value
            }
            Value::Add(a, b) => a.reify(reg)? + b.reify(reg)?,
            Value::Sub(a, b) => a.reify(reg)? - b.reify(reg)?,
            Value::Mul(a, b) => a.reify(reg)? * b.reify(reg)?,
            Value::Div(a, b) => a.reify(reg)? / b.reify(reg)?,
            Value::Neg(a) => -a.reify(reg)?,
            Value::Rev(a) => a.reify(reg)?.reverse_bits(),
        })
    }
}

//#[derive(Debug)]
//pub(crate) enum Line<'input> {
//    Directive(ParsedDirective<'input>),
//    Instruction(ParsedInstruction<'input>),
//    Label { public: bool, name: &'input str },
//}

#[derive(Debug)]
pub enum Directive<'input> {
    Define {
        public: bool,
        name: &'input str,
        value: Value<'input>,
    },
    Origin(Value<'input>),
    SideSet {
        value: Value<'input>,
        opt: bool,
        pindirs: bool,
    },
    WrapTarget,
    Wrap,
    LangOpt(&'input str),
}

#[derive(Debug)]
pub struct Instruction<'input> {
    operands: Operands<'input>,
    side_set: Option<Value<'input>>,
    delay: Value<'input>,
}

impl<'i> Instruction<'i> {
    pub fn reify(&self, reg: &SymbolRegister<'i>) -> Result<pio::Instruction, ReifyError<'i>> {
        Ok(pio::Instruction {
            operands: self.operands.reify(reg)?,
            side_set: self
                .side_set
                .as_ref()
                .map(|s| s.reify(reg))
                .transpose()?
                .map(u8::try_from)
                .transpose()?,
            delay: self.delay.reify(reg)? as u8,
        })
    }
}

#[derive(Debug)]
pub enum Operands<'input> {
    JMP {
        condition: pio::JmpCondition,
        address: Value<'input>,
    },
    WAIT {
        polarity: Value<'input>,
        source: pio::WaitSource,
        index: Value<'input>,
        relative: bool,
    },
    IN {
        source: pio::InSource,
        bit_count: Value<'input>,
    },
    OUT {
        destination: pio::OutDestination,
        bit_count: Value<'input>,
    },
    PUSH {
        if_full: bool,
        block: bool,
    },
    PULL {
        if_empty: bool,
        block: bool,
    },
    MOV {
        destination: pio::MovDestination,
        op: pio::MovOperation,
        source: pio::MovSource,
    },
    IRQ {
        clear: bool,
        wait: bool,
        index: Value<'input>,
        relative: bool,
    },
    SET {
        destination: pio::SetDestination,
        data: Value<'input>,
    },
}

impl<'i> Operands<'i> {
    pub fn reify(
        &self,
        state: &SymbolRegister<'i>,
    ) -> Result<pio::InstructionOperands, ReifyError<'i>> {
        Ok(match self {
            Operands::JMP { condition, address } => pio::InstructionOperands::JMP {
                condition: *condition,
                address: address.reify(state)?.try_into()?,
            },
            Operands::WAIT {
                polarity,
                source,
                index,
                relative,
            } => pio::InstructionOperands::WAIT {
                polarity: polarity.reify(state)?.try_into()?,
                source: *source,
                index: index.reify(state)?.try_into()?,
                relative: *relative,
            },
            Operands::IN { source, bit_count } => pio::InstructionOperands::IN {
                source: *source,
                bit_count: bit_count.reify(state)?.try_into()?,
            },
            Operands::OUT {
                destination,
                bit_count,
            } => pio::InstructionOperands::OUT {
                destination: *destination,
                bit_count: bit_count.reify(state)?.try_into()?,
            },
            Operands::PUSH { if_full, block } => pio::InstructionOperands::PUSH {
                if_full: *if_full,
                block: *block,
            },
            Operands::PULL { if_empty, block } => pio::InstructionOperands::PULL {
                if_empty: *if_empty,
                block: *block,
            },
            Operands::MOV {
                destination,
                op,
                source,
            } => pio::InstructionOperands::MOV {
                destination: *destination,
                op: *op,
                source: *source,
            },
            Operands::IRQ {
                clear,
                wait,
                index,
                relative,
            } => pio::InstructionOperands::IRQ {
                clear: *clear,
                wait: *wait,
                index: index.reify(state)?.try_into()?,
                relative: *relative,
            },
            Operands::SET { destination, data } => pio::InstructionOperands::SET {
                destination: *destination,
                data: data.reify(state)?.try_into()?,
            },
        })
    }
}
