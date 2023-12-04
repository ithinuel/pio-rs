use std::{marker::PhantomData, ops::Range};

use crate::compiler::Error;

/// The location of a token in an input.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location(pub Range<usize>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Line<'i> {
    Program { name: &'i str, location: Location },
    Directive(Directive<'i>),
    Instruction(Location, Instruction<'i>),
    LabelAndInstr(SymbolDef<'i>, Instruction<'i>),
    Label(SymbolDef<'i>),
    CodeBlock(&'i str, &'i str),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Directive<'i> {
    Wrap(Location),
    WrapTarget(Location),
    Origin(Location, Value<'i>),
    Define(SymbolDef<'i>, Expression<'i>),
    SideSet {
        location: Location,
        value: Value<'i>,
        optional: bool,
        pindirs: bool,
    },
    Word(Location, Value<'i>),
    LangOpt {
        location: Location,
        lang: &'i str,
        var: &'i str,
        val: &'i str,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SymbolDef<'i> {
    pub public: bool,
    pub is_label: bool,
    pub name: &'i str,
    pub location: Location,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value<'i> {
    Integer(i32),
    Identifier(Location, &'i str),
    Expression(Location, Box<Expression<'i>>),
}
impl Into<i32> for Value<'_> {
    fn into(self) -> i32 {
        match self {
            Value::Integer(v) => v,
            _ => panic!("Value can only be converted to u32 after being resolved."),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression<'i> {
    Value(Value<'i>),
    Plus(Box<Expression<'i>>, Box<Expression<'i>>),
    Minus(Box<Expression<'i>>, Box<Expression<'i>>),
    Multiply(Box<Expression<'i>>, Box<Expression<'i>>),
    Divide(Box<Expression<'i>>, Box<Expression<'i>>),
    Or(Box<Expression<'i>>, Box<Expression<'i>>),
    And(Box<Expression<'i>>, Box<Expression<'i>>),
    Xor(Box<Expression<'i>>, Box<Expression<'i>>),
    Opposite(Box<Expression<'i>>),
    Reverse(Box<Expression<'i>>),
}

impl Expression<'_> {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

pub struct ReificationError<'i>(&'i PhantomData<()>);
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instruction<'i> {
    pub ops: InstructionOperands<'i>,
    pub delay: Option<Expression<'i>>,
    pub side_set: Option<Value<'i>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionOperands<'i> {
    Nop,
    Wait {
        polarity: Value<'i>,
        src: WaitSource<'i>,
    },
    In {
        src: InSource,
        bit_count: Value<'i>,
    },
    Out {
        target: OutTarget,
        bit_count: Value<'i>,
    },
    Jmp {
        condition: Option<JmpCondition>,
        target: Expression<'i>,
    },
    Push {
        if_full: bool,
        blocking: bool,
    },
    Pull {
        if_empty: bool,
        block: bool,
    },
    Mov {
        src: MovSource,
        op: Option<MovOp>,
        trg: MovTarget,
    },
    Irq {
        modifier: IrqModifier,
        value: Value<'i>,
        relative: bool,
    },
    Set {
        target: SetTarget,
        value: Value<'i>,
    },
}
impl<'i> TryInto<pio::intermediate_repr::InstructionOperands> for InstructionOperands<'i> {
    type Error = Error<'i>;

    fn try_into(self) -> Result<pio::intermediate_repr::InstructionOperands, Self::Error> {
        use pio::intermediate_repr::{self as ir, InstructionOperands::*};

        Ok(match self {
            InstructionOperands::Nop => MOV {
                destination: ir::MovDestination::Y,
                op: ir::MovOperation::None,
                source: ir::MovSource::Y,
            },
            InstructionOperands::Wait { polarity, src } => {
                let polarity = match polarity.into() {
                    0 => ir::WaitPolarity::Zero,
                    1 => ir::WaitPolarity::One,
                    _ => {
                        return Err(Error::IntegerOverflow(
                            "wait polarity must be either 0 or 1",
                        ))
                    }
                };

                let pin_or_gpio = |index, src| match index {
                    i @ 0..=31 => Ok((src, i as u8, false)),
                    _ => Err(Error::IntegerOverflow(
                        "wait gpio index must be in [0; 31].",
                    )),
                };

                let (source, index, relative) = match src {
                    WaitSource::Irq(s, r) => match s.into() {
                        i @ 0..=7 => (ir::WaitSource::IRQ, i as u8, r),
                        _ => {
                            return Err(Error::IntegerOverflow("wait irq index must be in [0; 7]."))
                        }
                    },
                    WaitSource::Gpio(index) => pin_or_gpio(index.into(), ir::WaitSource::GPIO)?,
                    WaitSource::Pin(index) => pin_or_gpio(index.into(), ir::WaitSource::PIN)?,
                };
                WAIT {
                    polarity,
                    source,
                    index,
                    relative,
                }
            }
            //InstructionOperands::In { src, bit_count } => todo!(),
            //InstructionOperands::Out { target, bit_count } => todo!(),
            //InstructionOperands::Jmp { condition, target } => todo!(),
            InstructionOperands::Push { if_full, blocking } => PUSH { if_full, blocking },
            InstructionOperands::Pull { if_empty, block } => PULL { if_empty, block },
            //InstructionOperands::Mov { src, op, trg } => todo!(),
            //InstructionOperands::Irq {
            //    modifier,
            //    value,
            //    relative,
            //} => todo!(),
            //InstructionOperands::Set { target, value } => todo!(),
            _ => todo!(),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WaitSource<'i> {
    Irq(Value<'i>, bool),
    Gpio(Value<'i>),
    Pin(Value<'i>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InSource {
    Pins,
    X,
    Y,
    Null,
    Isr,
    Osr,
    Status,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OutTarget {
    Pins,
    X,
    Y,
    Null,
    PinDirs,
    Isr,
    Pc,
    Exec,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum JmpCondition {
    NotX,
    XPostDec,
    NotY,
    YPostDec,
    XNotEqualY,
    Pin,
    OSRNotEmpty,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SetTarget {
    Pins,
    PinDirs,
    X,
    Y,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MovTarget {
    Pins,
    X,
    Y,
    Exec,
    Pc,
    Isr,
    Osr,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MovSource {
    Pins,
    X,
    Y,
    Null,
    Status,
    Isr,
    Osr,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MovOp {
    Not,
    Reverse,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IrqModifier {
    Clear,
    SetWait,
    Set,
}
