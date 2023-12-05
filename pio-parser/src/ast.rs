use core::panic;
use std::{marker::PhantomData, ops::Range};

use crate::compiler::{Defines, Error, Resolve, SymbolId};

fn resolve<'i>(
    ctx: SymbolId<'i>,
    var_name: &'i str,
    defines: &Defines<'i>,
    pending: &mut Vec<&'i str>,
) -> Result<i32, Error<'i>> {
    if pending.contains(&var_name) {
        return Err(Error::CircularDependency(ctx.var_name, var_name));
    }
    pending.push(var_name);

    let (_, v) = defines
        .get(&(ctx.prog_name, var_name).into())
        .or_else(|| defines.get(&("", var_name).into()))
        .ok_or(Error::UndefinedSymbol(ctx.var_name, var_name))?;
    let v = v.resolve(SymbolId { var_name, ..ctx }, defines, pending)?;

    pending.pop();
    Ok(v)
}

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
impl<'i> Resolve<'i> for Value<'i> {
    fn resolve(
        &self,
        ctx: SymbolId<'i>,
        defines: &Defines<'i>,
        pending: &mut Vec<&'i str>,
    ) -> Result<i32, Error<'i>> {
        Ok(match self {
            Value::Integer(v) => *v,
            Value::Identifier(_, name) => resolve(ctx, name, defines, pending)?,
            Value::Expression(_, e) => e.resolve(ctx, defines, pending)?,
        })
    }
}
impl Into<i32> for Value<'_> {
    fn into(self) -> i32 {
        match self {
            Value::Integer(v) => v,
            _ => panic!("Value can only be converted to i32 after being resolved."),
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
impl<'i> Resolve<'i> for Expression<'i> {
    fn resolve(
        &self,
        ctx: SymbolId<'i>,
        defines: &Defines<'i>,
        pending: &mut Vec<&'i str>,
    ) -> Result<i32, Error<'i>> {
        Ok(match self {
            Expression::Value(v) => v.resolve(ctx, defines, pending)?,
            Expression::Plus(l, r) => {
                let l = l.resolve(ctx, defines, pending)?;
                let r = r.resolve(ctx, defines, pending)?;
                l.checked_add(r)
                    .ok_or(Error::IntegerOverflow(ctx.var_name))?
            }
            Expression::Minus(l, r) => {
                let l = l.resolve(ctx, defines, pending)?;
                let r = r.resolve(ctx, defines, pending)?;
                l.checked_sub(r)
                    .ok_or(Error::IntegerOverflow(ctx.var_name))?
            }
            Expression::Multiply(l, r) => {
                let l = l.resolve(ctx, defines, pending)?;
                let r = r.resolve(ctx, defines, pending)?;
                l.checked_mul(r)
                    .ok_or(Error::IntegerOverflow(ctx.var_name))?
            }
            Expression::Divide(l, r) => {
                let l = l.resolve(ctx, defines, pending)?;
                let r = r.resolve(ctx, defines, pending)?;
                l.checked_div(r).ok_or(Error::DivideByZero(ctx.var_name))?
            }
            Expression::Or(l, r) => {
                let l = l.resolve(ctx, defines, pending)?;
                let r = r.resolve(ctx, defines, pending)?;
                l | r
            }
            Expression::And(l, r) => {
                let l = l.resolve(ctx, defines, pending)?;
                let r = r.resolve(ctx, defines, pending)?;
                l & r
            }
            Expression::Xor(l, r) => {
                let l = l.resolve(ctx, defines, pending)?;
                let r = r.resolve(ctx, defines, pending)?;
                l ^ r
            }
            Expression::Opposite(v) => v
                .resolve(ctx, defines, pending)?
                .checked_neg()
                .ok_or(Error::IntegerOverflow(ctx.var_name))?,
            Expression::Reverse(v) => v.resolve(ctx, defines, pending)?.reverse_bits(),
        })
    }
}
impl Into<i32> for Expression<'_> {
    fn into(self) -> i32 {
        match self {
            Expression::Value(v) => v.into(),
            _ => panic!("Expression can only be converted to i32 after being resolved."),
        }
    }
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
impl<'i> Instruction<'i> {
    pub(crate) fn reify(
        &mut self,
        prog_name: &'i str,
        defines: &Defines<'i>,
    ) -> Result<pio::Instruction, Error<'i>> {
        let operands = self.ops.reify(prog_name, defines)?;

        let mut pending = Vec::new();
        let delay = if let Some(delay) = self.delay.take() {
            delay
                .resolve(
                    SymbolId {
                        prog_name,
                        var_name: "delay",
                    },
                    defines,
                    &mut pending,
                )?
                .try_into()
                .map_err(|_| Error::IntegerOutOfRange)?
        } else {
            0
        };
        let side_set = if let Some(side_set) = self.side_set.take() {
            Some(
                side_set
                    .resolve(
                        SymbolId {
                            prog_name,
                            var_name: "side_set",
                        },
                        defines,
                        &mut pending,
                    )?
                    .try_into()
                    .map_err(|_| Error::IntegerOutOfRange)?,
            )
        } else {
            None
        };
        self.ops.reify(prog_name, defines)?;
        Ok(pio::Instruction {
            operands: operands.try_into()?,
            delay,
            side_set,
        })
    }
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
        blocking: bool,
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
impl<'i> InstructionOperands<'i> {
    fn reify(&mut self, prog_name: &'i str, defines: &Defines<'i>) -> Result<Self, Error<'i>> {
        let mut pending = Vec::new();
        Ok(match &self {
            Self::Push { .. } | Self::Pull { .. } | Self::Nop | Self::Mov { .. } => self.clone(),
            Self::Out { bit_count: v, .. }
            | Self::In { bit_count: v, .. }
            | Self::Wait { polarity: v, .. }
            | Self::Irq { value: v, .. }
            | Self::Set { value: v, .. } => {
                *v = Value::Integer(v.resolve(
                    SymbolId {
                        prog_name,
                        var_name: "set",
                    },
                    defines,
                    &mut pending,
                )?);
                todo!()
            }
            Self::Jmp { target: v, condition } => {
                let me = self.clone();
                Self::Jmp {
                    target: Expression::Value(Value::Integer(v.resolve(
                        SymbolId {
                            prog_name,
                            var_name: "jmp",
                        },
                        defines,
                        &mut pending,
                    )?)),
                    condition: *condition
                }
            }
        })
    }
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
            InstructionOperands::In { src, bit_count } => IN {
                source: src.into(),
                bit_count: match bit_count.into() {
                    b @ 0..=31 => b as u8,
                    _ => return Err(Error::IntegerOverflow("in bit_count must be in [0; 31].")),
                },
            },
            InstructionOperands::Out { target, bit_count } => OUT {
                destination: target.into(),
                bit_count: match bit_count.into() {
                    b @ 0..=31 => b as u8,
                    _ => return Err(Error::IntegerOverflow("out bit_count must be in [0; 31].")),
                },
            },
            InstructionOperands::Jmp { condition, target } => JMP {
                condition: condition
                    .map(Into::into)
                    .unwrap_or(pio::intermediate_repr::JmpCondition::Always),
                address: match target.into() {
                    v @ 0..=31 => v as u8,
                    _ => return Err(Error::IntegerOverflow("jmp address must be in [0; 31].")),
                },
            },
            InstructionOperands::Push { if_full, blocking } => PUSH { if_full, blocking },
            InstructionOperands::Pull { if_empty, blocking } => PULL { if_empty, blocking },
            InstructionOperands::Mov { src, op, trg } => MOV {
                destination: trg.into(),
                op: op
                    .map(Into::into)
                    .unwrap_or(pio::intermediate_repr::MovOperation::None),
                source: src.into(),
            },
            InstructionOperands::Irq {
                modifier,
                value,
                relative,
            } => IRQ {
                clear: matches!(modifier, IrqModifier::Clear),
                wait: matches!(modifier, IrqModifier::SetWait),
                index: match value.into() {
                    v @ 0..=7 => v as u8,
                    _ => return Err(Error::IntegerOverflow("irq index must be in [0; 7].")),
                },
                relative,
            },
            InstructionOperands::Set { target, value } => SET {
                destination: target.into(),
                data: match value.into() {
                    v @ 0..=31 => v as u8,
                    _ => return Err(Error::IntegerOverflow("set value must be in [0; 31].")),
                },
            },
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
}
impl Into<pio::intermediate_repr::InSource> for InSource {
    fn into(self) -> pio::intermediate_repr::InSource {
        use pio::intermediate_repr::InSource::*;
        match self {
            InSource::Pins => PINS,
            InSource::X => X,
            InSource::Y => Y,
            InSource::Null => NULL,
            InSource::Isr => ISR,
            InSource::Osr => OSR,
        }
    }
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
impl Into<pio::intermediate_repr::OutDestination> for OutTarget {
    fn into(self) -> pio::intermediate_repr::OutDestination {
        use pio::intermediate_repr::OutDestination::*;
        match self {
            OutTarget::Pins => PINS,
            OutTarget::X => X,
            OutTarget::Y => Y,
            OutTarget::Null => NULL,
            OutTarget::PinDirs => PINDIRS,
            OutTarget::Isr => ISR,
            OutTarget::Pc => PC,
            OutTarget::Exec => EXEC,
        }
    }
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
impl Into<pio::intermediate_repr::JmpCondition> for JmpCondition {
    fn into(self) -> pio::intermediate_repr::JmpCondition {
        use pio::intermediate_repr::JmpCondition::*;
        match self {
            JmpCondition::NotX => XIsZero,
            JmpCondition::XPostDec => XDecNonZero,
            JmpCondition::NotY => YIsZero,
            JmpCondition::YPostDec => YDecNonZero,
            JmpCondition::XNotEqualY => XNotEqualY,
            JmpCondition::Pin => PinHigh,
            JmpCondition::OSRNotEmpty => OutputShiftRegisterNotEmpty,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SetTarget {
    Pins,
    PinDirs,
    X,
    Y,
}
impl Into<pio::intermediate_repr::SetDestination> for SetTarget {
    fn into(self) -> pio::intermediate_repr::SetDestination {
        use pio::intermediate_repr::SetDestination::*;
        match self {
            SetTarget::Pins => PINS,
            SetTarget::PinDirs => PINDIRS,
            SetTarget::X => X,
            SetTarget::Y => Y,
        }
    }
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
impl Into<pio::intermediate_repr::MovDestination> for MovTarget {
    fn into(self) -> pio::intermediate_repr::MovDestination {
        use pio::intermediate_repr::MovDestination::*;
        match self {
            MovTarget::Pins => PINS,
            MovTarget::X => X,
            MovTarget::Y => Y,
            MovTarget::Exec => EXEC,
            MovTarget::Pc => PC,
            MovTarget::Isr => ISR,
            MovTarget::Osr => OSR,
        }
    }
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
impl Into<pio::intermediate_repr::MovSource> for MovSource {
    fn into(self) -> pio::intermediate_repr::MovSource {
        use pio::intermediate_repr::MovSource::*;
        match self {
            MovSource::Pins => PINS,
            MovSource::X => X,
            MovSource::Y => Y,
            MovSource::Null => NULL,
            MovSource::Status => STATUS,
            MovSource::Isr => ISR,
            MovSource::Osr => OSR,
        }
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MovOp {
    Not,
    Reverse,
}
impl Into<pio::intermediate_repr::MovOperation> for MovOp {
    fn into(self) -> pio::intermediate_repr::MovOperation {
        use pio::intermediate_repr::MovOperation::*;
        match self {
            MovOp::Not => Invert,
            MovOp::Reverse => BitReverse,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IrqModifier {
    Clear,
    SetWait,
    Set,
}
