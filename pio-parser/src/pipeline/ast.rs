use std::ops::Range;

use pio_core as ir;

use crate::pipeline::compiler::{Defines, Error, Resolve, SymbolId};

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
impl From<Value<'_>> for i32 {
    fn from(value: Value<'_>) -> Self {
        match value {
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
impl From<Expression<'_>> for i32 {
    fn from(value: Expression<'_>) -> Self {
        match value {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instruction<'i> {
    pub ops: InstructionOperands<'i>,
    pub delay: Option<Expression<'i>>,
    pub side_set: Option<Value<'i>>,
}
impl<'i> Instruction<'i> {
    /// Resolves every symbol used in the instruction, replacing it by its
    /// value. After reification, all the values of the instruction are
    /// [`Value::Integer`] and can be converted into their machine
    /// representation.
    pub(crate) fn reify(
        &mut self,
        prog_name: &'i str,
        defines: &Defines<'i>,
    ) -> Result<(), Error<'i>> {
        self.ops.reify(prog_name, defines)?;

        let mut pending = Vec::new();
        if let Some(delay) = self.delay.take() {
            let delay = delay.resolve(
                SymbolId {
                    prog_name,
                    var_name: "delay",
                },
                defines,
                &mut pending,
            )?;
            self.delay = Some(Expression::Value(Value::Integer(delay)));
        }
        if let Some(side_set) = self.side_set.take() {
            let side_set = side_set.resolve(
                SymbolId {
                    prog_name,
                    var_name: "side_set",
                },
                defines,
                &mut pending,
            )?;
            self.side_set = Some(Value::Integer(side_set));
        }
        Ok(())
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
        index_mode: IrqIndexMode,
    },
    Set {
        target: SetTarget,
        value: Value<'i>,
    },
}
impl<'i> InstructionOperands<'i> {
    /// Resolves every symbol used by the operands, replacing it by its value.
    fn reify(&mut self, prog_name: &'i str, defines: &Defines<'i>) -> Result<(), Error<'i>> {
        let mut pending = Vec::new();
        let mut resolve_value =
            |var_name: &'static str, v: &mut Value<'i>| -> Result<(), Error<'i>> {
                let value = v.resolve(
                    SymbolId {
                        prog_name,
                        var_name,
                    },
                    defines,
                    &mut pending,
                )?;
                *v = Value::Integer(value);
                Ok(())
            };

        match self {
            Self::Push { .. } | Self::Pull { .. } | Self::Nop | Self::Mov { .. } => {}
            Self::Out { bit_count, .. } => resolve_value("out", bit_count)?,
            Self::In { bit_count, .. } => resolve_value("in", bit_count)?,
            Self::Wait { polarity, src } => {
                resolve_value("wait", polarity)?;
                match src {
                    WaitSource::Irq(v, _) | WaitSource::Gpio(v) | WaitSource::Pin(v) => {
                        resolve_value("wait", v)?
                    }
                    WaitSource::JmpPin(Some(v)) => resolve_value("wait", v)?,
                    WaitSource::JmpPin(None) => {}
                }
            }
            Self::Irq { value, .. } => resolve_value("irq", value)?,
            Self::Set { value, .. } => resolve_value("set", value)?,
            Self::Jmp { target, .. } => {
                let target_value = target.resolve(
                    SymbolId {
                        prog_name,
                        var_name: "jmp",
                    },
                    defines,
                    &mut pending,
                )?;
                *target = Expression::Value(Value::Integer(target_value));
            }
        }
        Ok(())
    }
}
impl<'i> TryFrom<InstructionOperands<'i>> for ir::InstructionOperands {
    type Error = Error<'i>;

    fn try_from(operands: InstructionOperands<'i>) -> Result<ir::InstructionOperands, Self::Error> {
        use InstructionOperands::*;

        let bit_count_to_u8 = |v: Value<'i>| -> Result<u8, Error<'i>> {
            match v.into() {
                // bit_count 32 is encoded as 0.
                b @ 1..=32 => Ok(b as u8),
                _ => Err(Error::IntegerOverflow("bit_count must be in [1; 32].")),
            }
        };

        Ok(match operands {
            Nop => ir::InstructionOperands::MOV {
                destination: ir::MovDestination::Y,
                op: ir::MovOperation::None,
                source: ir::MovSource::Y,
            },
            Wait { polarity, src } => {
                let polarity = match polarity.into() {
                    0 => 0,
                    1 => 1,
                    _ => {
                        return Err(Error::IntegerOverflow(
                            "wait polarity must be either 0 or 1",
                        ))
                    }
                };

                let pin_or_gpio = |index: i32, src: fn(u8) -> ir::WaitSource| match index {
                    i @ 0..=31 => Ok(src(i as u8)),
                    _ => Err(Error::IntegerOverflow(
                        "wait gpio/pin index must be in [0; 31].",
                    )),
                };

                let source = match src {
                    WaitSource::Irq(s, index_mode) => match s.into() {
                        i @ 0..=7 => ir::WaitSource::Irq {
                            index_mode: index_mode.into(),
                            irq: i as u8,
                        },
                        _ => {
                            return Err(Error::IntegerOverflow("wait irq index must be in [0; 7]."))
                        }
                    },
                    WaitSource::Gpio(index) => pin_or_gpio(index.into(), ir::WaitSource::Gpio)?,
                    WaitSource::Pin(index) => pin_or_gpio(index.into(), ir::WaitSource::Pin)?,
                    WaitSource::JmpPin(offset) => ir::WaitSource::JmpPin {
                        offset: match offset {
                            Some(offset) => match offset.into() {
                                // The RP2350 datasheet (Section 11.4.3)
                                // reserves all jmppin offsets beyond 3.
                                o @ 0..=3 => Some(o as u8),
                                _ => {
                                    return Err(Error::IntegerOverflow(
                                        "jmppin offset must be in [0; 3].",
                                    ))
                                }
                            },
                            None => None,
                        },
                    },
                };
                ir::InstructionOperands::WAIT { polarity, source }
            }
            In { src, bit_count } => ir::InstructionOperands::IN {
                source: src.into(),
                bit_count: bit_count_to_u8(bit_count)?,
            },
            Out { target, bit_count } => ir::InstructionOperands::OUT {
                destination: target.into(),
                bit_count: bit_count_to_u8(bit_count)?,
            },
            Jmp { condition, target } => ir::InstructionOperands::JMP {
                condition: condition
                    .map(Into::into)
                    .unwrap_or(ir::JmpCondition::Always),
                address: match target.into() {
                    v @ 0..=31 => v as u8,
                    _ => return Err(Error::IntegerOverflow("jmp address must be in [0; 31].")),
                },
            },
            Push { if_full, blocking } => ir::InstructionOperands::PUSH {
                if_full,
                block: blocking,
            },
            Pull { if_empty, blocking } => ir::InstructionOperands::PULL {
                if_empty,
                block: blocking,
            },
            Mov { src, op, trg } => match (trg, src) {
                (MovTarget::Rxfifo(index), MovSource::Isr) => ir::InstructionOperands::MOVTORX {
                    fifo_index: index.into(),
                },
                (MovTarget::Osr, MovSource::Rxfifo(index)) => ir::InstructionOperands::MOVFROMRX {
                    fifo_index: index.into(),
                },
                (MovTarget::Rxfifo(_), _) | (_, MovSource::Rxfifo(_)) => {
                    return Err(Error::InvalidMovFifo)
                }
                (trg, src) => ir::InstructionOperands::MOV {
                    destination: trg.into(),
                    op: op.map(Into::into).unwrap_or(ir::MovOperation::None),
                    source: src.into(),
                },
            },
            Irq {
                modifier,
                value,
                index_mode,
            } => ir::InstructionOperands::IRQ {
                clear: matches!(modifier, IrqModifier::Clear),
                wait: matches!(modifier, IrqModifier::SetWait),
                index: match value.into() {
                    v @ 0..=7 => v as u8,
                    _ => return Err(Error::IntegerOverflow("irq index must be in [0; 7].")),
                },
                index_mode: index_mode.into(),
            },
            Set { target, value } => ir::InstructionOperands::SET {
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
    Irq(Value<'i>, IrqIndexMode),
    Gpio(Value<'i>),
    Pin(Value<'i>),
    JmpPin(Option<Value<'i>>),
}

/// Index mode of `irq` and `wait .. irq` instructions.
///
/// `Prev` and `Next` are only available on the RP2350.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IrqIndexMode {
    Direct,
    Prev,
    Rel,
    Next,
}
impl From<IrqIndexMode> for ir::IrqIndexMode {
    fn from(mode: IrqIndexMode) -> ir::IrqIndexMode {
        match mode {
            IrqIndexMode::Direct => ir::IrqIndexMode::DIRECT,
            IrqIndexMode::Prev => ir::IrqIndexMode::PREV,
            IrqIndexMode::Rel => ir::IrqIndexMode::REL,
            IrqIndexMode::Next => ir::IrqIndexMode::NEXT,
        }
    }
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
impl From<InSource> for ir::InSource {
    fn from(src: InSource) -> ir::InSource {
        match src {
            InSource::Pins => ir::InSource::PINS,
            InSource::X => ir::InSource::X,
            InSource::Y => ir::InSource::Y,
            InSource::Null => ir::InSource::NULL,
            InSource::Isr => ir::InSource::ISR,
            InSource::Osr => ir::InSource::OSR,
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
impl From<OutTarget> for ir::OutDestination {
    fn from(target: OutTarget) -> ir::OutDestination {
        match target {
            OutTarget::Pins => ir::OutDestination::PINS,
            OutTarget::X => ir::OutDestination::X,
            OutTarget::Y => ir::OutDestination::Y,
            OutTarget::Null => ir::OutDestination::NULL,
            OutTarget::PinDirs => ir::OutDestination::PINDIRS,
            OutTarget::Isr => ir::OutDestination::ISR,
            OutTarget::Pc => ir::OutDestination::PC,
            OutTarget::Exec => ir::OutDestination::EXEC,
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
impl From<JmpCondition> for ir::JmpCondition {
    fn from(condition: JmpCondition) -> ir::JmpCondition {
        match condition {
            JmpCondition::NotX => ir::JmpCondition::XIsZero,
            JmpCondition::XPostDec => ir::JmpCondition::XDecNonZero,
            JmpCondition::NotY => ir::JmpCondition::YIsZero,
            JmpCondition::YPostDec => ir::JmpCondition::YDecNonZero,
            JmpCondition::XNotEqualY => ir::JmpCondition::XNotEqualY,
            JmpCondition::Pin => ir::JmpCondition::PinHigh,
            JmpCondition::OSRNotEmpty => ir::JmpCondition::OutputShiftRegisterNotEmpty,
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
impl From<SetTarget> for ir::SetDestination {
    fn from(target: SetTarget) -> ir::SetDestination {
        match target {
            SetTarget::Pins => ir::SetDestination::PINS,
            SetTarget::PinDirs => ir::SetDestination::PINDIRS,
            SetTarget::X => ir::SetDestination::X,
            SetTarget::Y => ir::SetDestination::Y,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MovTarget {
    Pins,
    X,
    Y,
    Pindirs,
    Exec,
    Pc,
    Isr,
    Osr,
    Rxfifo(RxFifoIndex),
}
impl From<MovTarget> for ir::MovDestination {
    fn from(target: MovTarget) -> ir::MovDestination {
        match target {
            MovTarget::Pins => ir::MovDestination::PINS,
            MovTarget::X => ir::MovDestination::X,
            MovTarget::Y => ir::MovDestination::Y,
            MovTarget::Pindirs => ir::MovDestination::PINDIRS,
            MovTarget::Exec => ir::MovDestination::EXEC,
            MovTarget::Pc => ir::MovDestination::PC,
            MovTarget::Isr => ir::MovDestination::ISR,
            MovTarget::Osr => ir::MovDestination::OSR,
            // rxfifo combinations are resolved when converting the operands.
            MovTarget::Rxfifo(_) => unreachable!(),
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
    Rxfifo(RxFifoIndex),
}
impl From<MovSource> for ir::MovSource {
    fn from(src: MovSource) -> ir::MovSource {
        match src {
            MovSource::Pins => ir::MovSource::PINS,
            MovSource::X => ir::MovSource::X,
            MovSource::Y => ir::MovSource::Y,
            MovSource::Null => ir::MovSource::NULL,
            MovSource::Status => ir::MovSource::STATUS,
            MovSource::Isr => ir::MovSource::ISR,
            MovSource::Osr => ir::MovSource::OSR,
            // rxfifo combinations are resolved when converting the operands.
            MovSource::Rxfifo(_) => unreachable!(),
        }
    }
}

/// Index of the receive fifo for the `mov` instructions exclusive to the
/// RP2350.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RxFifoIndex {
    Y,
    _0,
    _1,
    _2,
    _3,
}
impl From<RxFifoIndex> for ir::MovRxIndex {
    fn from(index: RxFifoIndex) -> ir::MovRxIndex {
        match index {
            RxFifoIndex::Y => ir::MovRxIndex::RXFIFOY,
            RxFifoIndex::_0 => ir::MovRxIndex::RXFIFO0,
            RxFifoIndex::_1 => ir::MovRxIndex::RXFIFO1,
            RxFifoIndex::_2 => ir::MovRxIndex::RXFIFO2,
            RxFifoIndex::_3 => ir::MovRxIndex::RXFIFO3,
        }
    }
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum MovOp {
    Not,
    Reverse,
}
impl From<MovOp> for ir::MovOperation {
    fn from(op: MovOp) -> ir::MovOperation {
        match op {
            MovOp::Not => ir::MovOperation::Invert,
            MovOp::Reverse => ir::MovOperation::BitReverse,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IrqModifier {
    Clear,
    SetWait,
    Set,
}
