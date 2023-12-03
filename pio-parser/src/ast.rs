use std::{marker::PhantomData, ops::Range};

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
impl Value<'_> {
    pub fn resolve(
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
    pub fn resolve(
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
impl<'i> TryInto<pio::intermediate_repr::InstructionOperands> for InstructionOperands<'i> {
    type Error = crate::compiler::Error<'i>;

    fn try_into(self) -> Result<pio::intermediate_repr::InstructionOperands, Self::Error> {
        use ir::InstructionOperands::*;
        use pio::intermediate_repr as ir;
        Ok(match self {
            InstructionOperands::Nop => MOV {
                destination: ir::MovDestination::Y,
                op: ir::MovOperation::None,
                source: ir::MovSource::Y,
            },
            InstructionOperands::Wait { polarity, src } => {
                use {WaitPolarity::*, WaitSource::*};

                match polarity {
                    Value::Integer(_) => todo!(),
                    Value::Identifier(_, _) => todo!(),
                    Value::Expression(_, _) => todo!(),
                }

                let (source, index, relative) = todo!();
                let polarity = todo!();
                //match src {
                //    WaitSource::Irq(s, r) => (),
                //    WaitSource::Gpio(_) => todo!(),
                //    WaitSource::Pin(_) => todo!(),
                //};
                WAIT {
                    polarity,
                    source,
                    index,
                    relative,
                }
            }
            InstructionOperands::In { src, bit_count } => todo!(),
            InstructionOperands::Out { target, bit_count } => todo!(),
            InstructionOperands::Jmp { condition, target } => todo!(),
            InstructionOperands::Push { if_full, blocking } => todo!(),
            InstructionOperands::Pull { if_empty, blocking } => todo!(),
            InstructionOperands::Mov { src, op, trg } => todo!(),
            InstructionOperands::Irq {
                modifier,
                value,
                relative,
            } => todo!(),
            InstructionOperands::Set { target, value } => todo!(),
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
