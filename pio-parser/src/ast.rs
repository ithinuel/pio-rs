
use std::ops::Range;

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
    Identifier(&'i str, Location),
    Expression(Box<Expression<'i>>),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Instruction<'i> {
    pub ops: InstructionOps<'i>,
    pub delay: Option<Expression<'i>>,
    pub side_set: Option<Value<'i>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InstructionOps<'i> {
    Nop,
    Wait {
        duration: Value<'i>,
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
