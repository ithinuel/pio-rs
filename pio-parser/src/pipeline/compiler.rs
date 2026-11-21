use std::collections::{hash_map::Entry, HashMap};

use super::*;
use arrayvec::ArrayVec;
use itertools::Itertools;

#[derive(thiserror::Error, Debug, PartialEq, Eq)]
pub enum Error<'i> {
    #[error("A program with the same name was already defined here")]
    ProgramAlreadyExists(Location),
    #[error("`{origin}` is only valid inside a program")]
    InvalidOutsideOfProgram {
        location: Location,
        origin: &'static str,
    },
    #[error("`.wrap` was already set here")]
    WrapAlreadySet(Location),
    #[error("`.wrap` cannot be placed before the first program instruction")]
    WrapBeforeFirstInstr(Location),
    #[error("`.wrap_target` was already set here")]
    WrapTargetAlreadySet(Location),
    #[error("`{origin}` must appear before any instruction in a program")]
    MustAppearBeforeAnyInstr {
        location: Location,
        origin: &'static str,
    },
    #[error("`{name}` was already as a {}", if *old_is_label { "label" } else { "define" })]
    AlreadyDefined {
        location: Location,
        name: &'i str,
        old_is_label: bool,
        new_is_label: bool,
    },
    #[error("Illegal division by 0 detected when computing `{0}`")]
    DivideByZero(&'i str),
    #[error("Integer overflow detected when computing `{0}`")]
    IntegerOverflow(&'i str),
    #[error("Circular dependency to `{1}` detected while computing `{0}`")]
    CircularDependency(&'i str, &'i str),
    #[error("Undefined `{1}` symbol used while computing `{0}`")]
    UndefinedSymbol(&'i str, &'i str),
    #[error("Integer out of range for type")]
    IntegerOutOfRange,
    #[error(
        "`rxfifo[..]` is only valid in `mov rxfifo[..], isr` and `mov osr, rxfifo[..]` instructions"
    )]
    InvalidMovFifo,
    #[error("`.wrap` cannot be used without a preceding `.wrap_target`")]
    WrapWithoutWrapTarget(Location),
    #[error("`.wrap_target` cannot be used without a following `.wrap`")]
    WrapTargetWithoutWrap(Location),
    #[error("A program must contain at least one instruction")]
    EmptyProgram,
    #[error("Program contains {count} instructions but at most {max} are allowed")]
    ProgramTooLong { count: usize, max: usize },
    #[error("`.pio_version` must be 0 (RP2040) or 1")]
    InvalidPioVersion(Location),
    #[error("`{feature}` is not supported on PIO version 0 (RP2040)")]
    FeatureNotSupportedOnPioVersion0 {
        location: Location,
        feature: &'static str,
    },
    #[error("The program uses instructions exclusive to the RP2350 but targets PIO version 0")]
    InstructionsRequirePioVersion1(Location),
}
impl<'i> Error<'i> {
    /// Returns the byte range of the input the error refers to, when known.
    pub fn span(&self) -> Option<std::ops::Range<usize>> {
        let location = match self {
            Error::ProgramAlreadyExists(location) => location,
            Error::InvalidOutsideOfProgram { location, .. } => location,
            Error::WrapAlreadySet(location) => location,
            Error::WrapBeforeFirstInstr(location) => location,
            Error::WrapTargetAlreadySet(location) => location,
            Error::MustAppearBeforeAnyInstr { location, .. } => location,
            Error::AlreadyDefined { location, .. } => location,
            Error::WrapWithoutWrapTarget(location) => location,
            Error::WrapTargetWithoutWrap(location) => location,
            Error::InvalidPioVersion(location) => location,
            Error::FeatureNotSupportedOnPioVersion0 { location, .. } => location,
            Error::InstructionsRequirePioVersion1(location) => location,
            _ => return None,
        };
        Some(location.0.clone())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SymbolId<'i> {
    pub prog_name: &'i str,
    pub var_name: &'i str,
}
impl<'i> From<(&'i str, &'i str)> for SymbolId<'i> {
    fn from((prog_name, var_name): (&'i str, &'i str)) -> Self {
        SymbolId {
            prog_name,
            var_name,
        }
    }
}
/// Key: (program, name)
/// Val: (name, value)
///
/// empty program name is the special name for global scope.
pub(crate) type Defines<'i> = HashMap<SymbolId<'i>, (SymbolDef<'i>, Expression<'i>)>;
pub(crate) trait Resolve<'i> {
    fn resolve(
        &self,
        ctx: SymbolId<'i>,
        defines: &Defines<'i>,
        pending: &mut Vec<&'i str>,
    ) -> Result<i32, Error<'i>>;
}

/// SideSet descriptor.
#[derive(Debug, Clone)]
pub struct SideSet<'i> {
    /// If true, side set on instructions maybe omitted.
    pub optional: bool,
    /// If true, side set controls pin direction rather than level.
    pub pindirs: bool,
    pub value: Value<'i>,
}

#[derive(Debug, Clone)]
pub enum InstrOrWord<'i> {
    Instruction(Instruction<'i>),
    Word(Value<'i>),
}
impl<'i> InstrOrWord<'i> {
    pub fn instruction(&self) -> Option<&Instruction<'i>> {
        if let Self::Instruction(i) = self {
            Some(i)
        } else {
            None
        }
    }
}

/// Shift register configuration written with the `.in` and `.out`
/// directives.
#[derive(Debug, Clone)]
pub struct ShiftConfig<'i> {
    /// Number of bits shifted by each `in`/`out` instruction.
    pub count: Value<'i>,
    /// ISR/OSR shift direction.
    pub direction: ShiftDirection,
    /// Whether auto-push (`.in`) or auto-pull (`.out`) is enabled.
    pub auto: bool,
    /// Auto-push/auto-pull threshold.
    pub threshold: Value<'i>,
    pub location: Location,
}

#[derive(Debug, Clone, Default)]
pub struct ParsedProgram<'i> {
    pub side_set: Option<SideSet<'i>>,
    /// offset in program and where is wrap is set.
    pub wrap: Option<(usize, Location)>,
    pub wrap_target: Option<(usize, Location)>,
    pub origin: Option<(Value<'i>, Location)>,
    pub lang_opt: HashMap<&'i str, HashMap<&'i str, &'i str>>,
    pub instr: Vec<InstrOrWord<'i>>,
    /// Target hardware version written with `.pio_version`. When
    /// the program does not declare one, the file-level default written
    /// before the first program is applied instead. Without either, the
    /// program targets the latest version.
    pub pio_version: Option<(pio_core::PioVersion, Location)>,
    /// Clock divider written with `.clock_div`.
    pub clock_div: Option<(ClockDiv, Location)>,
    /// FIFO configuration written with `.fifo`.
    pub fifo: Option<(FifoConfig, Location)>,
    /// `mov status` source written with `.mov_status`.
    pub mov_status: Option<(MovStatus<'i>, Location)>,
    /// Shift configuration written with `.in`.
    pub in_config: Option<ShiftConfig<'i>>,
    /// Shift configuration written with `.out`.
    pub out_config: Option<ShiftConfig<'i>>,
    /// Number of SET bits written with `.set`.
    pub set_count: Option<(Value<'i>, Location)>,
}

#[derive(Debug, Clone)]
pub struct CodeBlock<'i> {
    pub program: &'i str,
    pub lang: &'i str,
    pub block: &'i str,
}

#[derive(Debug, Clone)]
pub struct File<'i, T: 'i> {
    pub programs: HashMap<&'i str, T>,
    pub code_blocks: Vec<CodeBlock<'i>>,
    pub defines: HashMap<SymbolId<'i>, (bool, i32)>,
}

/// A program encoded into pio machine code.
pub struct AssembledProgram<'i, const PROGRAM_SIZE: usize> {
    pub program: pio_core::ProgramWithDefines<HashMap<String, i32>, PROGRAM_SIZE>,
    pub lang_opt: HashMap<&'i str, HashMap<&'i str, &'i str>>,
}

#[derive(Debug)]
pub struct Compiler<'i> {
    current_program: &'i str,
    defines: Defines<'i>,
    programs: HashMap<&'i str, (Location, ParsedProgram<'i>)>,
    /// program, lang, block
    code_blocks: Vec<CodeBlock<'i>>,
    /// `.pio_version` written before the first program; the default target
    /// for every program of the file.
    default_pio_version: Option<(pio_core::PioVersion, Location)>,
}
impl<'i> Compiler<'i> {
    fn get_current_program(
        &mut self,
        l: Location,
        source: &'static str,
    ) -> Result<&mut ParsedProgram<'i>, Error<'i>> {
        match self.programs.get_mut(self.current_program) {
            Some((_, p)) => Ok(p),
            None => Err(Error::InvalidOutsideOfProgram {
                location: l,
                origin: source,
            }),
        }
    }

    /// Checks that the program has no instruction yet, as required by the
    /// directives configuring the state machine.
    fn before_first_instruction(
        p: &ParsedProgram<'i>,
        location: Location,
        origin: &'static str,
    ) -> Result<(), Error<'i>> {
        if !p.instr.is_empty() {
            return Err(Error::MustAppearBeforeAnyInstr { location, origin });
        }
        Ok(())
    }

    fn add_symbol(
        &mut self,
        symbol: SymbolDef<'i>,
        is_label: bool,
        expr: Expression<'i>,
    ) -> Result<(), Error<'i>> {
        // search for possible conflict
        let entry = match self.defines.entry(("", symbol.name).into()) {
            // no need for a second search if we're still in the global context
            entry if self.current_program.is_empty() => entry,
            entry @ Entry::Occupied(_) => entry,
            _ => self
                .defines
                .entry((self.current_program, symbol.name).into()),
        };
        match entry {
            Entry::Occupied(entry) => {
                let entry = entry.get();
                return Err(Error::AlreadyDefined {
                    location: entry.0.location.clone(),
                    name: symbol.name,
                    old_is_label: entry.0.is_label,
                    new_is_label: is_label,
                });
            }
            Entry::Vacant(entry) => {
                entry.insert((symbol, expr));
            }
        }
        Ok(())
    }

    /// collects defines and assembles programs
    fn collate(v: impl Iterator<Item = Line<'i>> + 'i) -> Result<Self, Error<'i>> {
        let mut me = Self {
            current_program: "",
            defines: HashMap::new(),
            programs: HashMap::new(),
            code_blocks: Vec::new(),
            default_pio_version: None,
        };

        for line in v {
            match line {
                Line::Program { name, location } => match me.programs.entry(name) {
                    Entry::Occupied(entry) => {
                        return Err(Error::ProgramAlreadyExists(entry.get().0.clone()))
                    }
                    Entry::Vacant(entry) => {
                        me.current_program = name;
                        entry.insert((location, ParsedProgram::default()));
                    }
                },
                Line::Directive(Directive::Wrap(l)) => {
                    let p = me.get_current_program(l.clone(), ".wrap")?;

                    if p.wrap.is_some() {
                        return Err(Error::WrapAlreadySet(l));
                    }
                    if p.instr.is_empty() {
                        return Err(Error::WrapBeforeFirstInstr(l));
                    }
                    p.wrap = Some((p.instr.len(), l));
                }
                Line::Directive(Directive::WrapTarget(l)) => {
                    let p = me.get_current_program(l.clone(), ".wrap_target")?;

                    if p.wrap_target.is_some() {
                        return Err(Error::WrapTargetAlreadySet(l));
                    }
                    p.wrap_target = Some((p.instr.len(), l));
                }
                Line::Directive(Directive::Origin(l, v)) => {
                    let p = me.get_current_program(l.clone(), ".origin")?;

                    if !p.instr.is_empty() {
                        return Err(Error::MustAppearBeforeAnyInstr {
                            location: l,
                            origin: ".origin",
                        });
                    }

                    p.origin = Some((v, l))
                }
                Line::Directive(Directive::Define(s, v)) => {
                    me.add_symbol(s, false, v)?;
                }
                Line::Directive(Directive::SideSet {
                    location: l,
                    value,
                    optional,
                    pindirs,
                }) => {
                    let p = me.get_current_program(l.clone(), ".side_set")?;

                    if !p.instr.is_empty() {
                        return Err(Error::MustAppearBeforeAnyInstr {
                            location: l,
                            origin: ".side_set",
                        });
                    }

                    p.side_set = Some(SideSet {
                        optional,
                        pindirs,
                        value,
                    });
                }
                Line::Directive(Directive::LangOpt {
                    location: l,
                    lang,
                    var,
                    val,
                }) => {
                    me.get_current_program(l, ".lang_opt")?
                        .lang_opt
                        .entry(lang)
                        .or_default()
                        .insert(var, val);
                }
                Line::Directive(Directive::Word(l, v)) => {
                    me.get_current_program(l.clone(), ".word")?
                        .instr
                        .push(InstrOrWord::Word(v));
                }
                Line::Directive(Directive::PioVersion(l, v)) => {
                    let version = match v {
                        0 => pio_core::PioVersion::V0,
                        1 => pio_core::PioVersion::V1,
                        _ => return Err(Error::InvalidPioVersion(l)),
                    };
                    match me.programs.get_mut(me.current_program) {
                        Some((_, p)) => {
                            Self::before_first_instruction(p, l.clone(), ".pio_version")?;
                            p.pio_version = Some((version, l));
                        }
                        // Before the first program: the default for the
                        // whole file.
                        None => me.default_pio_version = Some((version, l)),
                    }
                }
                Line::Directive(Directive::ClockDiv(l, clock_div)) => {
                    let p = me.get_current_program(l.clone(), ".clock_div")?;
                    Self::before_first_instruction(p, l.clone(), ".clock_div")?;
                    p.clock_div = Some((clock_div, l));
                }
                Line::Directive(Directive::Fifo(l, config)) => {
                    let p = me.get_current_program(l.clone(), ".fifo")?;
                    Self::before_first_instruction(p, l.clone(), ".fifo")?;
                    p.fifo = Some((config, l));
                }
                Line::Directive(Directive::MovStatus(l, source)) => {
                    let p = me.get_current_program(l.clone(), ".mov_status")?;
                    Self::before_first_instruction(p, l.clone(), ".mov_status")?;
                    p.mov_status = Some((source, l));
                }
                Line::Directive(Directive::In {
                    location,
                    count,
                    direction,
                    auto,
                    threshold,
                }) => {
                    let p = me.get_current_program(location.clone(), ".in")?;
                    Self::before_first_instruction(p, location.clone(), ".in")?;
                    p.in_config = Some(ShiftConfig {
                        count,
                        direction,
                        auto,
                        threshold,
                        location,
                    });
                }
                Line::Directive(Directive::Out {
                    location,
                    count,
                    direction,
                    auto,
                    threshold,
                }) => {
                    let p = me.get_current_program(location.clone(), ".out")?;
                    Self::before_first_instruction(p, location.clone(), ".out")?;
                    p.out_config = Some(ShiftConfig {
                        count,
                        direction,
                        auto,
                        threshold,
                        location,
                    });
                }
                Line::Directive(Directive::SetCount(l, value)) => {
                    let p = me.get_current_program(l.clone(), ".set")?;
                    Self::before_first_instruction(p, l.clone(), ".set")?;
                    p.set_count = Some((value, l));
                }
                Line::Label(symbol) => {
                    let target = me
                        .get_current_program(symbol.location.clone(), "label")?
                        .instr
                        .len();
                    me.add_symbol(
                        symbol,
                        true,
                        Expression::Value(Value::Integer(target as i32)),
                    )?;
                }
                Line::Instruction(l, i) => {
                    me.get_current_program(l, "instr")?
                        .instr
                        .push(InstrOrWord::Instruction(i));
                }
                Line::LabelAndInstr(symbol, i) => {
                    let p = me.get_current_program(symbol.location.clone(), "label")?;

                    let target = p.instr.len();
                    p.instr.push(InstrOrWord::Instruction(i));
                    me.add_symbol(
                        symbol,
                        true,
                        Expression::Value(Value::Integer(target as i32)),
                    )?;
                }
                Line::CodeBlock(lang, block) => {
                    me.code_blocks.push(CodeBlock {
                        program: me.current_program,
                        lang,
                        block,
                    });
                }
            }
        }
        Ok(me)
    }

    /// Reifies instructions: replace the symbols used by their resolved
    /// values.
    fn reify(mut self) -> Result<Self, Error<'i>> {
        // define pending here so that it's capacity gets recycled in each sub loop
        let mut pending = Vec::new();

        for (prog_name, (_, program)) in self.programs.iter_mut() {
            if let Some((origin, _)) = &mut program.origin {
                let value = origin.resolve(
                    SymbolId {
                        prog_name,
                        var_name: ".origin",
                    },
                    &self.defines,
                    &mut pending,
                )?;
                *origin = Value::Integer(value);
            }
            if let Some(side_set) = &mut program.side_set {
                let value = side_set.value.resolve(
                    SymbolId {
                        prog_name,
                        var_name: ".side_set",
                    },
                    &self.defines,
                    &mut pending,
                )?;
                side_set.value = Value::Integer(value);
            }
            if let Some(shift) = &mut program.in_config {
                shift.count = Value::Integer(shift.count.resolve(
                    SymbolId {
                        prog_name,
                        var_name: ".in",
                    },
                    &self.defines,
                    &mut pending,
                )?);
                shift.threshold = Value::Integer(shift.threshold.resolve(
                    SymbolId {
                        prog_name,
                        var_name: ".in",
                    },
                    &self.defines,
                    &mut pending,
                )?);
            }
            if let Some(shift) = &mut program.out_config {
                shift.count = Value::Integer(shift.count.resolve(
                    SymbolId {
                        prog_name,
                        var_name: ".out",
                    },
                    &self.defines,
                    &mut pending,
                )?);
                shift.threshold = Value::Integer(shift.threshold.resolve(
                    SymbolId {
                        prog_name,
                        var_name: ".out",
                    },
                    &self.defines,
                    &mut pending,
                )?);
            }
            if let Some((count, _)) = &mut program.set_count {
                let value = count.resolve(
                    SymbolId {
                        prog_name,
                        var_name: ".set",
                    },
                    &self.defines,
                    &mut pending,
                )?;
                *count = Value::Integer(value);
            }
            if let Some((status, _)) = &mut program.mov_status {
                let value = match status {
                    MovStatus::TxFifoBelow(v) | MovStatus::RxFifoBelow(v) => v,
                    MovStatus::IrqSet { index, .. } => index,
                };
                let value = value.resolve(
                    SymbolId {
                        prog_name,
                        var_name: ".mov_status",
                    },
                    &self.defines,
                    &mut pending,
                )?;
                match status {
                    MovStatus::TxFifoBelow(v) | MovStatus::RxFifoBelow(v) => {
                        *v = Value::Integer(value)
                    }
                    MovStatus::IrqSet { index, .. } => *index = Value::Integer(value),
                }
            }

            for instr in program.instr.iter_mut() {
                match instr {
                    InstrOrWord::Instruction(i) => {
                        i.reify(prog_name, &self.defines)?;
                    }
                    InstrOrWord::Word(v) => {
                        let val = v.resolve(
                            SymbolId {
                                prog_name,
                                var_name: ".word",
                            },
                            &self.defines,
                            &mut pending,
                        )?;
                        *v = Value::Integer(val);
                    }
                }
            }
        }
        Ok(self)
    }

    pub fn compile(
        v: impl Iterator<Item = Line<'i>> + 'i,
    ) -> Result<File<'i, ParsedProgram<'i>>, Error<'i>> {
        let mut c = Compiler::collate(v)?.reify()?;

        // A program which does not declare a target inherits the
        // `.pio_version` written before the first program, when present.
        if let Some(default) = c.default_pio_version.clone() {
            for (_, p) in c.programs.values_mut() {
                p.pio_version.get_or_insert(default.clone());
            }
        }

        let programs = c
            .programs
            .into_iter()
            .map(|(name, (_, p))| (name, p))
            .collect();

        let mut pending = Vec::new();
        // resolve public defines
        let defines = c
            .defines
            .iter()
            .filter(|(_, (s, _))| s.public)
            .map(|(&k, (s, v))| Ok((k, (s.is_label, v.resolve(k, &c.defines, &mut pending)?))))
            .try_collect()?;

        Ok(File {
            programs,
            code_blocks: c.code_blocks,
            defines,
        })
    }

    /// Assembles the compiled programs into machine code.
    pub fn assemble<const PROGRAM_SIZE: usize>(
        v: File<'i, ParsedProgram<'i>>,
    ) -> Result<File<'i, AssembledProgram<'i, PROGRAM_SIZE>>, Error<'i>> {
        let File {
            programs,
            code_blocks,
            defines,
        } = v;

        let mut assembled = HashMap::with_capacity(programs.len());
        for (name, program) in programs {
            let lang_opt = program.lang_opt.clone();
            let program = Compiler::assemble_program::<PROGRAM_SIZE>(program)?;
            assembled.insert(
                name,
                AssembledProgram {
                    program: pio_core::ProgramWithDefines {
                        program,
                        public_defines: Compiler::public_defines(name, &defines),
                    },
                    lang_opt,
                },
            );
        }

        Ok(File {
            programs: assembled,
            code_blocks,
            defines,
        })
    }

    /// Returns the public defines visible from `prog_name`: the global ones
    /// shadowed by the ones defined in the program itself.
    fn public_defines(
        prog_name: &str,
        defines: &HashMap<SymbolId<'i>, (bool, i32)>,
    ) -> HashMap<String, i32> {
        let mut public_defines = HashMap::new();
        for scope in ["", prog_name] {
            for (symbol, (_, value)) in defines {
                if symbol.prog_name == scope {
                    public_defines.insert(symbol.var_name.to_string(), *value);
                }
            }
        }
        public_defines
    }

    /// Encodes a single program into pio machine code.
    fn assemble_program<const PROGRAM_SIZE: usize>(
        program: ParsedProgram<'i>,
    ) -> Result<pio_core::Program<PROGRAM_SIZE>, Error<'i>> {
        // `.side_set` and `.origin` are resolved during reification.
        let (side_set, side_max) = match &program.side_set {
            Some(ss) => {
                let bits: i32 = ss.value.clone().into();
                // With the `opt` flag, one bit of the delay/side-set field is
                // used to flag the presence of a side-set value.
                if !(0..=5).contains(&bits) || bits + ss.optional as i32 > 5 {
                    return Err(Error::IntegerOverflow("side_set must be in [0; 5]"));
                }
                let side_max = (1u8 << bits) - 1;
                (
                    pio_core::SideSet::new(ss.optional, bits as u8, ss.pindirs),
                    side_max,
                )
            }
            None => (pio_core::SideSet::default(), 0),
        };
        let delay_max = (1u8 << (5 - side_set.bits())) - 1;

        // The target hardware version; without `.pio_version` programs
        // target the latest version, like pioasm.
        let targets_rp2040 =
            program.pio_version.as_ref().map(|(v, _)| *v) == Some(pio_core::PioVersion::V0);

        if let Some((clock_div, _)) = &program.clock_div {
            if clock_div.integer == 0 || clock_div.integer > 0xffff {
                return Err(Error::IntegerOverflow(
                    "clock divider must be at least 1 and less than 65536",
                ));
            }
        }
        if let Some((fifo, location)) = &program.fifo {
            if targets_rp2040 {
                let feature = match fifo {
                    FifoConfig::Txput => Some(".fifo txput"),
                    FifoConfig::Txget => Some(".fifo txget"),
                    FifoConfig::Putget => Some(".fifo putget"),
                    _ => None,
                };
                if let Some(feature) = feature {
                    return Err(Error::FeatureNotSupportedOnPioVersion0 {
                        location: location.clone(),
                        feature,
                    });
                }
            }
        }
        if let Some((status, location)) = &program.mov_status {
            match status {
                MovStatus::IrqSet { index, .. } => {
                    if targets_rp2040 {
                        return Err(Error::FeatureNotSupportedOnPioVersion0 {
                            location: location.clone(),
                            feature: ".mov_status irq",
                        });
                    }
                    let index: i32 = index.clone().into();
                    if !(0..=7).contains(&index) {
                        return Err(Error::IntegerOverflow("mov_status irq must be in [0; 7]"));
                    }
                }
                MovStatus::TxFifoBelow(n) | MovStatus::RxFifoBelow(n) => {
                    let n: i32 = n.clone().into();
                    if !(0..=31).contains(&n) {
                        return Err(Error::IntegerOverflow(
                            "mov_status fifo threshold must be in [0; 31]",
                        ));
                    }
                }
            }
        }
        if let Some(shift) = &program.in_config {
            let count: i32 = shift.count.clone().into();
            if !(1..=32).contains(&count) {
                return Err(Error::IntegerOverflow("in count must be in [1; 32]"));
            }
            // The input shift count is not configurable on the RP2040.
            if targets_rp2040 && count != 32 {
                return Err(Error::IntegerOverflow(
                    "in count must be 32 when targeting PIO version 0",
                ));
            }
            let threshold: i32 = shift.threshold.clone().into();
            if !(1..=32).contains(&threshold) {
                return Err(Error::IntegerOverflow(
                    "auto-push threshold must be in [1; 32]",
                ));
            }
        }
        if let Some(shift) = &program.out_config {
            let count: i32 = shift.count.clone().into();
            if !(0..=32).contains(&count) {
                return Err(Error::IntegerOverflow("out count must be in [0; 32]"));
            }
            let threshold: i32 = shift.threshold.clone().into();
            if !(1..=32).contains(&threshold) {
                return Err(Error::IntegerOverflow(
                    "auto-pull threshold must be in [1; 32]",
                ));
            }
        }
        if let Some((count, _)) = &program.set_count {
            let count: i32 = count.clone().into();
            if !(0..=5).contains(&count) {
                return Err(Error::IntegerOverflow("set count must be in [0; 5]"));
            }
        }

        let count = program.instr.len();
        if count == 0 {
            return Err(Error::EmptyProgram);
        }
        if count > PROGRAM_SIZE {
            return Err(Error::ProgramTooLong {
                count,
                max: PROGRAM_SIZE,
            });
        }

        let mut code = Vec::with_capacity(count);
        let mut instructions = ArrayVec::new();
        for entry in &program.instr {
            match entry {
                InstrOrWord::Instruction(i) => {
                    let delay: i32 = i.delay.clone().map(Into::into).unwrap_or(0);
                    if delay < 0 || delay > delay_max as i32 {
                        return Err(Error::IntegerOverflow("delay value out of range"));
                    }
                    let side_set_value = match &i.side_set {
                        Some(v) => {
                            let side: i32 = v.clone().into();
                            if side < 0 || side > side_max as i32 {
                                return Err(Error::IntegerOverflow(
                                    "side-set value is greater than the side-set size",
                                ));
                            }
                            Some(side as u8)
                        }
                        None if side_set.bits() > 0 && !side_set.optional() => {
                            // The encoding leaves no room for instructions
                            // without a side-set value.
                            return Err(Error::IntegerOverflow(
                                "every instruction of the program requires a 'side' set",
                            ));
                        }
                        None => None,
                    };
                    let instruction = pio_core::Instruction {
                        operands: i.ops.clone().try_into()?,
                        delay: delay as u8,
                        side_set: side_set_value,
                    };
                    code.push(instruction.encode(side_set));
                    instructions.push(instruction);
                }
                InstrOrWord::Word(v) => {
                    let value: i32 = v.clone().into();
                    let value = u16::try_from(value).map_err(|_| Error::IntegerOutOfRange)?;
                    code.push(value);
                }
            }
        }

        // `Assembler::version` is the reference implementation to detect
        // instructions which are only available on the RP2350.
        let version = {
            let mut a = pio_core::Assembler::<PROGRAM_SIZE>::new_with_side_set(side_set);
            a.instructions = instructions;
            a.version()
        };
        // A program targeting the RP2040 may not use the instructions
        // exclusive to the RP2350.
        if let Some((pio_core::PioVersion::V0, location)) = &program.pio_version {
            if version == pio_core::PioVersion::V1 {
                return Err(Error::InstructionsRequirePioVersion1(location.clone()));
            }
        }

        let origin = match &program.origin {
            Some((origin, _)) => {
                let origin: i32 = origin.clone().into();
                if !(0..=31).contains(&origin) {
                    return Err(Error::IntegerOverflow("origin must be in [0; 31]"));
                }
                Some(origin as u8)
            }
            None => None,
        };

        let wrap = match (program.wrap, program.wrap_target) {
            (Some((source, _)), Some((target, _))) => pio_core::Wrap {
                source: (source - 1) as u8,
                target: target as u8,
            },
            (None, None) => pio_core::Wrap {
                source: (count - 1) as u8,
                target: 0,
            },
            (Some((_, location)), None) => return Err(Error::WrapWithoutWrapTarget(location)),
            (None, Some((_, location))) => return Err(Error::WrapTargetWithoutWrap(location)),
        };

        Ok(pio_core::Program {
            code: pio_core::Storage::Array(code.into_iter().collect()),
            origin,
            side_set,
            wrap,
            version,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::pipeline::ast::{Expression, Instruction, InstructionOperands, Value};

    use super::Error;

    #[test]
    fn resolve_undef_symbol() {
        let res = super::Compiler::compile(
            crate::pipeline::parse(
                r".program test
jmp glb_var",
            )
            .expect("failed to parse"),
        )
        .err();

        assert_eq!(res, Some(Error::UndefinedSymbol("jmp", "glb_var")));
    }
    #[test]
    fn resolve_global_var() {
        let p = super::Compiler::compile(
            crate::pipeline::parse(
                r".define glb_var 0
.program test
jmp glb_var",
            )
            .expect("failed to parse"),
        )
        .expect("failed to compile");

        assert_eq!(
            p.programs["test"].instr[0].instruction(),
            Some(&Instruction {
                ops: InstructionOperands::Jmp {
                    condition: None,
                    target: Expression::Value(Value::Integer(0))
                },
                delay: None,
                side_set: None
            })
        );
    }
    #[test]
    fn resolve_program_var() {
        let p = super::Compiler::compile(
            crate::pipeline::parse(
                r".program test
.define local_var 1
jmp local_var",
            )
            .expect("failed to parse"),
        )
        .expect("failed to compile");

        assert_eq!(
            p.programs["test"].instr[0].instruction(),
            Some(&Instruction {
                ops: InstructionOperands::Jmp {
                    condition: None,
                    target: Expression::Value(Value::Integer(1))
                },
                delay: None,
                side_set: None
            })
        );
    }
    #[test]
    fn resolve_circular_dependencies() {
        let res = super::Compiler::compile(
            crate::pipeline::parse(
                r".define glb_var1 glb_var2
.define glb_var2 glb_var1
.program test
jmp glb_var1",
            )
            .expect("failed to parse"),
        )
        .err();

        assert_eq!(res, Some(Error::CircularDependency("glb_var2", "glb_var1")));
    }
    #[test]
    fn resolve_local_reference_to_global() {
        let p = super::Compiler::compile(
            crate::pipeline::parse(
                r".define glb_var 0
.program test
.define lcl_var glb_var
jmp lcl_var",
            )
            .expect("failed to parse"),
        )
        .expect("failed to parse");

        assert_eq!(
            p.programs["test"]
                .instr
                .last()
                .and_then(|l| l.instruction()),
            Some(&Instruction {
                ops: InstructionOperands::Jmp {
                    condition: None,
                    target: Expression::Value(Value::Integer(0))
                },
                delay: None,
                side_set: None
            })
        );
    }
    #[test]
    fn resolve_global_reference_to_local() {
        let p = super::Compiler::compile(
            crate::pipeline::parse(
                r".define glb_var lcl_var
.program test
.define lcl_var 0
jmp glb_var",
            )
            .expect("failed to parse"),
        )
        .expect("failed to parse");

        assert_eq!(
            p.programs["test"]
                .instr
                .last()
                .and_then(|l| l.instruction()),
            Some(&Instruction {
                ops: InstructionOperands::Jmp {
                    condition: None,
                    target: Expression::Value(Value::Integer(0))
                },
                delay: None,
                side_set: None
            })
        );
    }
    #[test]
    fn public_defines_show_in_result() {
        let p = super::Compiler::compile(
            crate::pipeline::parse(
                r".define *glb_var 42
.define use_local lcl_var+1

.program hey
.define *lcl_var 43
.define *lcl_var2 use_local
*start: nop

.program ho
nop
*start: jmp start",
            )
            .expect("failed to parse"),
        )
        .expect("failed to parse");

        assert_eq!(
            p.defines,
            [
                (("", "glb_var").into(), (false, 42)),
                (("hey", "lcl_var").into(), (false, 43)),
                (("hey", "lcl_var2").into(), (false, 44)),
                (("hey", "start").into(), (true, 0)),
                (("ho", "start").into(), (true, 1)),
            ]
            .into_iter()
            .collect()
        );
    }
}

#[cfg(test)]
mod assemble_tests {
    use super::Error;
    use crate::pipeline::Error as PipelineError;

    // `mov y, y`, fields grouped per the PIO instruction layout (3/5/3/5).
    #[allow(clippy::unusual_byte_groupings)]
    const NOP: u16 = 0b101_00000_010_00010;

    fn assemble<const PROGRAM_SIZE: usize>(
        src: &str,
    ) -> Result<super::File<'_, super::AssembledProgram<'_, PROGRAM_SIZE>>, PipelineError<'_>> {
        crate::pipeline::compile_and_assemble(src)
    }

    #[test]
    fn assembles_a_simple_program() {
        let file = assemble::<32>(
            r".program test
nop
nop",
        )
        .expect("failed to assemble");
        let program = &file.programs["test"].program.program;
        assert_eq!(&*program.code, &[NOP, NOP]);
        // Default wrap covers the whole program.
        assert_eq!(program.wrap.source, 1);
        assert_eq!(program.wrap.target, 0);
        assert_eq!(program.origin, None);
    }

    #[test]
    fn assembles_origin_and_wrap() {
        let file = assemble::<32>(
            r".program test
.origin 4
.wrap_target
nop
nop
.wrap",
        )
        .expect("failed to assemble");
        let program = &file.programs["test"].program.program;
        assert_eq!(program.origin, Some(4));
        assert_eq!(program.wrap.source, 1);
        assert_eq!(program.wrap.target, 0);
    }

    #[test]
    fn wrap_without_target_is_an_error() {
        let err = assemble::<32>(
            r".program test
nop
.wrap",
        )
        .err()
        .expect("expected an error");
        assert!(matches!(
            err,
            PipelineError::Compiler(Error::WrapWithoutWrapTarget(_))
        ));
    }

    #[test]
    fn wrap_target_without_wrap_is_an_error() {
        let err = assemble::<32>(
            r".program test
.wrap_target
nop",
        )
        .err()
        .expect("expected an error");
        assert!(matches!(
            err,
            PipelineError::Compiler(Error::WrapTargetWithoutWrap(_))
        ));
    }

    #[test]
    fn empty_program_is_an_error() {
        let err = assemble::<32>(r".program test").err();
        assert!(matches!(
            err,
            Some(PipelineError::Compiler(Error::EmptyProgram))
        ));
    }

    #[test]
    fn program_over_max_size_is_an_error() {
        let mut src = String::from(".program test\n");
        for _ in 0..33 {
            src.push_str("nop\n");
        }
        let err = assemble::<32>(&src).err();
        assert!(matches!(
            err,
            Some(PipelineError::Compiler(Error::ProgramTooLong {
                count: 33,
                max: 32,
            }))
        ));
    }

    #[test]
    fn word_occupies_an_instruction_slot() {
        let file = assemble::<32>(
            r".program test
.word 0xabcd
nop",
        )
        .expect("failed to assemble");
        let program = &file.programs["test"].program.program;
        assert_eq!(&*program.code, &[0xabcd, NOP]);
    }

    #[test]
    fn word_out_of_range_is_an_error() {
        let err = assemble::<32>(
            r".program test
.word 0x10000",
        )
        .err();
        assert!(matches!(
            err,
            Some(PipelineError::Compiler(Error::IntegerOutOfRange))
        ));
    }

    #[test]
    fn side_set_values_are_encoded() {
        let file = assemble::<32>(
            r".program test
.side_set 1 opt
nop side 1 [2]",
        )
        .expect("failed to assemble");
        let program = &file.programs["test"].program.program;
        assert!(program.side_set.optional());
        assert_eq!(program.side_set.bits(), 2);
        assert!(!program.side_set.pindirs());
        // side=1 (1-bit) shifted into the side field with the opt flag, plus
        // delay=2:  (delay=2) | (side_present=1)<<4 | (side=1)<<(5-2) = 0b11010.
        assert_eq!(&*program.code, &[NOP | (0b11010u16 << 8)]);
    }

    #[test]
    fn missing_side_set_when_required_is_an_error() {
        let err = assemble::<32>(
            r".program test
.side_set 1
nop",
        )
        .err();
        assert!(matches!(
            err,
            Some(PipelineError::Compiler(Error::IntegerOverflow(_)))
        ));
    }

    #[test]
    fn delay_too_large_is_an_error() {
        let err = assemble::<32>(
            r".program test
nop [32]",
        )
        .err();
        assert!(matches!(
            err,
            Some(PipelineError::Compiler(Error::IntegerOverflow(_)))
        ));
    }

    #[test]
    fn rp2350_instructions_require_version_1() {
        // Each of these constructs is only available on the RP2350.
        for src in [
            "wait 1 jmppin",
            "wait 1 jmppin + 2",
            "mov rxfifo[y], isr",
            "mov osr, rxfifo[0]",
            "mov pindirs, x",
            "wait 1 irq prev 1",
            "wait 1 irq next 1",
            "irq 1 prev",
            "irq 1 next",
        ] {
            let full = format!(".program test\n{src}\nnop\n");
            let file = assemble::<32>(&full).unwrap_or_else(|e| panic!("{src}: {e:?}"));
            assert_eq!(
                file.programs["test"].program.program.version,
                pio_core::PioVersion::V1,
                "`{src}` should require the RP2350"
            );
        }
    }

    #[test]
    fn rp2040_instructions_do_not_require_version_1() {
        // `rel` irq addressing is available on both generations, and
        // `set pindirs` is not flagged by pio_core::Assembler::version().
        let file = assemble::<32>(
            r".program test
wait 1 irq rel 1
irq 2 rel
set pindirs, 1
nop",
        )
        .expect("failed to assemble");
        assert_eq!(
            file.programs["test"].program.program.version,
            pio_core::PioVersion::V0
        );
    }

    #[test]
    fn illegal_rxfifo_mov_combinations_are_rejected() {
        for src in [
            "mov rxfifo[y], x",
            "mov pins, rxfifo[y]",
            "mov rxfifo[0], rxfifo[1]",
        ] {
            let full = format!(".program test\n{src}\n");
            let err = assemble::<32>(&full).err();
            assert!(
                matches!(err, Some(PipelineError::Compiler(Error::InvalidMovFifo))),
                "`{src}` should be rejected, got: {err:?}"
            );
        }
    }

    #[test]
    fn jmppin_offset_out_of_range_is_rejected() {
        // The RP2350 datasheet (Section 11.4.3) only defines jmppin offsets
        // 0-3; everything else is a reserved encoding.
        for offset in ["4", "31", "(0-1)"] {
            let full = format!(".program test\nwait 1 jmppin + {offset}\n");
            let err = assemble::<32>(&full).err();
            assert!(
                matches!(
                    err,
                    Some(PipelineError::Compiler(Error::IntegerOverflow(_)))
                ),
                "`wait 1 jmppin + {offset}` should be rejected, got: {err:?}"
            );
        }
    }

    #[test]
    fn fifo_configs_not_supported_on_pio_version_0() {
        for cfg in ["txput", "txget", "putget"] {
            // Without `.pio_version` the target is the latest version.
            let full = format!(".program test\n.fifo {cfg}\nnop\n");
            assemble::<32>(&full).expect("should assemble");
            // Declaring an RP2040 target rejects the version 1 configs.
            let full = format!(".program test\n.pio_version rp2040\n.fifo {cfg}\nnop\n");
            let err = assemble::<32>(&full).err();
            assert!(
                matches!(
                    err,
                    Some(PipelineError::Compiler(
                        Error::FeatureNotSupportedOnPioVersion0 { .. }
                    ))
                ),
                "`.fifo {cfg}` should be rejected, got: {err:?}"
            );
        }
        // The other configs assemble on both targets.
        for cfg in ["txrx", "tx", "rx"] {
            let full = format!(".program test\n.pio_version 0\n.fifo {cfg}\nnop\n");
            assemble::<32>(&full).expect("should assemble");
        }
    }

    #[test]
    fn mov_status_irq_requires_version_1() {
        for irq_form in ["irq set 1", "irq prev set 1", "irq next set 7"] {
            let full = format!(".program test\n.mov_status {irq_form}\nnop\n");
            assemble::<32>(&full).expect("should assemble");
            let full = format!(".program test\n.pio_version 0\n.mov_status {irq_form}\nnop\n");
            let err = assemble::<32>(&full).err();
            assert!(
                matches!(
                    err,
                    Some(PipelineError::Compiler(
                        Error::FeatureNotSupportedOnPioVersion0 { .. }
                    ))
                ),
                "`.mov_status {irq_form}` should be rejected, got: {err:?}"
            );
        }
        // The fifo forms are supported on both versions.
        assemble::<32>(".program test\n.pio_version 0\n.mov_status txfifo < 3\nnop\n")
            .expect("should assemble");
    }

    #[test]
    fn rp2040_target_rejects_rp2350_instructions() {
        let err = assemble::<32>(
            ".program test
.pio_version rp2040
mov pindirs, x",
        )
        .err();
        assert!(matches!(
            err,
            Some(PipelineError::Compiler(
                Error::InstructionsRequirePioVersion1(_)
            ))
        ));
    }

    #[test]
    fn file_level_pio_version_gates_all_programs() {
        // A program which does not declare a target inherits the
        // file-level one.
        let err = assemble::<32>(
            ".pio_version rp2040
.program a
nop
.program b
irq 1 next",
        )
        .err();
        assert!(matches!(
            err,
            Some(PipelineError::Compiler(
                Error::InstructionsRequirePioVersion1(_)
            ))
        ));

        // The per-program directive overrides the file-level default.
        let file = assemble::<32>(
            ".pio_version rp2040
.program a
.pio_version rp2350
irq 1 next
nop
.program b
nop",
        )
        .expect("failed to assemble");
        assert_eq!(
            file.programs["a"].program.program.version,
            pio_core::PioVersion::V1
        );
        assert_eq!(
            file.programs["b"].program.program.version,
            pio_core::PioVersion::V0
        );
    }

    #[test]
    fn invalid_pio_version_is_rejected() {
        for version in ["2", "100"] {
            let full = format!(".program test\n.pio_version {version}\nnop\n");
            let err = assemble::<32>(&full).err();
            assert!(
                matches!(
                    err,
                    Some(PipelineError::Compiler(Error::InvalidPioVersion(_)))
                ),
                "`.pio_version {version}` should be rejected, got: {err:?}"
            );
        }
    }

    #[test]
    fn config_directives_must_precede_the_first_instruction() {
        for directive in [
            ".clock_div 2",
            ".fifo txrx",
            ".mov_status txfifo < 1",
            ".in 32",
            ".out 32",
            ".set 1",
            ".pio_version 1",
        ] {
            let full = format!(".program test\nnop\n{directive}\n");
            let err = assemble::<32>(&full).err();
            assert!(
                matches!(
                    err,
                    Some(PipelineError::Compiler(
                        Error::MustAppearBeforeAnyInstr { .. }
                    ))
                ),
                "`{directive}` should be rejected, got: {err:?}"
            );
        }
    }

    #[test]
    fn config_directives_are_invalid_outside_of_a_program() {
        for directive in [
            ".clock_div 2",
            ".fifo txrx",
            ".mov_status txfifo < 1",
            ".in 32",
            ".out 32",
            ".set 1",
        ] {
            let full = format!("{directive}\nnop\n");
            let err = assemble::<32>(&full).err();
            assert!(
                matches!(
                    err,
                    Some(PipelineError::Compiler(
                        Error::InvalidOutsideOfProgram { .. }
                    ))
                ),
                "`{directive}` should be rejected, got: {err:?}"
            );
        }
        // `.pio_version` is legal outside of a program: it then sets the
        // default for the whole file.
        assemble::<32>(".pio_version rp2350\n").expect("should assemble");
    }

    #[test]
    fn config_directive_values_are_range_checked() {
        for directive in [
            ".clock_div 0.5",
            ".clock_div 65536",
            ".mov_status txfifo < 32",
            ".mov_status rxfifo < (0-1)",
            ".mov_status irq set 8",
            ".mov_status irq set (0-1)",
            ".in 0",
            ".in 33",
            ".in 32 right auto 0",
            ".in 32 right auto 33",
            ".out 33",
            ".out 32 left auto 33",
            ".set 6",
            ".set (0-1)",
        ] {
            let full = format!(".program test\n{directive}\nnop\n");
            let err = assemble::<32>(&full).err();
            assert!(
                matches!(
                    err,
                    Some(PipelineError::Compiler(Error::IntegerOverflow(_)))
                ),
                "`{directive}` should be rejected, got: {err:?}"
            );
        }
    }

    #[test]
    fn in_count_must_be_32_for_version_0() {
        // The input shift count is not configurable on the RP2040.
        let err = assemble::<32>(".program test\n.pio_version 0\n.in 16\nnop\n").err();
        assert!(matches!(
            err,
            Some(PipelineError::Compiler(Error::IntegerOverflow(_)))
        ));
        assemble::<32>(".program test\n.pio_version 0\n.in 32\nnop\n").expect("should assemble");
        // The latest version accepts narrower counts.
        assemble::<32>(".program test\n.in 16\nnop\n").expect("should assemble");
    }

    #[test]
    fn config_directives_are_exposed_on_parsed_programs() {
        use crate::pipeline::{ClockDiv, FifoConfig, MovStatus, ShiftDirection, Value};

        let file = crate::pipeline::compile(
            ".define n 8
.program test
.pio_version rp2040
.clock_div 2.5
.fifo txrx
.mov_status rxfifo < 4
.in 32 left auto n
.out n right manual (n/2)
.set 5
nop",
        )
        .expect("failed to compile");
        let p = &file.programs["test"];

        assert_eq!(
            p.pio_version.as_ref().map(|(v, _)| *v),
            Some(pio_core::PioVersion::V0)
        );
        let (clock_div, _) = p.clock_div.clone().unwrap();
        assert_eq!(
            clock_div,
            ClockDiv {
                integer: 2,
                fractional: 128
            }
        );
        assert_eq!(p.fifo.clone().unwrap().0, FifoConfig::Txrx);
        assert_eq!(
            p.mov_status.clone().unwrap().0,
            MovStatus::RxFifoBelow(Value::Integer(4))
        );

        let shift = p.in_config.clone().unwrap();
        assert_eq!(shift.count, Value::Integer(32));
        assert_eq!(shift.direction, ShiftDirection::Left);
        assert!(shift.auto);
        // The symbols are resolved during reification.
        assert_eq!(shift.threshold, Value::Integer(8));

        let shift = p.out_config.clone().unwrap();
        assert_eq!(shift.count, Value::Integer(8));
        assert_eq!(shift.direction, ShiftDirection::Right);
        assert!(!shift.auto);
        assert_eq!(shift.threshold, Value::Integer(4));

        assert_eq!(p.set_count.clone().unwrap().0, Value::Integer(5));
    }

    #[test]
    fn mov_status_irq_sources_are_exposed() {
        use crate::pipeline::{MovStatus, MovStatusIrqSource as Source, Value};
        for (directive, source) in [
            (".mov_status irq set 0", Source::This),
            (".mov_status irq prev set 0", Source::Prev),
            (".mov_status irq next set 0", Source::Next),
        ] {
            let full = format!(".program test\n{directive}\nnop\n");
            let file = crate::pipeline::compile(&full).expect("failed to compile");
            assert_eq!(
                file.programs["test"].mov_status.clone().unwrap().0,
                MovStatus::IrqSet {
                    index: Value::Integer(0),
                    source
                },
                "`{directive}`"
            );
        }
    }
}
