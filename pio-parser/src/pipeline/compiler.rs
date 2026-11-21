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
    #[error("`.wrap` cannot be used without a preceding `.wrap_target`")]
    WrapWithoutWrapTarget(Location),
    #[error("`.wrap_target` cannot be used without a following `.wrap`")]
    WrapTargetWithoutWrap(Location),
    #[error("A program must contain at least one instruction")]
    EmptyProgram,
    #[error("Program contains {count} instructions but at most {max} are allowed")]
    ProgramTooLong { count: usize, max: usize },
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

#[derive(Debug, Clone, Default)]
pub struct ParsedProgram<'i> {
    pub side_set: Option<SideSet<'i>>,
    /// offset in program and where is wrap is set.
    pub wrap: Option<(usize, Location)>,
    pub wrap_target: Option<(usize, Location)>,
    pub origin: Option<(Value<'i>, Location)>,
    pub lang_opt: HashMap<&'i str, HashMap<&'i str, &'i str>>,
    pub instr: Vec<InstrOrWord<'i>>,
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
        let c = Compiler::collate(v)?.reify()?;

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
                if bits < 0 || bits > 5 || bits + ss.optional as i32 > 5 {
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

        let origin = match &program.origin {
            Some((origin, _)) => {
                let origin: i32 = origin.clone().into();
                if origin < 0 || origin > 31 {
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
            code: code.into_iter().collect(),
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

    const NOP: u16 = 0b101_00000_010_00010; // mov y, y

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
}
