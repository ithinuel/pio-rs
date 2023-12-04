use std::collections::{hash_map::Entry, HashMap};

use super::*;
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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SymbolId<'i> {
    prog_name: &'i str,
    var_name: &'i str,
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
type Defines<'i> = HashMap<SymbolId<'i>, (SymbolDef<'i>, Expression<'i>)>;
trait Resolve<'i> {
    fn resolve(
        &self,
        ctx: SymbolId<'i>,
        defines: &Defines<'i>,
        pending: &mut Vec<&'i str>,
    ) -> Result<i32, Error<'i>>;
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

impl<'i> Instruction<'i> {
    fn reify(
        &mut self,
        prog_name: &'i str,
        defines: &Defines<'i>,
    ) -> Result<pio::Instruction, Error<'i>> {
        let operands = self.ops.clone().try_into()?;

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
            operands,
            delay,
            side_set,
        })
    }
}

impl<'i> InstructionOperands<'i> {
    fn reify(&mut self, prog_name: &'i str, defines: &Defines<'i>) -> Result<(), Error<'i>> {
        let mut pending = Vec::new();
        match self {
            InstructionOperands::Push { .. }
            | InstructionOperands::Pull { .. }
            | InstructionOperands::Nop
            | InstructionOperands::Mov { .. } => {}
            InstructionOperands::Out { bit_count: v, .. }
            | InstructionOperands::In { bit_count: v, .. }
            | InstructionOperands::Wait { polarity: v, .. }
            | InstructionOperands::Irq { value: v, .. }
            | InstructionOperands::Set { value: v, .. } => {
                *v = Value::Integer(v.resolve(
                    SymbolId {
                        prog_name,
                        var_name: "set",
                    },
                    defines,
                    &mut pending,
                )?);
            }
            InstructionOperands::Jmp { target: v, .. } => {
                *v = Expression::Value(Value::Integer(v.resolve(
                    SymbolId {
                        prog_name,
                        var_name: "jmp",
                    },
                    defines,
                    &mut pending,
                )?));
            }
        }
        Ok(())
    }
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

#[derive(Debug, Clone)]
pub struct AssembledProgram<'i> {
    pub program: pio::Program<Vec<pio::InstructionOrWord>>,
    pub lang_opt: HashMap<&'i str, HashMap<&'i str, &'i str>>,
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
pub struct Program<'i> {
    pub side_set: Option<pio::SideSet>,
    /// offset in program and where is wrap is set.
    pub wrap: Option<i32>,
    pub wrap_target: Option<i32>,
    pub origin: Option<i32>,
    pub lang_opt: HashMap<&'i str, HashMap<&'i str, &'i str>>,
    pub instr: Vec<InstrOrWord<'i>>,
}
#[derive(Debug, Clone)]
pub struct File<'i, T: 'i> {
    pub programs: HashMap<&'i str, T>,
    pub code_blocks: Vec<CodeBlock<'i>>,
    pub defines: HashMap<SymbolId<'i>, (bool, i32)>,
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
                        .or_insert_with(HashMap::new)
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

    /// reifyies Instructions
    fn reify(mut self) -> Result<Self, Error<'i>> {
        // define pending here so that it's capacity gets recycled in each sub loop
        let mut pending = Vec::new();

        for (prog_name, (_, program)) in self.programs.iter_mut() {
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
        println!("program: {programs:#?}");

        let mut pending = Vec::new();
        // resolve public defines
        let defines = c
            .defines
            .iter()
            .filter(|(_, (s, _))| s.public)
            .map(|(&k, (s, v))| Ok((k, (s.is_label, v.resolve(k, &c.defines, &mut pending)?))))
            .try_collect()?;

        println!("defines: {defines:#?}");
        Ok(File {
            programs,
            code_blocks: c.code_blocks,
            defines,
        })
    }

    pub fn assemble(
        v: File<'i, ParsedProgram<'i>>,
    ) -> Result<File<'i, AssembledProgram>, Error<'i>> {
        for _parsed_program in v.programs {
            todo!()
        }

        //let prog = AssembledProgram {
        //    program: todo!(),
        //    lang_opt: v.lang_opt,
        //};
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Instruction, InstructionOperands, Value};

    use super::Error;

    #[test]
    fn resolve_undef_symbol() {
        let res = super::Compiler::compile(
            super::parse(
                r".program test
jmp glb_var",
            )
            .expect("failed to parse")
            .into_iter(),
        )
        .err();

        assert_eq!(res, Some(Error::UndefinedSymbol("jmp", "glb_var")));
    }
    #[test]
    fn resolve_global_var() {
        let p = super::Compiler::compile(
            super::parse(
                r".define glb_var 0
.program test
jmp glb_var",
            )
            .expect("failed to parse")
            .into_iter(),
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
            super::parse(
                r".program test
.define local_var 1
jmp local_var",
            )
            .expect("failed to parse")
            .into_iter(),
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
            super::parse(
                r".define glb_var1 glb_var2
.define glb_var2 glb_var1
.program test
jmp glb_var1",
            )
            .expect("failed to parse")
            .into_iter(),
        )
        .err();

        assert_eq!(res, Some(Error::CircularDependency("glb_var2", "glb_var1")));
    }
    #[test]
    fn resolve_local_reference_to_global() {
        let p = super::Compiler::compile(
            super::parse(
                r".define glb_var 0
.program test
.define lcl_var glb_var
jmp lcl_var",
            )
            .expect("failed to parse")
            .into_iter(),
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
            super::parse(
                r".define glb_var lcl_var
.program test
.define lcl_var 0
jmp glb_var",
            )
            .expect("failed to parse")
            .into_iter(),
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
            super::parse(
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
            .expect("failed to parse")
            .into_iter(),
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
