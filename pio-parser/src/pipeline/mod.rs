//! Re-implementation of the PIO asm front-end.
//!
//! This module implements a compilation pipeline which is split in distinct
//! steps:
//!
//! 1. lexing ([`lexer`]) turns the input text into a stream of tokens.
//! 2. parsing ([`parser`]) turns the token stream into an abstract syntax
//!    tree ([`ast`]).
//! 3. compiling ([`compiler`]) collates the directives, resolves labels and
//!    defines and validates the operands.
//! 4. assembling turns the compiled programs into encoded pio instructions.
//!
//! It is currently a work in progress and lives alongside the existing
//! parser. Prefer the [`crate::Parser`] API until this module reaches feature
//! parity.

mod ast;
pub mod compiler;
pub mod lexer;
pub mod parser;

pub use ast::*;
pub use compiler::{AssembledProgram, CodeBlock, Compiler, File, ParsedProgram, SideSet, SymbolId};

/// Error which may occur while processing a PIO asm source file.
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum Error<'i> {
    #[error("Compiling error: {0}")]
    Compiler(compiler::Error<'i>),
    // `lalrpop_util::ParseError` has no `Display` impl without its `std`
    // feature, hence no `{0}` interpolation here.
    #[error("Parsing error")]
    Parse(parser::Error<'i>),
    #[error("`parse_program` input must not contain a `.program` directive")]
    UnexpectedProgramDirective,
}
impl<'i> From<compiler::Error<'i>> for Error<'i> {
    fn from(value: compiler::Error<'i>) -> Self {
        Self::Compiler(value)
    }
}
impl<'i> From<parser::Error<'i>> for Error<'i> {
    fn from(value: parser::Error<'i>) -> Self {
        Self::Parse(value)
    }
}
impl<'i> Error<'i> {
    /// Returns the byte range of the input the error refers to, when known.
    pub fn span(&self) -> Option<std::ops::Range<usize>> {
        match self {
            Error::Compiler(e) => e.span(),
            Error::Parse(lalrpop_util::ParseError::InvalidToken { location })
            | Error::Parse(lalrpop_util::ParseError::UnrecognizedEof { location, .. }) => {
                Some(*location..*location)
            }
            Error::Parse(lalrpop_util::ParseError::UnrecognizedToken { token, .. })
            | Error::Parse(lalrpop_util::ParseError::ExtraToken { token }) => {
                Some(token.0..token.2)
            }
            Error::Parse(lalrpop_util::ParseError::User { .. })
            | Error::UnexpectedProgramDirective => None,
        }
    }

    /// Human readable description of the error.
    ///
    /// Contrary to [`Display`](std::fmt::Display), parse errors also mention
    /// the offending and expected tokens.
    pub fn message(&self) -> String {
        match self {
            Error::Parse(lalrpop_util::ParseError::InvalidToken { .. }) => {
                "invalid token".to_string()
            }
            Error::Parse(lalrpop_util::ParseError::UnrecognizedEof { expected, .. }) => {
                format!(
                    "unexpected end of file, expected one of {}",
                    expected.join(", ")
                )
            }
            Error::Parse(lalrpop_util::ParseError::UnrecognizedToken { token, expected }) => {
                format!(
                    "unexpected token {:?}, expected one of {}",
                    token.1,
                    expected.join(", ")
                )
            }
            Error::Parse(lalrpop_util::ParseError::ExtraToken { token }) => {
                format!("extra token {:?}", token.1)
            }
            Error::Parse(lalrpop_util::ParseError::User { error }) => error.to_string(),
            other => other.to_string(),
        }
    }
}

/// Parses the input and returns an abstract syntax tree.
pub fn parse(input: &str) -> Result<impl Iterator<Item = Line<'_>>, parser::Error<'_>> {
    Ok(parser::FileParser::new()
        .parse(lexer::Lexer::new(input))?
        .into_iter()
        .flatten())
}

/// Compiles the input and returns a [`File`] representation.
pub fn compile(input: &str) -> Result<File<'_, ParsedProgram<'_>>, Error<'_>> {
    Ok(Compiler::compile(parse(input)?)?)
}

/// Compiles and assembles the input into machine code.
pub fn compile_and_assemble<const PROGRAM_SIZE: usize>(
    input: &str,
) -> Result<File<'_, AssembledProgram<'_, PROGRAM_SIZE>>, Error<'_>> {
    Ok(Compiler::assemble(compile(input)?)?)
}

/// Compiles a single program, without a `.program` directive.
///
/// The input is treated as if it started with an anonymous (empty-named)
/// program header. Since the program has no name, defines written in the
/// input are visible like global defines would be.
pub fn compile_program(input: &str) -> Result<File<'_, ParsedProgram<'_>>, Error<'_>> {
    let anonymous = std::iter::once(Line::Program {
        name: "",
        location: Location(0..0),
    });
    Ok(Compiler::compile(anonymous.chain(parse(input)?))?)
}

/// Compiles and assembles a single program, without a `.program directive.
///
/// Returns [`Error::UnexpectedProgramDirective`] when the input declares one
/// or more programs itself.
pub fn compile_and_assemble_program<const PROGRAM_SIZE: usize>(
    input: &str,
) -> Result<AssembledProgram<'_, PROGRAM_SIZE>, Error<'_>> {
    let file = compile_program(input)?;
    if file.programs.len() > 1 {
        return Err(Error::UnexpectedProgramDirective);
    }
    let mut file = Compiler::assemble::<PROGRAM_SIZE>(file)?;
    // `compile_program` always inserts the anonymous program.
    Ok(file.programs.remove("").unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compile_program_without_directive() {
        let assembled = compile_and_assemble_program::<32>(
            "
public label:
    pull
    out pins, 1
    jmp label
.define public owo label + 2
",
        )
        .expect("failed to assemble");

        assert_eq!(
            &*assembled.program.program.code,
            &[
                0b100_00000_101_00000u16, // PULL
                0b011_00000_000_00001,    // OUT PINS, 1
                0b000_00000_000_00000,    // JMP LABEL
            ]
        );
        assert_eq!(assembled.program.public_defines.get("label"), Some(&0));
        assert_eq!(assembled.program.public_defines.get("owo"), Some(&2));
    }

    #[test]
    fn compile_program_rejects_program_directive() {
        let err = compile_and_assemble_program::<32>(".program foo\nnop")
            .err()
            .expect("expected an error");
        assert_eq!(err, Error::UnexpectedProgramDirective);
    }

    #[test]
    fn errors_point_at_the_input() {
        // Parse errors carry the span of the offending token.
        let input = ".program test\nmov bad, x";
        let err = compile_and_assemble::<32>(input).err().unwrap();
        assert_eq!(&input[err.span().expect("expected a span")], "bad");
        assert!(err.message().contains("unexpected token"));
        assert!(err.message().contains("expected one of"));

        // Symbol resolution errors carry no location but a useful message.
        let err = compile_and_assemble::<32>(".program test\njmp nowhere")
            .err()
            .unwrap();
        assert_eq!(err.span(), None);
        assert!(err.message().contains("nowhere"));
    }
}
