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
pub use compiler::{CodeBlock, Compiler, File, ParsedProgram, SideSet, SymbolId};

/// Error which may occur while processing a PIO asm source file.
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum Error<'i> {
    #[error("Compiling error: {0}")]
    Compiler(compiler::Error<'i>),
    // `lalrpop_util::ParseError` has no `Display` impl without its `std`
    // feature, hence no `{0}` interpolation here.
    #[error("Parsing error")]
    Parse(parser::Error<'i>),
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
