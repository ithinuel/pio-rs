#[cfg(test)]
mod tests;

mod ast;
mod lexer;
mod parser;

pub use ast::*;
mod compiler;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum Error<'i> {
    #[error("Woops, compilation error")]
    Compiler(compiler::Error<'i>),
    #[error("Woops, parsing error")]
    Parser(parser::Error<'i>),
}
impl<'i> From<compiler::Error<'i>> for Error<'i> {
    fn from(value: compiler::Error<'i>) -> Self {
        Self::Compiler(value)
    }
}
impl<'i> From<parser::Error<'i>> for Error<'i> {
    fn from(value: parser::Error<'i>) -> Self {
        Self::Parser(value)
    }
}

/// Parses the input and returns an asbstract syntax tree.
pub fn parse(input: &str) -> Result<impl Iterator<Item = Line<'_>>, parser::Error<'_>> {
    Ok(parser::FileParser::new()
        .parse(lexer::Lexer::new(input))?
        .into_iter()
        .flatten())
}

/// Compiles the input and returns a File representation.
pub fn compile(input: &str) -> Result<compiler::File<'_>, Error<'_>> {
    Ok(compiler::Compiler::compile(parse(input)?)?)
}
