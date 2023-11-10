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
pub fn parse(input: &str) -> Result<Vec<Line<'_>>, parser::Error<'_>> {
    parser::FileParser::new()
        .parse(lexer::Lexer::new(input))
        .map(|v| v.into_iter().flat_map(|v| v).collect())
}

/// Compiles the input and returns a File representation.
pub fn compile(input: &str) -> Result<compiler::File<'_>, Error<'_>> {
    Ok(compiler::Compiler::compile(
        parser::FileParser::new()
            .parse(lexer::Lexer::new(input))
            .map(|v| v.into_iter().flat_map(|v| v))?,
    )?)
}
