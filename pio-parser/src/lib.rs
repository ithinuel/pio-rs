//! Parser for the PIO assembly language.
//!
//! [`Parser`] turns PIO asm source code into assembled [`pio_core`]
//! programs. The heavy lifting is done by the [`pipeline`] module, which
//! exposes the individual lexing, parsing, compiling and assembling steps.

use std::collections::HashMap;

use pio_core::ProgramWithDefines;

pub mod pipeline;

/// Error which may occur while parsing a PIO program.
///
/// Since 0.5 this is the full pipeline error ([`pipeline::Error`]), covering
/// lexing, parsing, symbol resolution and assembling. The previous type,
/// `lalrpop_util::ParseError` over the old grammar's private token type,
/// cannot be carried over: that grammar no longer exists. Use
/// [`pipeline::Error::span`] and [`pipeline::Error::message`] to render this
/// error against the source text.
pub type ParseError<'input> = pipeline::Error<'input>;

/// PIO asm parser.
pub struct Parser<const PROGRAM_SIZE: usize>;

impl<const PROGRAM_SIZE: usize> Parser<PROGRAM_SIZE> {
    /// Parse a PIO "file", which contains some number of PIO programs
    /// separated by `.program` directives.
    pub fn parse_file(
        source: &str,
    ) -> Result<
        HashMap<String, ProgramWithDefines<HashMap<String, i32>, PROGRAM_SIZE>>,
        ParseError<'_>,
    > {
        let file = pipeline::compile_and_assemble::<PROGRAM_SIZE>(source)?;
        Ok(file
            .programs
            .into_iter()
            .map(|(name, program)| (name.to_string(), program.program))
            .collect())
    }

    /// Parse a single PIO program, without the `.program` directive.
    pub fn parse_program(
        source: &str,
    ) -> Result<ProgramWithDefines<HashMap<String, i32>, PROGRAM_SIZE>, ParseError<'_>> {
        Ok(pipeline::compile_and_assemble_program::<PROGRAM_SIZE>(source)?.program)
    }
}

#[test]
// Field layout of the encodings below follows the PIO instruction
// groupings (3/5/3/5).
#[allow(clippy::unusual_byte_groupings)]
fn test() {
    let p = Parser::<32>::parse_program(
        "
    label:
      pull
      out pins, 1
      jmp label
    ",
    )
    .unwrap();

    assert_eq!(
        &p.program.code[..],
        &[
            // LABEL:
            0b100_00000_101_00000, // PULL
            0b011_00000_000_00001, // OUT PINS, 1
            0b000_00000_000_00000, // JMP LABEL
        ]
    );
    assert_eq!(p.program.origin, None);
    assert_eq!(
        p.program.wrap,
        pio_core::Wrap {
            source: 2,
            target: 0,
        }
    );
}

#[test]
#[allow(clippy::unusual_byte_groupings)]
fn test_rp2350() {
    let p = Parser::<32>::parse_program(
        "
    label:
      mov osr, rxfifo[0]
      mov rxfifo[1], isr
      mov pins, isr
      mov osr, x
      jmp label
    ",
    )
    .unwrap();

    assert_eq!(
        &p.program.code[..],
        &[
            // LABEL:
            0b100_00000_1001_1_000, // MOV OSR, RXFIFO0
            0b100_00000_0001_1_001, // MOV RXFIFO1, ISR
            0b101_00000_000_00_110, // MOV PINS, ISR
            0b101_00000_111_00_001, // MOV OSR, X
            0b000_00000_000_00000,  // JMP LABEL
        ]
    );
    assert_eq!(p.program.origin, None);
    assert_eq!(
        p.program.wrap,
        pio_core::Wrap {
            source: 4,
            target: 0,
        }
    );
}

#[test]
#[allow(clippy::unusual_byte_groupings)]
fn test_side_set() {
    let p = Parser::<32>::parse_program(
        "
    .side_set 1 opt
    .origin 5

    label:
      pull
      .wrap_target
      out pins, 1
      .wrap
      jmp label side 1
    ",
    )
    .unwrap();

    assert_eq!(
        &p.program.code[..],
        &[
            // LABEL:
            0b100_00000_101_00000, // PULL
            0b011_00000_000_00001, // OUT PINS, 1
            0b000_11000_000_00000, // JMP LABEL, SIDE 1
        ]
    );
    assert_eq!(p.program.origin, Some(5));
    assert_eq!(
        p.program.wrap,
        pio_core::Wrap {
            source: 1,
            target: 1,
        }
    );
}

#[test]
fn test_unknown_label() {
    // Unknown symbols are a typed error now; the legacy front-end panicked.
    let err = Parser::<32>::parse_program(
        "
    jmp some_unknown_label
    ",
    )
    .err()
    .expect("expected an error");

    assert_eq!(
        err,
        pipeline::Error::Compiler(pipeline::compiler::Error::UndefinedSymbol(
            "jmp",
            "some_unknown_label"
        ))
    );
}
