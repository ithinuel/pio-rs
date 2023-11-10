//! # Programmable Input/Output
//!
//! ```rust
//! // Repeatedly get one word of data from the TX FIFO, stalling when
//! // the FIFO is empty. Write the least significant bit to the OUT pin
//! // group.
//! // https://github.com/raspberrypi/pico-examples/tree/master/pio/hello_pio/hello.pio
//! let mut a = pio::Assembler::<Vec<_>>::new();
//!
//! let mut loop_label = a.label();
//!
//! a.bind(&mut loop_label);
//! a.pull(false, false);
//! a.out(pio::OutDestination::PINS, 1);
//! a.jmp(pio::JmpCondition::Always, &mut loop_label);
//!
//! let program = a.assemble_program::<Vec<_>>();
//! ```
//!
//! ## Wrapping
//! ```rust
//! use arrayvec::ArrayVec;
//! use pio::{SetDestination, OutDestination};
//! let mut a = pio::Assembler::<Vec<_>>::new();
//!
//! let mut wrap_source = a.label();
//! let mut wrap_target = a.label();
//!
//! // Initialize pin direction only once
//! a.set(SetDestination::PINDIRS, 1);
//! a.bind(&mut wrap_target);
//! a.out(OutDestination::PINS, 1);
//! a.bind(&mut wrap_source);
//!
//! let program = a.assemble_with_wrap::<Vec<_>>(wrap_source, wrap_target);
//! ```

#![cfg_attr(not(feature = "std"), no_std)]
// PIO instr grouping is 3/5/3/5
#![allow(clippy::unusual_byte_groupings)]
#![allow(clippy::upper_case_acronyms)]

pub use arrayvec::ArrayVec;

mod assembler;
mod intermediate_repr;
mod storage;

pub use assembler::*;
pub use intermediate_repr::*;

/// Data for 'side' set instruction parameters.
#[derive(Debug, Clone, Copy)]
pub struct SideSet {
    opt: bool,
    bits: u8,
    max: u8,
    pindirs: bool,
}

impl SideSet {
    pub const fn new(opt: bool, bits: u8, pindirs: bool) -> SideSet {
        SideSet {
            opt,
            bits: bits + opt as u8,
            max: (1 << bits) - 1,
            pindirs,
        }
    }

    #[doc(hidden)]
    pub fn new_from_proc_macro(opt: bool, bits: u8, pindirs: bool) -> SideSet {
        SideSet {
            opt,
            bits,
            max: (1 << bits) - 1,
            pindirs,
        }
    }

    pub fn optional(&self) -> bool {
        self.opt
    }

    pub fn bits(&self) -> u8 {
        self.bits
    }

    pub fn pindirs(&self) -> bool {
        self.pindirs
    }
}

impl Default for SideSet {
    fn default() -> Self {
        SideSet::new(false, 0, false)
    }
}

/// A label in pointing in the program.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Label(u8);

/// Source and target for automatic program wrapping.
///
/// After the instruction at offset pointed by [`source`] has been executed, the program control flow jumps to the
/// instruction pointed by [`target`]. If the instruction is a jump, and the condition is true, the jump takes priority.
///
/// [`source`]: Self::source
/// [`target`]: Self::target
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Wrap {
    /// Source instruction for wrap.
    pub source: Label,
    /// Target instruction for wrap.
    pub target: Label,
}

/// Program ready to be executed by PIO hardware.
#[derive(Debug, Clone)]
pub struct Program<T> {
    /// Assembled program code.
    code: T,
    /// Offset at which the program must be loaded.
    ///
    /// Most often 0 if defined. This might be needed when using data based `JMP`s.
    ///
    /// NOTE: Instruction addresses in JMP instructions as well as
    /// wrap source/target are calculated as if the origin was 0.
    /// Functions loading the program into PIO instruction memory will
    /// adjust those addresses accordingly if the program is loaded
    /// to a non-zero origin address.
    origin: Option<u8>,
    /// Wrapping behavior for this program.
    wrap: Wrap,
    /// Side-set info for this program.
    side_set: SideSet,
}

impl<T> Program<T> {
    /// Get the encoded instructions.
    pub fn code(&self) -> &T {
        &self.code
    }

    /// Get the program loading location (if any),
    pub fn origin(&self) -> Option<u8> {
        self.origin
    }

    /// Set the program loading location.
    ///
    /// If `None`, the program can be loaded at any location in the instruction memory.
    pub fn set_origin(self, origin: Option<u8>) -> Self {
        Self { origin, ..self }
    }

    /// Get the wrap points of this program.
    pub fn wrap(&self) -> &Wrap {
        &self.wrap
    }
    pub(crate) fn set_wrap(self, wrap: Wrap) -> Self {
        Self { wrap, ..self }
    }

    /// Get the SideSet parameters for this program.
    pub fn side_set(&self) -> &SideSet {
        &self.side_set
    }
}

/// Parsed program with defines.
pub struct ProgramWithDefines<PublicDefines, T> {
    /// The compiled program.
    program: Program<T>,
    /// Public defines.
    public_defines: PublicDefines,
}
impl<D, T> ProgramWithDefines<D, T> {
    pub fn program(&self) -> &Program<T> {
        &self.program
    }
    pub fn public_defines(&self) -> &D {
        &self.public_defines
    }
}

/// This block ensures that README.md is checked when `cargo test` is run.
#[cfg(doctest)]
mod test_readme {
    macro_rules! external_doc_test {
        ($x:expr) => {
            #[doc = $x]
            extern "C" {}
        };
    }
    //external_doc_test!(include_str!("../README.md"));
}

// End of file
