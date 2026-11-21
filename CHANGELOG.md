# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

The parser in `pio-parser` was rewritten: `pio_parser::Parser` now drives a new
compilation pipeline (logos lexer, lalrpop grammar, symbol-resolving compiler,
encoder) which supports the full syntax accepted by pioasm. This requires a new
major version of `pio-parser` (and `pio`/`pio-proc` to depend on it).

### Added

- `pio-parser`: new public `pipeline` module exposing the individual
  compilation steps: `parse` (text -> AST), `compile` (AST -> programs with
  resolved symbols) and `compile_and_assemble` (programs -> machine code),
  plus `compile_program`/`compile_and_assemble_program` for single-program
  input without a `.program` directive.
- `pio-parser`: `.word` directive support.
- `pio-parser`: directives (`.program`, `.define`, ...) are now accepted in
  any case, as per "instruction names, keywords and directives are case
  insensitive" in the datasheet.
- `pio-parser`: `pio_parser::pipeline::Error::span` and
  `pio_parser::pipeline::Error::message` locate and describe an error for
  diagnostics.
- `pio_file!`/`pio_asm!`: macro diagnostics now point at the offending
  source span for compile and assemble errors too, not just parse errors.
- `pio-parser`: the RP2350-only directives `.pio_version`, `.clock_div`,
  `.fifo`, `.mov_status`, `.in`, `.out` and `.set` are supported. They are
  validated when assembling (placement, target version and the value ranges
  pioasm enforces) and exposed as data on
  `pio_parser::pipeline::ParsedProgram`. A `.pio_version` written before the
  first program sets the default target for the whole file; without one
  programs target the latest version, like pioasm.

### Changed

- (Breaking) `pio-core`: `Program::code` is now a `Storage<PROGRAM_SIZE>`
  enum — `Storage::Slice(&'static [u16])` for programs baked at compile
  time (what `pio_asm!`/`pio_file!` now emit) or `Storage::Array(ArrayVec)`
  for programs built at runtime. `Storage` derefs to `[u16]`, so reads are
  unchanged; the slice variant makes `const`/`static` program bindings
  possible (`const P: pio::Program<32> = pio::pio_asm!(…).program;`).
- (Breaking) `pio_parser::ParseError` is now `pio_parser::pipeline::Error`, a
  typed error covering lexing, parsing, symbol resolution and assembling. The
  old type was `lalrpop_util::ParseError` over the previous grammar's private
  token type and could not represent the new failure modes.
- (Breaking) Errors replace panics and silent mis-assembly: unknown labels and
  illegal `mov rxfifo[..]` combinations no longer panic, and oversized
  programs, delays, origins, side-set values and `.word` values, and unpaired
  `.wrap`/`.wrap_target` no longer assemble into garbage.
- (Breaking) Some inputs previously accepted are rejected: `.program`
  directives in single-program input (`pio_asm!`), `.lang_opt` outside a
  program, and empty programs.
- (Breaking) `pio-core` & `pio-parser`: `wait` jmppin offsets are limited to
  0-3 as defined by the RP2350 datasheet (Section 11.4.3); larger offsets are
  a compile error (and a panic when assembling through `pio_core`), and
  instructions carrying a reserved offset are no longer decoded.
- `pio-parser`: the datasheet suffix form `wait <polarity> irq <irq_num>
  prev|next` now parses; as was already the case for `rel`, a trailing index
  mode overrides a prefixed one.
- (Breaking) `pio-parser`: the words `rxfifo`, `txfifo`, `txrx`, `tx`, `rx`,
  `txput`, `txget`, `putget`, `left`, `right`, `auto`, `manual`, `rp2040`
  and `rp2350` are keywords for the new directives and can no longer be used
  as program, label or define names; pioasm reserves the same words.

### Removed

- `pio-proc`: removed the `lalrpop-util` dependency.

## [0.4.0] [Crates.io](https://crates.io/crates/pio-rs/0.4.0) [Github](https://github.com/rp-rs/pio-rs/releases/tag/v0.4.0)

- Added support for the RP235x `WAIT JMPPIN` and `WAIT IRQ PREV/NEXT` instruction variants.
- Redesigned `WaitSource` into a data-carrying enum: `Gpio(u8)`, `Pin(u8)`, `Irq { index_mode, irq }`, `JmpPin { offset }`. (Breaking change)
- Removed the `index` and `relative` fields from `InstructionOperands::WAIT`; they are now part of `WaitSource`. `Assembler::wait` takes only `polarity` and `source`. (Breaking change)
- Removed the `proc-macro-error2` dependency.
- Disabled default features of `lalrpop`/`lalrpop-util`, reducing build dependencies.

## [0.3.0] [Crates.io](https://crates.io/crates/pio-rs/0.3.0) [Github](https://github.com/rp-rs/pio-rs/releases/tag/v0.3.0)

- Added support for new PIO features in RP235x chips
- Reexport `pio_asm!` and `pio_file!` macros from the `pio` crate. You should not directly depend on `pio-proc` anymore.
- `pio_file!` now ensures the Rust program is recompiled when the referenced PIO file is modified.
- Added check to ensure argument for SET is not out of range.
- Fixed encoding of IN instruction with `bit_count` == 32

## [0.2.1] [Crates.io](https://crates.io/crates/pio-rs/0.2.1) [Github](https://github.com/rp-rs/pio-rs/releases/tag/v0.2.1)

- Fixed the search path for `pio_file` when using relative paths
- Check that the irq specified in a wait command is valid
- rename ParsedInstruction refiy method to reify
- Fix global directive newlines error
- Use (rel)ative bit for IRQ WaitSource
- disambiguate the use of pio_proc macros vs pio::Assembler
- Limit valid range if irqs for wait command
- Enable constant encoding for InstructionOperands
- Support `//` comments in .pio files

## [0.2.0] [Crates.io](https://crates.io/crates/pio-rs/0.2.0) [Github](https://github.com/rp-rs/pio-rs/releases/tag/v0.2.0)

- Updated syntax to allow for `.pio` files
- Updated pio asm macro with new syntax to follow `asm!`
- Add optional `rel` flag to index on `WAIT IRQ` instruction. (Breaking change, adds parameter to public data structures)

## [0.1.0] [Crates.io](https://crates.io/crates/pio-rs/0.1.0) [Github](https://github.com/rp-rs/pio-rs/releases/tag/v0.1.0)

- First release

[Unreleased]: https://github.com/rp-rs/pio-rs/compare/v0.2.1...HEAD
[0.2.1]: https://github.com/rp-rs/pio-rs/tag/v0.2.1
[0.2.0]: https://github.com/rp-rs/pio-rs/tag/v0.2.0
[0.1.0]: https://github.com/rp-rs/pio-rs/tag/v0.1.0
