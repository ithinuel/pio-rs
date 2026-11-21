use std::{collections::HashMap, fs};

use pio_core::PioVersion;

const MAX: usize = pio::RP2040_MAX_PROGRAM_SIZE;

/// Field-for-field snapshot of an assembled program so that expectations can
/// be written inline.
///
/// The goldens below were captured while the legacy front-end still existed
/// and was diff-tested against this pipeline; programs which also have pioasm
/// `.hex` references are additionally checked in `pipeline_asm.rs` and
/// `pico-examples.rs`.
#[derive(Debug, PartialEq, Eq)]
struct ProgramSnapshot {
    code: Vec<u16>,
    origin: Option<u8>,
    wrap: (u8, u8),
    side_set: (bool, u8, bool),
    version: PioVersion,
    public_defines: Vec<(String, i32)>,
}

fn snapshots(source: &str) -> HashMap<String, ProgramSnapshot> {
    let file = pio_parser::pipeline::compile_and_assemble::<MAX>(source)
        .unwrap_or_else(|e| panic!("pipeline failed: {}", e.message()));

    file.programs
        .iter()
        .map(|(name, assembled)| {
            let program = &assembled.program;
            let mut public_defines: Vec<_> = program
                .public_defines
                .iter()
                .map(|(name, value)| (name.clone(), *value))
                .collect();
            public_defines.sort();

            (
                name.to_string(),
                ProgramSnapshot {
                    code: program.program.code.iter().copied().collect(),
                    origin: program.program.origin,
                    wrap: (program.program.wrap.source, program.program.wrap.target),
                    side_set: (
                        program.program.side_set.optional(),
                        program.program.side_set.bits(),
                        program.program.side_set.pindirs(),
                    ),
                    version: program.program.version,
                    public_defines,
                },
            )
        })
        .collect()
}

fn expected(
    code: &[u16],
    origin: Option<u8>,
    wrap: (u8, u8),
    side_set: (bool, u8, bool),
    version: PioVersion,
    public_defines: &[(&str, i32)],
) -> ProgramSnapshot {
    ProgramSnapshot {
        code: code.to_vec(),
        origin,
        wrap,
        side_set,
        version,
        public_defines: public_defines
            .iter()
            .map(|(name, value)| (name.to_string(), *value))
            .collect(),
    }
}

// Exercises public defines, public labels, origin and wrapping.
#[test]
fn test_pio_golden() {
    let snapshots = snapshots(&fs::read_to_string("tests/test.pio").unwrap());

    let expect = expected(
        &[0x0000, 0x0000],
        Some(5),
        (0, 0),
        (false, 0, false),
        PioVersion::V0,
        &[("foo", 3), ("label", 0), ("owo", 2)],
    );
    assert_eq!(snapshots.len(), 2);
    assert_eq!(snapshots["test"], expect);
    assert_eq!(snapshots["test2"], expect);
}

// Exercises case-insensitive instructions, directives and keywords, and
// `.lang_opt`-free side-set programs.
#[test]
fn case_test_golden() {
    let snapshots = snapshots(&fs::read_to_string("tests/case_test.pio").unwrap());

    let reference = expected(
        &[
            0x0000, 0x0000, 0xa006, 0xa0e1, 0x2081, 0xa042, 0x20a1, 0x20c1, 0x4061, 0x60a1, 0x8020,
            0x8060, 0x80e0, 0x8080, 0xa085, 0xc023, 0xc002, 0xc041, 0xc001, 0xe080, 0x0020, 0x00e0,
        ],
        None,
        (18, 4),
        (false, 0, false),
        PioVersion::V0,
        &[("label", 0), ("me_a_symbol", 2)],
    );
    assert_eq!(snapshots["case_reference"], reference);
    // The same program written with varied casing assembles identically.
    assert_eq!(snapshots["variable_case"], reference);

    let side = expected(
        &[0x6001],
        Some(5),
        (0, 0),
        (false, 1, false),
        PioVersion::V0,
        &[("me_a_symbol", 2)],
    );
    assert_eq!(snapshots["variable_case_side"], side);
    // `.SIDE_SET 1` in upper case gives the same side-set encoding.
    assert_eq!(snapshots["variable_case_side_ref"], side);
}

// Exercises RP2350-only instructions.
#[test]
fn rp2350_golden() {
    let snapshots = snapshots(&fs::read_to_string("tests/rp2350.pio").unwrap());

    let expect = expected(
        &[
            0x204a, 0x20db, 0x20d4, 0x20e0, 0x24e8, 0x8010, 0x8018, 0x8099, 0xc00b, 0xc01c, 0xc015,
            0xa061, 0xa042,
        ],
        None,
        (12, 0),
        (false, 0, false),
        PioVersion::V1, // the program requires the RP2350
        &[],
    );
    assert_eq!(snapshots["rp2350"], expect);
}
