use std::{fs, path::PathBuf};

/// Differential test: programs compiled by the re-implemented pipeline must
/// match what the legacy parser produces, field for field.
fn assert_matches_legacy(test: &str) {
    const MAX: usize = pio::RP2040_MAX_PROGRAM_SIZE;

    let path = PathBuf::from(test);
    let program_source = fs::read_to_string(&path).unwrap();

    let legacy = pio_parser::Parser::<MAX>::parse_file(&program_source)
        .unwrap_or_else(|e| panic!("{test}: legacy parser failed: {e:?}"));
    let pipeline = pio_parser::pipeline::compile_and_assemble::<MAX>(&program_source)
        .unwrap_or_else(|e| panic!("{test}: pipeline failed: {e:?}"));

    let mut legacy_names: Vec<&str> = legacy.keys().map(String::as_str).collect();
    let mut pipeline_names: Vec<&str> = pipeline.programs.keys().copied().collect();
    legacy_names.sort();
    pipeline_names.sort();
    assert_eq!(legacy_names, pipeline_names, "{test}: program sets");

    for name in legacy_names {
        let legacy_program = &legacy[name].program;
        let new = &pipeline.programs[name].program.program;

        assert_eq!(&*new.code, &*legacy_program.code, "{test}: {name}: code");
        assert_eq!(new.origin, legacy_program.origin, "{test}: {name}: origin");
        assert_eq!(new.wrap, legacy_program.wrap, "{test}: {name}: wrap");
        assert_eq!(
            new.version, legacy_program.version,
            "{test}: {name}: version"
        );
        assert_eq!(
            (
                new.side_set.optional(),
                new.side_set.bits(),
                new.side_set.pindirs()
            ),
            (
                legacy_program.side_set.optional(),
                legacy_program.side_set.bits(),
                legacy_program.side_set.pindirs()
            ),
            "{test}: {name}: side_set"
        );
        assert_eq!(
            &pipeline.programs[name].program.public_defines, &legacy[name].public_defines,
            "{test}: {name}: public defines"
        );
    }
}

#[test_generator::test_resources("tests/pico-examples/*.pio")]
fn pico_examples_match_legacy(test: &str) {
    // rmii_rx.pio uses `wait jmppin`, an RP2350-only instruction which the
    // pipeline does not support yet.
    if test.ends_with("rmii_rx.pio") {
        return;
    }
    assert_matches_legacy(test);
}

#[test]
fn test_pio_file_matches_legacy() {
    // Exercises public defines, public labels, origin and wrapping.
    assert_matches_legacy("tests/test.pio");
}
