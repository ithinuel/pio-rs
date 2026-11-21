use std::{fs, path::PathBuf};

/// Checks that the re-implementation of the parser (the `pipeline` module)
/// can parse and compile all the pico-examples.
///
/// The end-to-end comparison against the reference `.hex` encodings happens
/// in `pico-examples.rs` for the legacy parser; it will be replicated for the
/// pipeline once it can assemble programs.
#[test_generator::test_resources("tests/pico-examples/*.pio")]
fn pipeline_parse_and_compile(test: &str) {
    // rmii_rx.pio uses `wait jmppin`, an RP2350-only instruction which the
    // pipeline does not support yet.
    if test.ends_with("rmii_rx.pio") {
        return;
    }

    let path = PathBuf::from(test);
    let program_source = fs::read_to_string(&path).unwrap();

    let file = pio_parser::pipeline::compile(&program_source)
        .unwrap_or_else(|e| panic!("{test}: compilation failed: {e:?}"));

    assert!(
        !file.programs.is_empty(),
        "{test}: no programs were compiled"
    );
}
