use std::{fs, path::PathBuf};

/// Checks that the `pipeline` module can parse and compile all the
/// pico-examples.
///
/// The end-to-end comparison against the reference `.hex` encodings happens
/// in `pico-examples.rs` (through the public [`pio_parser::Parser`] API) and
/// in `pipeline_asm.rs` (through the pipeline API); field-level expectations
/// for the other corpus files live in `pipeline_golden.rs`.
#[test_generator::test_resources("tests/pico-examples/*.pio")]
fn pipeline_parse_and_compile(test: &str) {
    let path = PathBuf::from(test);
    let program_source = fs::read_to_string(&path).unwrap();

    let file = pio_parser::pipeline::compile(&program_source)
        .unwrap_or_else(|e| panic!("{test}: compilation failed: {e:?}"));

    assert!(
        !file.programs.is_empty(),
        "{test}: no programs were compiled"
    );
}

#[test]
fn pipeline_compiles_case_test() {
    let program_source = fs::read_to_string("tests/case_test.pio").unwrap();
    let file =
        pio_parser::pipeline::compile(&program_source).expect("case_test.pio: compilation failed");
    assert_eq!(file.programs.len(), 4);
}
