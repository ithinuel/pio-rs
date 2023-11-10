//use super::Parser;

//#[test]
//fn single_line_label_instr() {
//    Parser::<{ pio::RP2040_MAX_PROGRAM_SIZE }>::parse_program("bar: nop\n").unwrap();
//}

//#[test]
//fn ignore_various_comment_styles() {
//    Parser::<{ pio::RP2040_MAX_PROGRAM_SIZE }>::parse_program(
//        r"
//    // c-style eol comment
/*
 * c-style block comment
 */
//    // python-style eol comment
//    bar: nop
//    ",
//    )
//    .unwrap();
//}

//#[test]
//fn test() {
//    let p = Parser::<32>::parse_program(
//        "
//    label:
//      pull
//      out pins, 1
//      jmp label
//    ",
//    )
//    .unwrap();

//    assert_eq!(
//        &p.program.code[..],
//        &[
//            // LABEL:
//            0b100_00000_101_00000, // PULL
//            0b011_00000_000_00001, // OUT PINS, 1
//            0b000_00000_000_00000, // JMP LABEL
//        ]
//    );
//    assert_eq!(p.program.origin, None);
//    assert_eq!(
//        p.program.wrap,
//        pio::Wrap {
//            source: 2,
//            target: 0,
//        }
//    );
//}

//#[test]
//fn test_side_set() {
//    let p = Parser::<32>::parse_program(
//        "
//.side_set 1 opt
//    .origin 5

//    label:
//      pull
//      .wrap_target
//      out pins, 1
//      .wrap
//      jmp label side 1
//    ",
//    )
//    .unwrap();

//    assert_eq!(
//        &p.program.code[..],
//        &[
//            // LABEL:
//            0b100_00000_101_00000, // PULL
//            0b011_00000_000_00001, // OUT PINS, 1
//            0b000_11000_000_00000, // JMP LABEL, SIDE 1
//        ]
//    );
//    assert_eq!(p.program.origin, Some(5));
//    assert_eq!(
//        p.program.wrap,
//        pio::Wrap {
//            source: 1,
//            target: 1,
//        }
//    );
//}

//#[test]
//fn test_lang_opt() {
//    let _ = Parser::<32>::parse_program(
//        "
//    nop
//    .lang_opt python yes = 32
//    ",
//    )
//    .unwrap();
//}

//#[test]
//#[should_panic(expected = "Unknown label some_unknown_label")]
//fn test_unknown_label() {
//    let _ = Parser::<32>::parse_program(
//        "
//    jmp some_unknown_label
//    ",
//    )
//    .unwrap();
//}

//#[test]
//#[ignore]
//#[should_panic(expected = "Circular dependency in definition of `a_symbol`")]
//fn test_circular_deps() {
//    let _ = Parser::<32>::parse_program(
//        "
//    .define a_symbol 42 + another_symbol
//    .define another_symbol a_symbol - 42
//    ",
//    )
//    .unwrap();
//}

//#[test]
//fn single_line_label_instr() {
//    Parser::<{ pio::RP2040_MAX_PROGRAM_SIZE }>::parse_program("bar: nop\n").unwrap();
//}

//#[test]
//fn ignore_various_comment_styles() {
//    Parser::<{ pio::RP2040_MAX_PROGRAM_SIZE }>::parse_program(r"
//    // c-style eol comment
/*
 * c-style block comment
 */
//    // python-style eol comment
//    bar: nop
//    ").unwrap();
//}

//#[test]
//fn code_blocks() {
//    Parser::<{ pio::RP2040_MAX_PROGRAM_SIZE }>::parse_program(r"
//nop
//% c-sdk {
//some blob of code with possible % and } or even %} provided it is not alone on a line. In such case
//it marks the end of the code block.
//%} right ?
//% not-an actual start {
//Yes indeed.
//%} # However, while spaces are accepted, comments are not allowed on an end-of-code-block-marker
//%}    ").unwrap();
//}
