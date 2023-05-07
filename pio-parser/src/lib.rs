// PIO instr grouping is 3/5/3/5
#![allow(clippy::unusual_byte_groupings)]
#![allow(clippy::upper_case_acronyms)]
#![deny(clippy::cast_sign_loss)]
#![deny(clippy::cast_possible_truncation)]

mod parser;
mod ast;

//#[derive(Debug, Default)]
//struct FileState {
//    defines: HashMap<String, (bool, i32)>,
//}

//#[derive(Debug)]
//struct ProgramState<'a> {
//    file_state: &'a mut FileState,
//    defines: HashMap<String, (bool, i32)>,
//}

//impl<'a> ProgramState<'a> {
//    fn new(file_state: &'a mut FileState) -> Self {
//        ProgramState {
//            file_state,
//            defines: HashMap::new(),
//        }
//    }

//    fn resolve(&self, name: &str) -> i32 {
//        self.defines
//            .get(name)
//            .or_else(|| self.file_state.defines.get(name))
//            .unwrap_or_else(|| panic!("Unknown label {}", name))
//            .1
//    }

//    fn public_defines(&self) -> HashMap<String, i32> {
//        let mut p = HashMap::new();
//        for (name, (public, value)) in &self.file_state.defines {
//            if *public {
//                p.insert(name.to_string(), *value);
//            }
//        }
//        for (name, (public, value)) in &self.defines {
//            if *public {
//                p.insert(name.to_string(), *value);
//            }
//        }
//        p
//    }
//}

//pub type ParseError<'input> = lalrpop_util::ParseError<usize, parser::Token<'input>, &'static str>;

//pub struct Parser<const PROGRAM_SIZE: usize>;

//impl<const PROGRAM_SIZE: usize> Parser<PROGRAM_SIZE> {
//    /// Parse a PIO "file", which contains some number of PIO programs
//    /// separated by `.program` directives.
//    pub fn parse_file(
//        source: &str,
//    ) -> Result<HashMap<String, ProgramWithDefines<HashMap<String, i32>, PROGRAM_SIZE>>, ParseError>
//    {
//        match parser::FileParser::new().parse(source) {
//            Ok(f) => {
//                let mut state = FileState::default();

//                // set up global defines
//                let fake_prog_state = ProgramState::new(&mut state);
//                for d in f.0 {
//                    if let ParsedDirective::Define {
//                        public,
//                        name,
//                        value,
//                    } = d.0
//                    {
//                        fake_prog_state
//                            .file_state
//                            .defines
//                            .insert(name.to_string(), (public, value.reify(&fake_prog_state)));
//                    }
//                }

//                Ok(f.1
//                    .iter()
//                    .map(|p| {
//                        let program_name = p.0.to_string();
//                        (program_name, Parser::process(&p.1, &mut state))
//                    })
//                    .collect())
//            }
//            Err(e) => Err(e),
//        }
//    }

//    /// Parse a single PIO program, without the `.program` directive.
//    pub fn parse_program(
//        source: &str,
//    ) -> Result<ProgramWithDefines<HashMap<String, i32>, PROGRAM_SIZE>, ParseError> {
//        parser::ProgramParser::new()
//            .parse(source)
//            .map(|p| Parser::process(&p, &mut FileState::default()))
//    }

//    fn process(
//        p: &[Line],
//        file_state: &mut FileState,
//    ) -> ProgramWithDefines<HashMap<String, i32>, PROGRAM_SIZE> {
//        let mut state = ProgramState::new(file_state);

//        // first pass
//        //   - resolve labels
//        //   - resolve defines
//        //   - read side set settings
//        let mut side_set_size = 0;
//        let mut side_set_opt = false;
//        let mut side_set_pindirs = false;
//        let mut origin = None;
//        let mut wrap_target = None;
//        let mut wrap = None;
//        let mut instr_index = 0;
//        for line in p {
//            match line {
//                Line::Instruction(..) => {
//                    instr_index += 1;
//                }
//                Line::Label { public, name } => {
//                    state
//                        .defines
//                        .insert(name.to_string(), (*public, instr_index as i32));
//                }
//                Line::Directive(d) => match d {
//                    ParsedDirective::Define {
//                        public,
//                        name,
//                        value,
//                    } => {
//                        state
//                            .defines
//                            .insert(name.to_string(), (*public, value.reify(&state)));
//                    }
//                    ParsedDirective::Origin(value) => {
//                        origin = Some(value.reify(&state) as u8);
//                    }
//                    ParsedDirective::SideSet {
//                        value,
//                        opt,
//                        pindirs,
//                    } => {
//                        assert!(instr_index == 0);
//                        side_set_size = value.reify(&state) as u8;
//                        side_set_opt = *opt;
//                        side_set_pindirs = *pindirs;
//                    }
//                    ParsedDirective::WrapTarget => {
//                        assert!(wrap_target.is_none());
//                        wrap_target = Some(instr_index);
//                    }
//                    ParsedDirective::Wrap => {
//                        assert!(wrap.is_none());
//                        wrap = Some(instr_index - 1);
//                    }
//                    _ => {}
//                },
//            }
//        }

//        let mut a = pio::Assembler::new_with_side_set(pio::SideSet::new(
//            side_set_opt,
//            side_set_size,
//            side_set_pindirs,
//        ));

//        // second pass
//        //   - emit instructions
//        for line in p {
//            if let Line::Instruction(i) = line {
//                a.instructions.push(i.reify(&state));
//            }
//        }

//        let program = a.assemble_program().set_origin(origin);

//        let program = match (wrap, wrap_target) {
//            (Some(wrap_source), Some(wrap_target)) => program.set_wrap(pio::Wrap {
//                source: wrap_source,
//                target: wrap_target,
//            }),
//            (None, None) => program,
//            _ => panic!(
//                "must define either both or neither of wrap and wrap_target, but not only one of them"
//            ),
//        };

//        ProgramWithDefines {
//            program,
//            public_defines: state.public_defines(),
//        }
//    }
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
//    .side_set 1 opt
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
//#[should_panic(expected = "Unknown label some_unknown_label")]
//fn test_unknown_label() {
//    let _ = Parser::<32>::parse_program(
//        "
//    jmp some_unknown_label
//    ",
//    )
//    .unwrap();
//}
