#![allow(clippy::all)]
include!(concat!(env!("OUT_DIR"), "/pipeline/grammar.rs"));

pub type Error<'i> = lalrpop_util::ParseError<usize, lexer::Token<'i>, lexer::Error>;

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::pipeline::{parse, Line, Location};

    const COMMA: [&'static str; 2] = ["", ","];
    const BASE: crate::pipeline::Instruction = crate::pipeline::Instruction {
        ops: crate::pipeline::InstructionOperands::Nop,
        delay: None,
        side_set: None,
    };

    fn assert<'i, V, U>(pairs: U)
    where
        U: IntoIterator<Item = &'i (V, Line<'i>)>,
        V: AsRef<str> + std::fmt::Debug + 'i,
    {
        for (input, expected) in pairs {
            println!("Testing: {input:?}");
            assert_eq!(
                parse(input.as_ref()).expect("Parse failure").collect_vec(),
                &[expected.clone()],
                "on input: {:?}",
                input
            );
        }
    }

    #[test]
    fn parse_program_directive() {
        let res = parse(".program hello_world")
            .expect("Parsing failure")
            .collect_vec();
        assert_eq!(
            res,
            [Line::Program {
                name: "hello_world",
                location: Location(9..20)
            }]
        )
    }

    #[test]
    fn parse_fifo_directive() {
        use crate::pipeline::{Directive, FifoConfig};
        let test_set = [
            (".fifo txrx", FifoConfig::Txrx),
            (".fifo tx", FifoConfig::Tx),
            (".fifo rx", FifoConfig::Rx),
            (".fifo TXPUT", FifoConfig::Txput),
            (".Fifo TxGet", FifoConfig::Txget),
            (".fifo putget", FifoConfig::Putget),
        ];
        assert(
            &test_set
                .into_iter()
                .map(|(src, config)| {
                    (
                        src,
                        Line::Directive(Directive::Fifo(Location(0..5), config)),
                    )
                })
                .collect_vec(),
        );
    }

    #[test]
    fn parse_pio_version_directive() {
        use crate::pipeline::Directive;
        let test_set = [
            (".pio_version 0", 0),
            (".pio_version 1", 1),
            (".PIO_VERSION RP2040", 0),
            (".pio_version rp2350", 1),
        ];
        assert(
            &test_set
                .into_iter()
                .map(|(src, version)| {
                    (
                        src,
                        Line::Directive(Directive::PioVersion(Location(0..12), version)),
                    )
                })
                .collect_vec(),
        );
    }

    #[test]
    fn parse_clock_div_directive() {
        use crate::pipeline::{ClockDiv, Directive};
        let test_set = [
            (
                ".clock_div 1",
                ClockDiv {
                    integer: 1,
                    fractional: 0,
                },
            ),
            (
                ".clock_div 1.5",
                ClockDiv {
                    integer: 1,
                    fractional: 128,
                },
            ),
            (
                ".clock_div .5",
                ClockDiv {
                    integer: 0,
                    fractional: 128,
                },
            ),
            // The fractional part is truncated to 8 bits.
            (
                ".clock_div 10.003",
                ClockDiv {
                    integer: 10,
                    fractional: 0,
                },
            ),
        ];
        assert(
            &test_set
                .into_iter()
                .map(|(src, clock_div)| {
                    (
                        src,
                        Line::Directive(Directive::ClockDiv(Location(0..10), clock_div)),
                    )
                })
                .collect_vec(),
        );
    }

    #[test]
    fn parse_mov_status_directive() {
        use crate::pipeline::Value::Integer;
        use crate::pipeline::{Directive, MovStatus, MovStatusIrqSource as Source};
        let test_set = [
            (".mov_status txfifo < 4", MovStatus::TxFifoBelow(Integer(4))),
            (".mov_status RXFIFO<0", MovStatus::RxFifoBelow(Integer(0))),
            (
                ".mov_status irq set 3",
                MovStatus::IrqSet {
                    index: Integer(3),
                    source: Source::This,
                },
            ),
            (
                ".mov_status irq prev set 4",
                MovStatus::IrqSet {
                    index: Integer(4),
                    source: Source::Prev,
                },
            ),
            (
                ".mov_status irq NEXT set 5",
                MovStatus::IrqSet {
                    index: Integer(5),
                    source: Source::Next,
                },
            ),
        ];
        assert(
            &test_set
                .into_iter()
                .map(|(src, source)| {
                    (
                        src,
                        Line::Directive(Directive::MovStatus(Location(0..11), source)),
                    )
                })
                .collect_vec(),
        );
    }

    #[test]
    fn parse_in_out_set_directives() {
        use crate::pipeline::{Directive, ShiftDirection::*, Value::Integer};
        assert(&[
            (
                ".in 32",
                Line::Directive(Directive::In {
                    location: Location(0..3),
                    count: Integer(32),
                    direction: Right,
                    auto: false,
                    threshold: Integer(32),
                }),
            ),
            (
                ".in 4 left auto 8",
                Line::Directive(Directive::In {
                    location: Location(0..3),
                    count: Integer(4),
                    direction: Left,
                    auto: true,
                    threshold: Integer(8),
                }),
            ),
            (
                ".in 8 RIGHT MANUAL",
                Line::Directive(Directive::In {
                    location: Location(0..3),
                    count: Integer(8),
                    direction: Right,
                    auto: false,
                    threshold: Integer(32),
                }),
            ),
            (
                ".out 16 left",
                Line::Directive(Directive::Out {
                    location: Location(0..4),
                    count: Integer(16),
                    direction: Left,
                    auto: false,
                    threshold: Integer(32),
                }),
            ),
            (
                ".out 0 auto 1",
                Line::Directive(Directive::Out {
                    location: Location(0..4),
                    count: Integer(0),
                    direction: Right,
                    auto: true,
                    threshold: Integer(1),
                }),
            ),
            (
                ".set 3",
                Line::Directive(Directive::SetCount(Location(0..4), Integer(3))),
            ),
        ]);
    }

    #[test]
    fn parse_label() {
        use crate::pipeline::SymbolDef;
        use std::collections::HashSet;

        let mut label = SymbolDef {
            public: true,
            is_label: true,
            name: "label",
            location: Location(7..12),
        };

        let res = parse("public label:").expect("Parsing failure");
        assert_eq!(res.collect_vec(), [Line::Label(label.clone())]);

        label.public = false;
        label.location = Location(0..5);
        let res = parse("label:").expect("Parsing failure");
        assert_eq!(res.collect_vec(), [Line::Label(label.clone())]);

        let res = parse("label: label2:")
            .err()
            .expect("Expected parse failure");
        let expected = [
            "EoF", "in_", "irq", "jmp", "mov", "newline", "nop", "out", "pull", "push", "set",
            "wait",
        ]
        .into_iter()
        .map(str::to_string)
        .collect::<HashSet<_>>();
        match res {
            lalrpop_util::ParseError::UnrecognizedToken {
                expected: result, ..
            } => {
                assert_eq!(result.into_iter().collect::<HashSet<_>>(), expected);
            }
            _ => panic!("Invalid error type"),
        }
    }

    #[test]
    fn code_blocks() {
        let blob = r"some blob of code with possible % and } or even %} provided it is not alone on a line. In such case
it marks the end of the code block.
%} right ?
% not-an actual start {
Yes indeed.
%} # However, while spaces are accepted, comments are not allowed on an end-of-code-block-marker";

        let formatted = format!(
            r"% {} {{{}
%}}     ",
            "c-sdk", blob
        );

        let res = parse(&formatted).unwrap();
        assert_eq!(res.collect_vec(), &[Line::CodeBlock("c-sdk", blob)])
    }

    #[test]
    fn directive_wraps() {
        use crate::pipeline::{
            Directive::{Wrap, WrapTarget},
            Line::Directive,
        };

        let res = parse(".wrap").unwrap();
        assert_eq!(res.collect_vec(), &[Directive(Wrap(Location(0..5)))]);

        let res = parse(".wrap_target").unwrap();
        assert_eq!(res.collect_vec(), &[Directive(WrapTarget(Location(0..12)))]);
    }

    #[test]
    fn parse_expressions() {
        use crate::pipeline::{Directive::*, Expression::*, Line::*, Value::*};

        let origin = |expr| Directive(Origin(Location(0..7), expr));

        assert(&[
            (".origin 23", origin(Integer(23))),
            (
                ".origin ident",
                origin(Identifier(Location(8..13), "ident")),
            ),
            (
                ".origin (1+2)",
                origin(Expression(
                    Location(8..13),
                    Plus(Value(Integer(1)).boxed(), Value(Integer(2)).boxed()).boxed(),
                )),
            ),
            (
                ".origin (3+4+5)",
                origin(Expression(
                    Location(8..15),
                    Plus(
                        Plus(Value(Integer(3)).boxed(), Value(Integer(4)).boxed()).boxed(),
                        Value(Integer(5)).boxed(),
                    )
                    .boxed(),
                )),
            ),
            (
                ".origin (6*7+8)",
                origin(Expression(
                    Location(8..15),
                    Plus(
                        Multiply(Value(Integer(6)).boxed(), Value(Integer(7)).boxed()).boxed(),
                        Value(Integer(8)).boxed(),
                    )
                    .boxed(),
                )),
            ),
            (
                ".origin (9*10+11*12*13)",
                origin(Expression(
                    Location(8..23),
                    Plus(
                        Multiply(Value(Integer(9)).boxed(), Value(Integer(10)).boxed()).boxed(),
                        Multiply(
                            Multiply(Value(Integer(11)).boxed(), Value(Integer(12)).boxed())
                                .boxed(),
                            Value(Integer(13)).boxed(),
                        )
                        .boxed(),
                    )
                    .boxed(),
                )),
            ),
            (
                ".origin (9*10+var1*-12*13-24*::(var2))",
                origin(Expression(
                    Location(8..38),
                    Minus(
                        Plus(
                            Multiply(Value(Integer(9)).boxed(), Value(Integer(10)).boxed()).boxed(),
                            Multiply(
                                Multiply(
                                    Value(Identifier(Location(14..18), "var1")).boxed(),
                                    Opposite(Value(Integer(12)).boxed()).boxed(),
                                )
                                .boxed(),
                                Value(Integer(13)).boxed(),
                            )
                            .boxed(),
                        )
                        .boxed(),
                        Multiply(
                            Value(Integer(24)).boxed(),
                            Reverse(Value(Identifier(Location(32..36), "var2")).boxed()).boxed(),
                        )
                        .boxed(),
                    )
                    .boxed(),
                )),
            ),
        ]);

        assert!(
            parse(".define foo 3+::var*2").is_err(),
            "Reverse syntax in pio-rs should be `::( expr )`"
        )
    }

    #[test]
    fn parse_define() {
        use crate::pipeline::{
            Directive::Define, Expression::Value, Line::Directive, SymbolDef, Value::Integer,
        };

        assert(&[(
            ".define public var 32",
            Directive(Define(
                SymbolDef {
                    public: true,
                    is_label: false,
                    name: "var",
                    location: Location(15..18),
                },
                Value(Integer(32)),
            )),
        )]);
    }

    #[test]
    fn parse_sideset() {
        use crate::pipeline::{Directive::SideSet, Line::Directive, Value::Integer};
        let side_set = |optional, pindirs| {
            Directive(SideSet {
                location: Location(0..9),
                value: Integer(32),
                optional,
                pindirs,
            })
        };

        assert(&[
            (".side_set 32", side_set(false, false)),
            (".side_set 32 opt", side_set(true, false)),
            (".side_set 32 pindirs", side_set(false, true)),
            (".side_set 32 opt pindirs", side_set(true, true)),
        ]);
    }

    #[test]
    fn parse_word() {
        use crate::pipeline::{Directive::Word, Line::Directive, Value::Integer};

        assert(&[(".word 42", Directive(Word(Location(0..5), Integer(42))))]);
    }

    #[test]
    fn parse_lang_opt() {
        use crate::pipeline::{Directive::*, Line::*};

        assert(&[
            (
                ".lang_opt python foo = something.with.no.white_space::bar()",
                Directive(LangOpt {
                    location: Location(0..9),
                    lang: "python",
                    var: "foo",
                    val: "something.with.no.white_space::bar()",
                }),
            ),
            (
                r#".lang_opt rust foo = "Some string with spaces in it" "#,
                Directive(LangOpt {
                    location: Location(0..9),
                    lang: "rust",
                    var: "foo",
                    val: "\"Some string with spaces in it\"",
                }),
            ),
            (
                r#".lang_opt what_ever fizzbarbaz = 0x42"#,
                Directive(LangOpt {
                    location: Location(0..9),
                    lang: "what_ever",
                    var: "fizzbarbaz",
                    val: "0x42",
                }),
            ),
        ]);
    }

    #[test]
    fn parse_instruction_delay_and_side() {
        use crate::pipeline::{Expression::*, Instruction, InstructionOperands, Line::*, Value::*};

        let forty_two = Some(Value(Integer(42)));
        let twelve = Some(Integer(12));
        let nop = |delay, side_set| {
            Instruction(
                Location(0..3),
                Instruction {
                    ops: InstructionOperands::Nop,
                    delay,
                    side_set,
                },
            )
        };

        assert(&[
            ("NOP", nop(None, None)),
            ("nop", nop(None, None)),
            ("nop [42]", nop(forty_two.clone(), None)),
            ("nop side 12", nop(None, twelve.clone())),
            ("nop side 12 [42]", nop(forty_two.clone(), twelve.clone())),
            ("nop [42] side 12", nop(forty_two, twelve)),
        ]);
    }

    #[test]
    fn parse_instruction_wait() {
        use crate::pipeline::{Instruction, InstructionOperands, Line::*, Value::*, WaitSource::*};

        let wait = |polarity, src| {
            Instruction(
                Location(0..4),
                Instruction {
                    ops: InstructionOperands::Wait { polarity, src },
                    ..BASE
                },
            )
        };

        let polarity = [("1", Integer(1)), ("0", Integer(0))];
        let source = ["pin", "gpio", "irq"];
        let value = [("1", Integer(1)), ("26", Integer(26))];
        let relative = ["", " rel"];

        let test_set = itertools::iproduct!(polarity, COMMA, source, COMMA, value, COMMA, relative)
            .filter(|(_, _, s, _, _, c, r)| {
                ((*s != "irq") && c.is_empty() && r.is_empty())
                    || (*s == "irq" && (c.is_empty() || !r.is_empty()))
            })
            .map(|((p, ep), c1, s, c2, (v, ev), c3, r)| {
                let input = format!("wait {p}{c1} {s}{c2} {v}{c3}{r}");
                let expected = (
                    ep,
                    match s {
                        "pin" => Pin(ev),
                        "gpio" => Gpio(ev),
                        "irq" => Irq(
                            ev,
                            if r.is_empty() {
                                crate::pipeline::IrqIndexMode::Direct
                            } else {
                                crate::pipeline::IrqIndexMode::Rel
                            },
                        ),
                        _ => unreachable!(),
                    },
                );
                (input, expected)
            })
            .map(|(input, (polarity, src))| (input, wait(polarity, src)))
            .collect_vec();
        assert(&test_set);
    }

    #[test]
    fn parse_instruction_wait_jmppin() {
        use crate::pipeline::{
            Expression, Instruction, InstructionOperands, Line::*, Value::*, WaitSource::*,
        };

        let wait = |polarity, src| {
            Instruction(
                Location(0..4),
                Instruction {
                    ops: InstructionOperands::Wait { polarity, src },
                    ..BASE
                },
            )
        };

        let test_set = [
            ("wait 0 jmppin", Integer(0), JmpPin(None)),
            ("wait 1 jmppin", Integer(1), JmpPin(None)),
            ("wait 1 jmppin + 8", Integer(1), JmpPin(Some(Integer(8)))),
            (
                "wait 1, jmppin, + (4+4)",
                Integer(1),
                JmpPin(Some(Expression(
                    Location(18..23),
                    Expression::Plus(
                        Expression::Value(Integer(4)).boxed(),
                        Expression::Value(Integer(4)).boxed(),
                    )
                    .boxed(),
                ))),
            ),
        ]
        .into_iter()
        .map(|(input, polarity, src)| (input.to_string(), wait(polarity, src)))
        .collect_vec();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_wait_irq_index_modes() {
        use crate::pipeline::{
            Instruction, InstructionOperands, IrqIndexMode, Line::*, Value::*, WaitSource::*,
        };

        let wait = |irq, index_mode| {
            Instruction(
                Location(0..4),
                Instruction {
                    ops: InstructionOperands::Wait {
                        polarity: Integer(1),
                        src: Irq(Integer(irq), index_mode),
                    },
                    ..BASE
                },
            )
        };

        use IrqIndexMode::*;
        let test_set = [
            ("wait 1 irq 2", Direct),
            ("wait 1 irq prev 2", Prev),
            ("wait 1 irq rel 2", Rel),
            ("wait 1 irq next 2", Next),
            ("wait 1 irq 2 rel", Rel),
            ("wait 1 irq 2 prev", Prev),
            ("wait 1 irq 2 next", Next),
            // A trailing index mode overrides the prefixed one.
            ("wait 1 irq prev 2 rel", Rel),
            ("wait 1 irq next 2 rel", Rel),
            ("wait 1 irq prev 2 next", Next),
        ]
        .into_iter()
        .map(|(input, index_mode)| (input.to_string(), wait(2, index_mode)))
        .collect_vec();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_irq_index_modes() {
        use crate::pipeline::{
            Instruction, InstructionOperands::Irq, IrqIndexMode, IrqModifier, Line::*,
            Value::Integer,
        };
        let irq = |ops| {
            Instruction(
                Location(0..3),
                Instruction {
                    ops,
                    delay: None,
                    side_set: None,
                },
            )
        };

        use IrqIndexMode::*;
        let test_set = [
            ("irq 3", IrqModifier::Set, Direct),
            ("irq 3 prev", IrqModifier::Set, Prev),
            ("irq 3 rel", IrqModifier::Set, Rel),
            ("irq 3 next", IrqModifier::Set, Next),
            ("irq wait 3 prev", IrqModifier::SetWait, Prev),
            ("irq clear 3 next", IrqModifier::Clear, Next),
        ]
        .into_iter()
        .map(|(input, modifier, index_mode)| {
            (
                input.to_string(),
                irq(Irq {
                    modifier,
                    value: Integer(3),
                    index_mode,
                }),
            )
        })
        .collect_vec();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_mov_rxfifo() {
        use crate::pipeline::{
            Instruction, InstructionOperands::Mov, Line::*, MovOp, MovSource, MovTarget,
            RxFifoIndex,
        };
        let mov = |ops| {
            Instruction(
                Location(0..3),
                Instruction {
                    ops,
                    delay: None,
                    side_set: None,
                },
            )
        };

        let test_set = [
            (
                "mov rxfifo[y], isr",
                Mov {
                    trg: MovTarget::Rxfifo(RxFifoIndex::Y),
                    op: None,
                    src: MovSource::Isr,
                },
            ),
            (
                "mov RXFIFO[0], ISR",
                Mov {
                    trg: MovTarget::Rxfifo(RxFifoIndex::_0),
                    op: None,
                    src: MovSource::Isr,
                },
            ),
            (
                "mov rxfifo[1], isr",
                Mov {
                    trg: MovTarget::Rxfifo(RxFifoIndex::_1),
                    op: None,
                    src: MovSource::Isr,
                },
            ),
            (
                "mov rxfifo[2], isr",
                Mov {
                    trg: MovTarget::Rxfifo(RxFifoIndex::_2),
                    op: None,
                    src: MovSource::Isr,
                },
            ),
            (
                "mov rxfifo[3], isr",
                Mov {
                    trg: MovTarget::Rxfifo(RxFifoIndex::_3),
                    op: None,
                    src: MovSource::Isr,
                },
            ),
            (
                "mov osr, rxfifo[y]",
                Mov {
                    trg: MovTarget::Osr,
                    op: None,
                    src: MovSource::Rxfifo(RxFifoIndex::Y),
                },
            ),
            (
                "mov osr, rxfifo[3]",
                Mov {
                    trg: MovTarget::Osr,
                    op: None,
                    src: MovSource::Rxfifo(RxFifoIndex::_3),
                },
            ),
            (
                // The mov op is silently dropped for rxfifo instructions,
                // like the legacy parser does.
                "mov rxfifo[y], ::isr",
                Mov {
                    trg: MovTarget::Rxfifo(RxFifoIndex::Y),
                    op: Some(MovOp::Reverse),
                    src: MovSource::Isr,
                },
            ),
        ]
        .into_iter()
        .map(|(input, ops)| (input.to_string(), mov(ops)))
        .collect_vec();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_in() {
        use crate::pipeline::InSource::*;

        let src = [
            ("pins", Pins),
            ("x", X),
            ("y", Y),
            ("null", Null),
            ("isr", Isr),
            ("osr", Osr),
        ];

        let test_set = generate_in_and_out_test_set("in", src, |source, bit_count| {
            crate::pipeline::InstructionOperands::In {
                src: source,
                bit_count,
            }
        });

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_out() {
        use crate::pipeline::OutTarget::*;

        let trg = [
            ("pins", Pins),
            ("x", X),
            ("y", Y),
            ("null", Null),
            ("pindirs", PinDirs),
            ("isr", Isr),
            ("pc", Pc),
            ("exec", Exec),
        ];

        let test_set = generate_in_and_out_test_set("out", trg, |target, bit_count| {
            crate::pipeline::InstructionOperands::Out { target, bit_count }
        });
        assert(&test_set);
    }

    fn generate_in_and_out_test_set<'i, T: Clone>(
        cmd: &str,
        v: impl IntoIterator<Item = (&'static str, T)> + Clone,
        mut f: impl for<'u> FnMut(
            T,
            crate::pipeline::Value<'u>,
        ) -> crate::pipeline::InstructionOperands<'u>,
    ) -> Vec<(String, Line<'i>)> {
        use crate::pipeline::{Expression::*, Instruction, Line::*, Value::*};

        let instr = |len, ops| Instruction(Location(0..len), Instruction { ops, ..BASE });
        let bit_count = [
            ("0", Integer(0)),
            (
                "(0+5)",
                Expression(
                    Location(0..5),
                    Plus(Value(Integer(0)).boxed(), Value(Integer(5)).boxed()).boxed(),
                ),
            ),
        ];
        itertools::iproduct!(v, COMMA, bit_count)
            .map(|((in_str, val), comma, (in_bit_count, mut bit_count))| {
                let input = format!("{cmd} {in_str}{comma} {in_bit_count}");
                match &mut bit_count {
                    Expression(loc, ..) => {
                        let b = cmd.len() + 1 + in_str.len() + 1 + comma.len() + loc.0.start;
                        let e = cmd.len() + 1 + in_str.len() + 1 + comma.len() + loc.0.end;
                        *loc = Location(b..e)
                    }
                    _ => {}
                }
                let expected = instr(cmd.len(), f(val, bit_count));

                (input, expected)
            })
            .collect()
    }

    #[test]
    fn parse_instruction_jmp() {
        use crate::pipeline::{
            Expression::Value, Instruction, JmpCondition::*, Line::*, Value::Integer,
        };
        let jmp = |ops| Instruction(Location(0..3), Instruction { ops, ..BASE });

        let src = [
            ("! x", Some(NotX)),
            ("x--", Some(XPostDec)),
            ("!y", Some(NotY)),
            ("y−−", Some(YPostDec)),
            ("x!= y", Some(XNotEqualY)),
            ("pin", Some(Pin)),
            ("! osre", Some(OSRNotEmpty)),
            ("", None),
        ];
        let test_set = itertools::iproduct!(src, COMMA)
            .map(|((in_cond, condition), comma)| {
                let input = format!("jmp {in_cond}{comma} 0");
                let expected = jmp(crate::pipeline::InstructionOperands::Jmp {
                    condition,
                    target: Value(Integer(0)),
                });
                (input, expected)
            })
            .collect_vec();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_push_pull() {
        use crate::pipeline::{
            Instruction,
            InstructionOperands::{Pull, Push},
            Line::*,
        };

        let blocking = [(" block", true), (" noblock", false), ("", true)];
        let iffull = [(" iffull", true), ("", false)];
        let ifempty = [(" ifempty", true), ("", false)];
        let instr = |ops| {
            Instruction(
                Location(0..4),
                Instruction {
                    ops,
                    delay: None,
                    side_set: None,
                },
            )
        };

        let push = itertools::iproduct!(blocking, iffull).map(|(b, i)| {
            let input = format!("push{}{}", i.0, b.0);
            let expected = instr(Push {
                if_full: i.1,
                blocking: b.1,
            });
            (input, expected)
        });
        let pull = itertools::iproduct!(blocking, ifempty).map(|(b, i)| {
            let input = format!("pull{}{}", i.0, b.0);
            let expected = instr(Pull {
                if_empty: i.1,
                blocking: b.1,
            });
            (input, expected)
        });
        let test_set = push.chain(pull).collect_vec();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_set() {
        use crate::pipeline::{
            Instruction, InstructionOperands::Set, Line::*, SetTarget::*, Value::Integer,
        };
        let set = |ops| {
            Instruction(
                Location(0..3),
                Instruction {
                    ops,
                    delay: None,
                    side_set: None,
                },
            )
        };

        let set_targets = [("pins", Pins), ("pindirs", PinDirs), ("x", X), ("y", Y)];

        let test_set = itertools::iproduct!(set_targets, COMMA)
            .map(|((target, expected), comma)| {
                let input = format!("set {target}{comma} 0");
                let expected = set(Set {
                    target: expected,
                    value: Integer(0),
                });
                (input, expected)
            })
            .collect_vec();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_mov() {
        use crate::pipeline::{Instruction, Line::*, MovOp, MovSource, MovTarget};
        let mov = |ops| {
            Instruction(
                Location(0..3),
                Instruction {
                    ops,
                    delay: None,
                    side_set: None,
                },
            )
        };
        let target = [
            ("pins", MovTarget::Pins),
            ("x", MovTarget::X),
            ("y", MovTarget::Y),
            ("pindirs", MovTarget::Pindirs),
            ("exec", MovTarget::Exec),
            ("pc", MovTarget::Pc),
            ("isr", MovTarget::Isr),
            ("osr", MovTarget::Osr),
        ];
        let source = [
            ("pins", MovSource::Pins),
            ("x", MovSource::X),
            ("y", MovSource::Y),
            ("null", MovSource::Null),
            ("status", MovSource::Status),
            ("isr", MovSource::Isr),
            ("osr", MovSource::Osr),
        ];
        let op = [
            ("!", Some(MovOp::Not)),
            ("::", Some(MovOp::Reverse)),
            ("", None),
        ];

        let test_data = itertools::iproduct!(target, COMMA, op, source)
            .map(|(trg, comma, op, src)| {
                let input = format!("mov {}{comma} {}{}", trg.0, op.0, src.0);
                let expected = mov(crate::pipeline::InstructionOperands::Mov {
                    src: src.1,
                    op: op.1,
                    trg: trg.1,
                });

                (input, expected)
            })
            .collect_vec();

        assert(&test_data);
    }

    #[test]
    fn parse_instruction_irq() {
        use crate::pipeline::{
            Instruction, InstructionOperands::Irq, IrqModifier, Line::*, Value::Integer,
        };
        let irq = |ops| {
            Instruction(
                Location(0..3),
                Instruction {
                    ops,
                    delay: None,
                    side_set: None,
                },
            )
        };

        let irq_modifier = [
            ("clear ", IrqModifier::Clear),
            ("wait ", IrqModifier::SetWait),
            ("nowait ", IrqModifier::Set),
            ("set ", IrqModifier::Set),
            ("", IrqModifier::Set),
        ];

        let rel = [(" rel", true), ("", false)];

        let test_data = itertools::iproduct!(irq_modifier, rel)
            .map(|(m, r)| {
                let input = format!("irq {}0{}", m.0, r.0);
                let expected = irq(Irq {
                    modifier: m.1,
                    value: Integer(0),
                    index_mode: if r.1 {
                        crate::pipeline::IrqIndexMode::Rel
                    } else {
                        crate::pipeline::IrqIndexMode::Direct
                    },
                });
                (input, expected)
            })
            .collect_vec();

        assert(&test_data);
    }
}
