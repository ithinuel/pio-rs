#![allow(clippy::all)]
include!(concat!(env!("OUT_DIR"), "/grammar.rs"));

pub type Error<'i> = lalrpop_util::ParseError<usize, lexer::Token<'i>, lexer::Error>;

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{parse, Line, Location};

    const COMMA: [&'static str; 2] = ["", ","];
    const BASE: crate::Instruction = crate::Instruction {
        ops: crate::InstructionOperands::Nop,
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
    fn parse_label() {
        use super::SymbolDef;
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

        label.public = false;
        label.location = Location(0..5);
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
        use crate::{
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
        use crate::{Directive::*, Expression::*, Line::*, Value::*};

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
        use crate::{
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
        use crate::{Directive::SideSet, Line::Directive, Value::Integer};
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
        use crate::{Directive::Word, Line::Directive, Value::Integer};

        assert(&[(".word 42", Directive(Word(Location(0..5), Integer(42))))]);
    }

    #[test]
    fn parse_lang_opt() {
        use crate::{Directive::*, Line::*};

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
        use crate::{Expression::*, Instruction, InstructionOperands, Line::*, Value::*};

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
        use crate::{Instruction, InstructionOperands, Line::*, Value::*, WaitSource::*};

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
                        "irq" => Irq(ev, !r.is_empty()),
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
    fn parse_instruction_in() {
        use crate::InSource::*;

        let src = [
            ("pins", Pins),
            ("x", X),
            ("y", Y),
            ("null", Null),
            ("isr", Isr),
            ("osr", Osr),
            ("status", Status),
        ];

        let test_set = generate_in_and_out_test_set("in", src, |source, bit_count| {
            crate::InstructionOperands::In {
                src: source,
                bit_count,
            }
        });

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_out() {
        use crate::OutTarget::*;

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
            crate::InstructionOperands::Out { target, bit_count }
        });
        assert(&test_set);
    }

    fn generate_in_and_out_test_set<'i, T: Clone>(
        cmd: &str,
        v: impl IntoIterator<Item = (&'static str, T)> + Clone,
        mut f: impl for<'u> FnMut(T, crate::Value<'u>) -> crate::InstructionOperands<'u>,
    ) -> Vec<(String, Line<'i>)> {
        use crate::{Expression::*, Instruction, Line::*, Value::*};

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
        use crate::{Expression::Value, Instruction, JmpCondition::*, Line::*, Value::Integer};
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
                let expected = jmp(crate::InstructionOperands::Jmp {
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
        use crate::{
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
                block: b.1,
            });
            (input, expected)
        });
        let test_set = push.chain(pull).collect_vec();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_set() {
        use crate::{Instruction, InstructionOperands::Set, Line::*, SetTarget::*, Value::Integer};
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
        use crate::{Instruction, Line::*, MovOp, MovSource, MovTarget};
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
                let expected = mov(crate::InstructionOperands::Mov {
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
        use crate::{Instruction, InstructionOperands::Irq, IrqModifier, Line::*, Value::Integer};
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
                    relative: r.1,
                });
                (input, expected)
            })
            .collect_vec();

        assert(&test_data);
    }
}
