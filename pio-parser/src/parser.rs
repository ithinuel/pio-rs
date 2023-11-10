#![allow(clippy::all)]
include!(concat!(env!("OUT_DIR"), "/pio.rs"));

pub type Error<'i> = lalrpop_util::ParseError<usize, lexer::Token<'i>, lexer::Error>;

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{parse, Line, Location};

    const BASE: crate::Instruction = crate::Instruction {
        ops: crate::InstructionOps::Nop,
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
                parse(input.as_ref()).expect("Parse failure"),
                &[expected.clone()],
                "on input: {:?}",
                input
            );
        }
    }

    #[test]
    fn parse_program_directive() {
        let res = parse(".program hello_world").expect("Parsing failure");
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

        let mut label = SymbolDef {
            public: true,
            is_label: true,
            name: "label",
            location: Location(7..12),
        };

        let res = parse("public label:").expect("Parsing failure");
        assert_eq!(res, [Line::Label(label.clone())]);

        label.public = false;
        label.location = Location(0..5);
        let res = parse("label:").expect("Parsing failure");
        assert_eq!(res, [Line::Label(label.clone())]);

        label.public = false;
        label.location = Location(0..5);
        let res = parse("label: label2:").expect_err("Expected parse failure");
        match res {
            lalrpop_util::ParseError::UnrecognizedToken { expected, .. } => {
                // This test is brittle/over-specific because order matters when it should not.
                assert_eq!(
                    expected,
                    &[
                        "EoF", "in_", "irq", "jmp", "mov", "newline", "nop", "out", "pull", "push",
                        "set", "wait"
                    ]
                );
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
        assert_eq!(res, &[Line::CodeBlock("c-sdk", blob)])
    }

    #[test]
    fn directive_wraps() {
        use crate::{
            Directive::{Wrap, WrapTarget},
            Line::Directive,
        };

        let res = parse(".wrap").unwrap();
        assert_eq!(res, &[Directive(Wrap(Location(0..5)))]);

        let res = parse(".wrap_target").unwrap();
        assert_eq!(res, &[Directive(WrapTarget(Location(0..12)))]);
    }

    #[test]
    fn parse_expressions() {
        use crate::{Directive::*, Expression::*, Line::*, Value::*};

        assert(&[
            (".origin 23", Directive(Origin(Location(0..7), Integer(23)))),
            (
                ".origin ident",
                Directive(Origin(Location(0..7), Identifier("ident", Location(8..13)))),
            ),
            (
                ".origin (1+2)",
                Directive(Origin(
                    Location(0..7),
                    Expression(Plus(Value(Integer(1)).boxed(), Value(Integer(2)).boxed()).boxed()),
                )),
            ),
            (
                ".origin (3+4+5)",
                Directive(Origin(
                    Location(0..7),
                    Expression(
                        Plus(
                            Plus(Value(Integer(3)).boxed(), Value(Integer(4)).boxed()).boxed(),
                            Value(Integer(5)).boxed(),
                        )
                        .boxed(),
                    ),
                )),
            ),
            (
                ".origin (6*7+8)",
                Directive(Origin(
                    Location(0..7),
                    Expression(
                        Plus(
                            Multiply(Value(Integer(6)).boxed(), Value(Integer(7)).boxed()).boxed(),
                            Value(Integer(8)).boxed(),
                        )
                        .boxed(),
                    ),
                )),
            ),
            (
                ".origin (9*10+11*12*13)",
                Directive(Origin(
                    Location(0..7),
                    Expression(
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
                    ),
                )),
            ),
            (
                ".origin (9*10+var1*-12*13-24*::(var2))",
                Directive(Origin(
                    Location(0..7),
                    Expression(
                        Minus(
                            Plus(
                                Multiply(Value(Integer(9)).boxed(), Value(Integer(10)).boxed())
                                    .boxed(),
                                Multiply(
                                    Multiply(
                                        Value(Identifier("var1", Location(14..18))).boxed(),
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
                                Reverse(Value(Identifier("var2", Location(32..36))).boxed())
                                    .boxed(),
                            )
                            .boxed(),
                        )
                        .boxed(),
                    ),
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

        assert(&[
            (
                ".side_set 32",
                Directive(SideSet {
                    location: Location(0..9),
                    value: Integer(32),
                    optional: false,
                    pindirs: false,
                }),
            ),
            (
                ".side_set 32 opt",
                Directive(SideSet {
                    location: Location(0..9),
                    value: Integer(32),
                    optional: true,
                    pindirs: false,
                }),
            ),
            (
                ".side_set 32 opt pindirs",
                Directive(SideSet {
                    location: Location(0..9),
                    value: Integer(32),
                    optional: true,
                    pindirs: true,
                }),
            ),
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
        use crate::{Expression::*, Instruction, InstructionOps, Line::*, Value::*};

        assert(&[
            (
                "NOP",
                Instruction(
                    Location(0..3),
                    Instruction {
                        ops: InstructionOps::Nop,
                        delay: None,
                        side_set: None,
                    },
                ),
            ),
            (
                "nop",
                Instruction(
                    Location(0..3),
                    Instruction {
                        ops: InstructionOps::Nop,
                        delay: None,
                        side_set: None,
                    },
                ),
            ),
            (
                "nop [42]",
                Instruction(
                    Location(0..3),
                    Instruction {
                        ops: InstructionOps::Nop,
                        delay: Some(Value(Integer(42))),
                        side_set: None,
                    },
                ),
            ),
            (
                "nop side 12",
                Instruction(
                    Location(0..3),
                    Instruction {
                        ops: InstructionOps::Nop,
                        delay: None,
                        side_set: Some(Integer(12)),
                    },
                ),
            ),
            (
                "nop side 12 [42]",
                Instruction(
                    Location(0..3),
                    Instruction {
                        ops: InstructionOps::Nop,
                        delay: Some(Value(Integer(42))),
                        side_set: Some(Integer(12)),
                    },
                ),
            ),
            (
                "nop [42] side 12",
                Instruction(
                    Location(0..3),
                    Instruction {
                        ops: InstructionOps::Nop,
                        delay: Some(Value(Integer(42))),
                        side_set: Some(Integer(12)),
                    },
                ),
            ),
        ]);
    }

    #[test]
    fn parse_instruction_wait() {
        use crate::{Instruction, InstructionOps, Line::*, Value::*, WaitSource::*};

        let val = Integer(0);
        // cases for `irq _ rel` are added manually to the iterator
        let source = [
            ("irq", Irq(val.clone(), false)),
            ("pin", Pin(val.clone())),
            ("gpio", Gpio(val)),
        ]
        .into_iter();
        let irq_rel_cases = [
            ("wait irq 0 rel", (Integer(1), Irq(Integer(0), true))),
            ("wait irq, 0 rel", (Integer(1), Irq(Integer(0), true))),
            ("wait 25 irq 0 rel", (Integer(25), Irq(Integer(0), true))),
            ("wait 25 irq, 0 rel", (Integer(25), Irq(Integer(0), true))),
        ]
        .into_iter();
        // default duration is 1
        let duration = [("", Integer(1)), ("25 ", Integer(25))].into_iter();
        // commas are optional
        let comma = ["", ","].into_iter();

        let test_set: Vec<_> = source
            .cartesian_product(duration)
            .cartesian_product(comma)
            .map(|(((s, es), (d, ed)), c)| {
                let input = format!("wait {d}{s}{c} 0");
                let expected = (ed, es);
                (input, expected)
            })
            .chain(irq_rel_cases.map(|(a, b)| (a.to_owned(), b)))
            .map(|(input, (duration, src))| {
                (
                    input,
                    Instruction(
                        Location(0..4),
                        Instruction {
                            ops: InstructionOps::Wait { duration, src },
                            ..BASE
                        },
                    ),
                )
            })
            .collect();
        assert(&test_set);
    }

    #[test]
    fn parse_instruction_in() {
        use crate::{Expression::*, InSource::*, Instruction, Line::*, Value::*};

        let src = [
            ("pins", Pins),
            ("x", X),
            ("y", Y),
            ("null", Null),
            ("isr", Isr),
            ("osr", Osr),
            ("status", Status),
        ]
        .into_iter();
        let bit_count = [
            ("0", Integer(0)),
            (
                "(0+5)",
                Expression(Plus(Value(Integer(0)).boxed(), Value(Integer(5)).boxed()).boxed()),
            ),
        ]
        .into_iter();
        let test_set: Vec<_> = src
            .cartesian_product(bit_count)
            .flat_map(|((in_src, src), (in_bit_count, bit_count))| {
                let input = format!("in {in_src} {in_bit_count}");
                let input_with_comma = format!("in {in_src}, {in_bit_count}");
                let expected = Instruction(
                    Location(0..2),
                    Instruction {
                        ops: crate::InstructionOps::In { src, bit_count },
                        ..BASE
                    },
                );

                [(input, expected.clone()), (input_with_comma, expected)].into_iter()
            })
            .collect();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_out() {
        use crate::{Expression::*, Instruction, Line::*, OutTarget::*, Value::*};

        let src = [
            ("pins", Pins),
            ("x", X),
            ("y", Y),
            ("null", Null),
            ("pindirs", PinDirs),
            ("isr", Isr),
            ("pc", Pc),
            ("exec", Exec),
        ]
        .into_iter();
        let bit_count = [
            ("0", Integer(0)),
            (
                "(0+5)",
                Expression(Plus(Value(Integer(0)).boxed(), Value(Integer(5)).boxed()).boxed()),
            ),
        ]
        .into_iter();
        let test_set: Vec<_> = src
            .cartesian_product(bit_count)
            .flat_map(|((in_trg, target), (in_bit_count, bit_count))| {
                let input = format!("out {in_trg} {in_bit_count}");
                let input_with_comma = format!("out {in_trg}, {in_bit_count}");
                let expected = Instruction(
                    Location(0..3),
                    Instruction {
                        ops: crate::InstructionOps::Out { target, bit_count },
                        ..BASE
                    },
                );

                [(input, expected.clone()), (input_with_comma, expected)].into_iter()
            })
            .collect();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_jmp() {
        use crate::{Expression::Value, Instruction, JmpCondition::*, Line::*, Value::Integer};

        let src = [
            ("! x", Some(NotX)),
            ("x--", Some(XPostDec)),
            ("!y", Some(NotY)),
            ("y−−", Some(YPostDec)),
            ("x!= y", Some(XNotEqualY)),
            ("pin", Some(Pin)),
            ("! osre", Some(OSRNotEmpty)),
            ("", None),
        ]
        .into_iter();
        let test_set: Vec<_> = src
            .flat_map(|(in_cond, condition)| {
                let input = format!("jmp {in_cond} 0");
                let input_with_comma = format!("jmp {in_cond}, 0");
                let expected = Instruction(
                    Location(0..3),
                    Instruction {
                        ops: crate::InstructionOps::Jmp {
                            condition,
                            target: Value(Integer(0)),
                        },
                        ..BASE
                    },
                );

                [(input, expected.clone()), (input_with_comma, expected)].into_iter()
            })
            .collect();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_push_pull() {
        use crate::{
            Instruction,
            InstructionOps::{Pull, Push},
            Line::*,
        };

        let blocking = [(" block", true), (" noblock", false), ("", true)];
        let iffull = [(" iffull", true), ("", false)].into_iter();
        let ifempty = [(" ifempty", true), ("", false)].into_iter();

        let test_set: Vec<_> = blocking
            .into_iter()
            .cartesian_product(iffull)
            .map(|(b, i)| {
                let input = format!("push{}{}", i.0, b.0);
                let expected = Instruction(
                    Location(0..4),
                    Instruction {
                        ops: Push {
                            if_full: i.1,
                            blocking: b.1,
                        },
                        delay: None,
                        side_set: None,
                    },
                );
                (input, expected)
            })
            .chain(
                blocking
                    .into_iter()
                    .cartesian_product(ifempty)
                    .map(|(b, i)| {
                        let input = format!("pull{}{}", i.0, b.0);
                        let expected = Instruction(
                            Location(0..4),
                            Instruction {
                                ops: Pull {
                                    if_empty: i.1,
                                    blocking: b.1,
                                },
                                delay: None,
                                side_set: None,
                            },
                        );
                        (input, expected)
                    }),
            )
            .collect();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_set() {
        use crate::{Instruction, InstructionOps::Set, Line::*, SetTarget::*, Value::Integer};

        let set_targets = [("pins", Pins), ("pindirs", PinDirs), ("x", X), ("y", Y)];

        let test_set: Vec<_> = set_targets
            .into_iter()
            .flat_map(|(target, expected)| {
                let input = format!("set {target} 0");
                let input_with_comma = format!("set {target}, 0");
                let expected = Instruction(
                    Location(0..3),
                    Instruction {
                        ops: Set {
                            target: expected,
                            value: Integer(0),
                        },
                        delay: None,
                        side_set: None,
                    },
                );
                [(input, expected.clone()), (input_with_comma, expected)].into_iter()
            })
            .collect();

        assert(&test_set);
    }

    #[test]
    fn parse_instruction_mov() {
        use crate::{Instruction, Line::*, MovOp, MovSource, MovTarget};

        let target = [
            ("pins", MovTarget::Pins),
            ("x", MovTarget::X),
            ("y", MovTarget::Y),
            ("exec", MovTarget::Exec),
            ("pc", MovTarget::Pc),
            ("isr", MovTarget::Isr),
            ("osr", MovTarget::Osr),
        ]
        .into_iter();
        let source = [
            ("pins", MovSource::Pins),
            ("x", MovSource::X),
            ("y", MovSource::Y),
            ("null", MovSource::Null),
            ("status", MovSource::Status),
            ("isr", MovSource::Isr),
            ("osr", MovSource::Osr),
        ]
        .into_iter();
        let op = [
            ("!", Some(MovOp::Not)),
            ("::", Some(MovOp::Reverse)),
            ("", None),
        ]
        .into_iter();

        let test_data: Vec<_> = target
            .cartesian_product(op)
            .cartesian_product(source)
            .flat_map(|((trg, op), src)| {
                let input = format!("mov {} {}{}", trg.0, op.0, src.0);
                let input_with_comma = format!("mov {}, {}{}", trg.0, op.0, src.0);
                let expected = Instruction(
                    Location(0..3),
                    Instruction {
                        ops: crate::InstructionOps::Mov {
                            src: src.1,
                            op: op.1,
                            trg: trg.1,
                        },
                        delay: None,
                        side_set: None,
                    },
                );

                [(input, expected.clone()), (input_with_comma, expected)].into_iter()
            })
            .collect();

        assert(&test_data);
    }

    #[test]
    fn parse_instruction_irq() {
        use crate::{Instruction, InstructionOps::Irq, IrqModifier, Line::*, Value::Integer};

        let irq_modifier = [
            ("clear ", IrqModifier::Clear),
            ("wait ", IrqModifier::SetWait),
            ("nowait ", IrqModifier::Set),
            ("set ", IrqModifier::Set),
            ("", IrqModifier::Set),
        ]
        .into_iter();

        let rel = [(" rel", true), ("", false)].into_iter();

        let test_data: Vec<_> = irq_modifier
            .cartesian_product(rel)
            .map(|(m, r)| {
                let input = format!("irq {}0{}", m.0, r.0);
                let expected = Instruction(
                    Location(0..3),
                    Instruction {
                        ops: Irq {
                            modifier: m.1,
                            value: Integer(0),
                            relative: r.1,
                        },
                        delay: None,
                        side_set: None,
                    },
                );
                (input, expected)
            })
            .collect();

        assert(&test_data);
    }
}
