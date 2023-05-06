use super::*;

#[test]
fn test_jump_1() {
    let mut a = Assembler::<32>::new();

    let mut l = a.label();
    a.set(SetDestination::X, 0);
    a.bind(&mut l);
    a.set(SetDestination::X, 1);
    a.jmp(JmpCondition::Always, &mut l);

    assert_eq!(
        a.assemble().as_slice(),
        &[
            0b111_00000_001_00000, // SET X 0
            // L:
            0b111_00000_001_00001, // SET X 1
            0b000_00000_000_00001, // JMP L
        ]
    );
}

#[test]
fn test_jump_2() {
    let mut a = Assembler::<32>::new();

    let mut top = a.label();
    let mut bottom = a.label();
    a.bind(&mut top);
    a.set(SetDestination::Y, 0);
    a.jmp(JmpCondition::YIsZero, &mut bottom);
    a.jmp(JmpCondition::Always, &mut top);
    a.bind(&mut bottom);
    a.set(SetDestination::Y, 1);

    assert_eq!(
        a.assemble().as_slice(),
        &[
            // TOP:
            0b111_00000_010_00000, // SET Y 0
            0b000_00000_011_00011, // JMP YIsZero BOTTOM
            0b000_00000_000_00000, // JMP Always TOP
            // BOTTOM:
            0b111_00000_010_00001, // SET Y 1
        ]
    );
}

#[test]
fn test_assemble_with_wrap() {
    let mut a = Assembler::<32>::new();

    let mut source = a.label();
    let mut target = a.label();

    a.set(SetDestination::PINDIRS, 0);
    a.bind(&mut target);
    a.r#in(InSource::NULL, 1);
    a.push(false, false);
    a.bind(&mut source);
    a.jmp(JmpCondition::Always, &mut target);

    assert_eq!(
        a.assemble_with_wrap(source, target).wrap,
        Wrap {
            source: 2,
            target: 1,
        }
    );
}

#[test]
fn test_assemble_program_default_wrap() {
    let mut a = Assembler::<32>::new();

    a.set(SetDestination::PINDIRS, 0);
    a.r#in(InSource::NULL, 1);
    a.push(false, false);

    assert_eq!(
        a.assemble_program().wrap,
        Wrap {
            source: 2,
            target: 0,
        }
    );
}

macro_rules! instr_test {
    ($name:ident ( $( $v:expr ),* ) , $expected:expr, $side_set:expr) => {
        paste::paste! {
            #[test]
            fn [< test _ $name _ $expected >]() {
                let expected = $expected;

                let mut a = Assembler::<32>::new_with_side_set($side_set);
                a.$name(
                    $( $v ),*
                );
                let instr = a.assemble()[0];
                if instr != expected {
                    panic!("assertion failure: (left == right)\nleft:  {:#016b}\nright: {:#016b}", instr, expected);
                }

                let decoded = Instruction::decode(instr, $side_set).unwrap();
                let encoded = decoded.encode($side_set);
                if encoded != expected {
                    panic!("assertion failure: (left == right)\nleft:  {:#016b}\nright: {:#016b}", encoded, expected);
                }
            }
        }
    };

    ($name:ident ( $( $v:expr ),* ) , $b:expr) => {
        instr_test!( $name ( $( $v ),* ), $b, SideSet::new(false, 0, false) );
    };
}

instr_test!(wait(0, WaitSource::IRQ, 2, false), 0b001_00000_010_00010);
instr_test!(wait(1, WaitSource::IRQ, 7, false), 0b001_00000_110_00111);
instr_test!(wait(1, WaitSource::GPIO, 16, false), 0b001_00000_100_10000);
instr_test!(
    wait_with_delay(0, WaitSource::IRQ, 2, false, 30),
    0b001_11110_010_00010
);
instr_test!(
    wait_with_side_set(0, WaitSource::IRQ, 2, false, 0b10101),
    0b001_10101_010_00010,
    SideSet::new(false, 5, false)
);
instr_test!(wait(0, WaitSource::IRQ, 2, true), 0b001_00000_010_10010);

#[test]
#[should_panic]
fn test_wait_relative_not_used_on_irq() {
    let mut a = Assembler::<32>::new();
    a.wait(0, WaitSource::PIN, 10, true);
    a.assemble_program();
}

instr_test!(r#in(InSource::Y, 10), 0b010_00000_010_01010);

instr_test!(out(OutDestination::Y, 10), 0b011_00000_010_01010);

instr_test!(push(true, false), 0b100_00000_010_00000);
instr_test!(push(false, true), 0b100_00000_001_00000);

instr_test!(pull(true, false), 0b100_00000_110_00000);
instr_test!(pull(false, true), 0b100_00000_101_00000);

instr_test!(
    mov(
        MovDestination::Y,
        MovOperation::BitReverse,
        MovSource::STATUS
    ),
    0b101_00000_010_10101
);

instr_test!(irq(true, false, 0b11, false), 0b110_00000_010_00011);
instr_test!(irq(false, true, 0b111, true), 0b110_00000_001_10111);

instr_test!(set(SetDestination::Y, 10), 0b111_00000_010_01010);

/// This block ensures that README.md is checked when `cargo test` is run.
#[cfg(doctest)]
mod test_readme {
    macro_rules! external_doc_test {
        ($x:expr) => {
            #[doc = $x]
            extern "C" {}
        };
    }
    external_doc_test!(include_str!("../README.md"));
}

