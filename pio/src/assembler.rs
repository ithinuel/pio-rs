use crate::intermediate_repr::{
    InSource, Instruction, InstructionOperands, InstructionOrWord, JmpCondition, MovDestination,
    MovOperation, MovSource, OutDestination, SetDestination, WaitPolarity, WaitSource,
};

use crate::storage::{Storage, WritableStorage};
use crate::{Label, OperandError, Program, SideSet, Wrap};

/// State of Label possibly used before being bound to a location.
///
/// The Unbound states builds a linked list of program indices terminated by `u8::max_value()`.
/// Each instruction refering to the label points to the next instr in the chain until the label is
/// bound.
///
/// A label starts in the `Unbound` state as the end of the chain.
/// When the label is used before being bound, its value becomes the location of the instruction
/// using it. The insrtuction's target is the previous value of the Unbound state.
///
/// When a label is bound to an address, each instr in the chain is updated to use the location of
/// the label.
///
/// When the label is used after being bound, the instruction takes the value of the Bound state.
#[derive(Debug, PartialEq, Eq)]
enum LabelState {
    Unbound(u8),
    Bound(u8),
}

#[derive(Debug, PartialEq, Eq)]
pub enum BindingError {
    AlreadyBound,
    Unbound,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AssemblingError {
    WrapSource(BindingError),
    WrapTarget(BindingError),
    OperandError(OperandError),
}
impl From<OperandError> for AssemblingError {
    fn from(value: OperandError) -> Self {
        AssemblingError::OperandError(value)
    }
}

/// A label.
///
/// *Warning:* It is an error to use and bind a single Label in different assembly instances.
#[derive(Debug, PartialEq, Eq)]
pub struct IRLabel {
    state: LabelState,
}

impl IRLabel {
    fn new() -> Self {
        Self {
            state: LabelState::Unbound(u8::MAX),
        }
    }
    fn use_at(&mut self, at: u8) -> u8 {
        match self.state {
            LabelState::Unbound(a) => {
                self.state = LabelState::Unbound(at);
                a
            }
            LabelState::Bound(a) => a,
        }
    }
}
impl TryFrom<IRLabel> for Label {
    type Error = BindingError;

    fn try_from(value: IRLabel) -> Result<Self, Self::Error> {
        match value.state {
            LabelState::Unbound(_) => Err(BindingError::Unbound),
            LabelState::Bound(address) => Ok(Label(address)),
        }
    }
}

/// A PIO Assembler. See chapter three of the [RP2040 Datasheet][].
///
/// [RP2040 Datasheet]: https://rptl.io/rp2040-datasheet
#[derive(Debug, Default)]
pub struct Assembler<T> {
    /// Keeps an sequence of intermediate representation of the instructions and words.
    instructions: T,
    side_set: SideSet,
    unbound_label: usize,
}

impl<T: Storage<InstructionOrWord> + Default> Assembler<T> {
    /// Create a new Assembler.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Assembler::new_with_side_set(SideSet::default())
    }

    /// Create a new Assembler with `SideSet` settings.
    #[allow(clippy::new_without_default)]
    pub fn new_with_side_set(side_set: SideSet) -> Self {
        Assembler {
            instructions: T::default(),
            side_set,
            unbound_label: 0,
        }
    }

    /// Assemble the program into PIO instructions.
    fn assemble<U>(self) -> Result<U, OperandError>
    where
        U: FromIterator<u16>,
    {
        assert_eq!(self.unbound_label, 0, "Not all label have been bound.");
        self.instructions
            .into_iter()
            .map(|v| match v {
                InstructionOrWord::Instruction(i) => i.encode(self.side_set),
                InstructionOrWord::Word(w) => Ok(w),
            })
            .collect()
    }

    /// Assemble the program into [`Program`].
    ///
    /// The program contains the instructions and side-set info set. You can directly compile into a program with
    /// correct wrapping with [`Self::assemble_with_wrap`], or you can set the wrapping after the compilation with
    /// [`Program::set_wrap`] from labels.
    pub fn assemble_program<U>(self) -> Result<Program<U>, AssemblingError>
    where
        U: Storage<u16> + FromIterator<u16>,
    {
        let side_set = self.side_set;
        let code: U = self.assemble()?;
        let wrap = Wrap {
            source: Label(code.len() as u8),
            target: Label(0),
        };

        Ok(Program {
            code,
            origin: None,
            side_set,
            wrap,
        })
    }

    /// Assemble the program into [`Program`] with wrapping.
    ///
    /// Takes pair of labels controlling the wrapping. The first label is the source (top) of the wrap while the second
    /// label is the target (bottom) of the wrap. The source label should be positioned _after_ the instruction from
    /// which the wrapping happens.
    pub fn assemble_with_wrap<U>(
        self,
        source: IRLabel,
        target: IRLabel,
    ) -> Result<Program<U>, AssemblingError>
    where
        U: Storage<u16> + FromIterator<u16>,
    {
        let source = source.try_into().map_err(AssemblingError::WrapSource)?;
        let target = target.try_into().map_err(AssemblingError::WrapTarget)?;
        Ok(self.assemble_program()?.set_wrap(Wrap { source, target }))
    }

    /// Create a new unbound Label.
    pub fn label(&mut self) -> IRLabel {
        self.unbound_label += 1;
        IRLabel::new()
    }

    /// Bind `label` to the current instruction position.
    pub fn bind(&mut self, label: &mut IRLabel) -> Result<(), BindingError> {
        match label.state {
            LabelState::Bound(_) => return Err(BindingError::AlreadyBound),
            LabelState::Unbound(mut patch) => {
                let resolved_address = self.instructions.len() as u8;
                // walk back the chain of indices and update them with the bound address of the
                // label.
                while patch != u8::MAX {
                    // those are internally generated and should not generate regular error.
                    // If something fails here, that is a critical failure.
                    let instr = self
                        .instructions
                        .get_mut(patch as usize)
                        .unwrap_or_else(|| panic!("Failed to find patch point at {}", patch));

                    let mut decoded_instr = instr.instruction().expect("Invalid instruction");
                    if let InstructionOperands::JMP { address, .. } = &mut decoded_instr.operands {
                        patch = *address;
                        *address = resolved_address;
                    } else {
                        unreachable!();
                    }
                    *instr = decoded_instr.into();
                }
                label.state = LabelState::Bound(resolved_address);
                self.unbound_label -= 1;
            }
        }
        Ok(())
    }
}

macro_rules! instr_impl {
    ( $(#[$inner:ident $($args:tt)*])* $name:ident ( $self:ident $(, $( $arg_name:ident : $arg_ty:ty ),*)? ) $body:expr, $delay:expr, $side_set:expr ) => {
        $(#[$inner $($args)*])*
        pub fn $name(
            &mut $self
            $(, $( $arg_name : $arg_ty , )*)?
        ) {
            $self.instructions.push(Instruction {
                operands: $body,
                delay: $delay,
                side_set: $side_set,
            }.into())
        }
    }
}

macro_rules! instr {
    ( $(#[$inner:ident $($args:tt)*])* $name:ident ( $self:ident $(, $($arg_name:ident : $arg_ty:ty ),*)? ) $body:expr ) => {
        instr_impl!($(#[$inner $($args)*])* $name ( $self $(, $( $arg_name: $arg_ty ),*)? ) $body, 0, None );
        paste::paste! {
            instr_impl!($(#[$inner $($args)*])* [< $name _with_delay >] ( $self $(, $( $arg_name: $arg_ty ),*)? , delay: u8 ) $body, delay, None );
            instr_impl!($(#[$inner $($args)*])* [< $name _with_side_set >] ( $self $(, $( $arg_name: $arg_ty ),*)? , side_set: u8 ) $body, 0, Some(side_set) );
            instr_impl!($(#[$inner $($args)*])* [< $name _with_delay_and_side_set >] ( $self $(, $( $arg_name: $arg_ty ),*)? , delay: u8, side_set: u8 ) $body, delay, Some(side_set) );
        }
    }
}

impl<T: WritableStorage<InstructionOrWord>> Assembler<T> {
    instr!(
        /// Emit a `jmp` instruction to `label` for `condition`.
        jmp(self, condition: JmpCondition, label: &mut IRLabel) {
            let address = label.use_at(self.instructions.len() as u8);
            InstructionOperands::JMP {
                condition,
                address,
            }
        }
    );

    instr!(
        /// Emit a `wait` instruction with `polarity` from `source` with `index` which may be
        /// `relative`.
        wait(self, polarity: WaitPolarity, source: WaitSource, index: u8, relative: bool) {
            InstructionOperands::WAIT {
                polarity,
                source,
                index,
                relative,
            }
        }
    );

    instr!(
        /// Emit an `in` instruction from `source` with `bit_count`.
        r#in(self, source: InSource, bit_count: u8) {
            InstructionOperands::IN { source, bit_count }
        }
    );

    instr!(
        /// Emit an `out` instruction to `source` with `bit_count`.
        out(self, destination: OutDestination, bit_count: u8) {
            InstructionOperands::OUT {
                destination,
                bit_count,
            }
        }
    );

    instr!(
        /// Emit a `push` instruction with `if_full` and `block`.
        push(self, if_full: bool, block: bool) {
            InstructionOperands::PUSH {
                if_full,
                block,
            }
        }
    );

    instr!(
        /// Emit a `pull` instruction with `if_empty` and `block`.
        pull(self, if_empty: bool, block: bool) {
            InstructionOperands::PULL {
                if_empty,
                block,
            }
        }
    );

    instr!(
        /// Emit a `mov` instruction to `destination` using `op` from `source`.
        mov(self, destination: MovDestination, op: MovOperation, source: MovSource) {
            InstructionOperands::MOV {
                destination,
                op,
                source,
            }
        }
    );

    instr!(
        /// Emit an `irq` instruction using `clear` and `wait` with `index` which may be `relative`.
        irq(self, clear: bool, wait: bool, index: u8, relative: bool) {
            InstructionOperands::IRQ {
                clear,
                wait,
                index,
                relative,
            }
        }
    );

    instr!(
        /// Emit a `set` instruction
        set(self, destination: SetDestination, data: u8) {
            InstructionOperands::SET {
                destination,
                data,
            }
        }
    );

    instr!(
        /// Emit a `mov` instruction from Y to Y without operation effectively acting as a `nop`
        /// instruction.
        nop(self) {
            InstructionOperands::MOV {
                destination: MovDestination::Y,
                op: MovOperation::None,
                source: MovSource::Y
            }
        }
    );
}

#[test]
fn test_jump_1() {
    let mut a = Assembler::<Vec<_>>::new();

    let mut l = a.label();
    a.set(SetDestination::X, 0);
    a.bind(&mut l).expect("Failed to bind");
    a.set(SetDestination::X, 1);
    a.jmp(JmpCondition::Always, &mut l);

    assert_eq!(
        a.assemble::<Vec<_>>().expect("Failed to assemble"),
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
    let mut a = Assembler::<Vec<_>>::new();

    let mut top = a.label();
    let mut bottom = a.label();
    a.bind(&mut top).expect("Failed to bind");
    a.set(SetDestination::Y, 0);
    a.jmp(JmpCondition::YIsZero, &mut bottom);
    a.jmp(JmpCondition::Always, &mut top);
    a.bind(&mut bottom).expect("Failed to bind");
    a.set(SetDestination::Y, 1);

    assert_eq!(
        a.assemble::<Vec<_>>().expect("Failed to assemble"),
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
    let mut a = Assembler::<Vec<_>>::new();

    let mut source = a.label();
    let mut target = a.label();

    a.set(SetDestination::PINDIRS, 0);
    a.bind(&mut target).expect("Failed to bind");
    a.r#in(InSource::NULL, 1);
    a.push(false, false);
    a.bind(&mut source).expect("Failed to bind");
    a.jmp(JmpCondition::Always, &mut target);

    assert_eq!(
        a.assemble_with_wrap::<Vec<_>>(source, target)
            .expect("Failed to assemble")
            .wrap,
        Wrap {
            source: Label(3),
            target: Label(1),
        }
    );
}

#[test]
fn test_assemble_program_default_wrap() {
    let mut a = Assembler::<Vec<_>>::new();

    a.set(SetDestination::PINDIRS, 0);
    a.r#in(InSource::NULL, 1);
    a.push(false, false);

    assert_eq!(
        a.assemble_program::<Vec<_>>()
            .expect("Failed to assemble")
            .wrap,
        Wrap {
            source: Label(3),
            target: Label(0),
        }
    );
}

macro_rules! instr_test {
    ($name:ident ( $( $v:expr ),* ) , $expected:expr, $side_set:expr) => {
        paste::paste! {
            #[test]
            fn [< test _ $name _ $expected >]() {
                let expected = $expected;

                let mut a = Assembler::<Vec<_>>::new_with_side_set($side_set);
                a.$name(
                    $( $v ),*
                );
                let assembled = a.assemble::<Vec<_>>().expect("Failed to assemble")[0];
                assert_eq!(assembled, expected, "encoded: {assembled:#016b}\nexpected: {expected:#016b}");

                //let decoded = assembled. Instruction::decode(assembled, $side_set).unwrap();
                //let encoded = decoded.encode($side_set).expect("Failed to encode");
                //assert_eq!(encoded, expected, "encoded: {encoded:#016b}\nexpected: {expected:#016b}");
            }
        }
    };

    ($name:ident ( $( $v:expr ),* ) , $b:expr) => {
        instr_test!( $name ( $( $v ),* ), $b, SideSet::new(false, 0, false) );
    };
}

instr_test!(
    wait(WaitPolarity::Zero, WaitSource::IRQ, 2, false),
    0b001_00000_010_00010
);
instr_test!(
    wait(WaitPolarity::One, WaitSource::IRQ, 7, false),
    0b001_00000_110_00111
);
instr_test!(
    wait(WaitPolarity::One, WaitSource::GPIO, 16, false),
    0b001_00000_100_10000
);
instr_test!(
    wait_with_delay(WaitPolarity::Zero, WaitSource::IRQ, 2, false, 30),
    0b001_11110_010_00010
);
instr_test!(
    wait_with_side_set(WaitPolarity::Zero, WaitSource::IRQ, 2, false, 0b10101),
    0b001_10101_010_00010,
    SideSet::new(false, 5, false)
);
instr_test!(
    wait(WaitPolarity::Zero, WaitSource::IRQ, 2, true),
    0b001_00000_010_10010
);

#[test]
fn test_wait_relative_not_used_on_irq() {
    let mut a = Assembler::<Vec<_>>::new();
    a.wait(WaitPolarity::Zero, WaitSource::PIN, 10, true);
    assert_eq!(
        a.assemble_program::<Vec<_>>().err(),
        Some(OperandError::RelativeMustNotBeSetWithoutWaitsourceIRQ.into())
    );
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
