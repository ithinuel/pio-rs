use crate::SideSet;
use num_enum::TryFromPrimitive;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum WaitPolarity {
    /// Wait for Zero
    Zero = 0b0,
    /// Wait for One
    One = 0b1,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum JmpCondition {
    /// Always
    Always = 0b000,
    /// `!X`: scratch X zero
    XIsZero = 0b001,
    /// `X--`: scratch X non-zero, post decrement
    XDecNonZero = 0b010,
    /// `!Y`: scratch Y zero
    YIsZero = 0b011,
    /// `Y--`: scratch Y non-zero, post decrement
    YDecNonZero = 0b100,
    /// `X!=Y`: scratch X not equal to scratch Y
    XNotEqualY = 0b101,
    /// `PIN`: branch on input pin
    PinHigh = 0b110,
    /// `!OSRE`: output shift register not empty
    OutputShiftRegisterNotEmpty = 0b111,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum WaitSource {
    GPIO = 0b00,
    PIN = 0b01,
    IRQ = 0b10,
    // RESERVED = 0b11,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum InSource {
    PINS = 0b000,
    X = 0b001,
    Y = 0b010,
    NULL = 0b011,
    // RESERVED = 0b100,
    // RESERVED = 0b101,
    ISR = 0b110,
    OSR = 0b111,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum OutDestination {
    PINS = 0b000,
    X = 0b001,
    Y = 0b010,
    NULL = 0b011,
    PINDIRS = 0b100,
    PC = 0b101,
    ISR = 0b110,
    EXEC = 0b111,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum MovDestination {
    PINS = 0b000,
    X = 0b001,
    Y = 0b010,
    // RESERVED = 0b011,
    EXEC = 0b100,
    PC = 0b101,
    ISR = 0b110,
    OSR = 0b111,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum MovOperation {
    None = 0b00,
    Invert = 0b01,
    BitReverse = 0b10,
    // RESERVED = 0b11,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum MovSource {
    PINS = 0b000,
    X = 0b001,
    Y = 0b010,
    NULL = 0b011,
    // RESERVED = 0b100,
    STATUS = 0b101,
    ISR = 0b110,
    OSR = 0b111,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
pub enum SetDestination {
    PINS = 0b000,
    X = 0b001,
    Y = 0b010,
    // RESERVED = 0b011,
    PINDIRS = 0b100,
    // RESERVED = 0b101,
    // RESERVED = 0b110,
    // RESERVED = 0b111,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandError {
    /// relative flag should only be used with WaitSource::IRQ
    RelativeMustNotBeSetWithoutWaitsourceIRQ,
    /// Index for WaitSource::IRQ should be in range 0..=7
    IndexOutOfRange(u8),
    /// Delay should be in 0..=max
    DelayOutOfRange { delay: u8, max: u8 },
    /// Side pin should in 0..=max
    SideSetOutOfRange { out: u8, max: u8 },
    /// Mandatory sideSet parameter missing
    MissingSideSetParameter,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionOperands {
    JMP {
        condition: JmpCondition,
        address: u8,
    },
    WAIT {
        /// 1 -> wait for 1
        /// 0 -> wait for 0
        polarity: WaitPolarity,
        source: WaitSource,
        index: u8,
        relative: bool,
    },
    IN {
        source: InSource,
        bit_count: u8,
    },
    OUT {
        destination: OutDestination,
        bit_count: u8,
    },
    PUSH {
        if_full: bool,
        block: bool,
    },
    PULL {
        if_empty: bool,
        block: bool,
    },
    MOV {
        destination: MovDestination,
        op: MovOperation,
        source: MovSource,
    },
    IRQ {
        clear: bool,
        wait: bool,
        index: u8,
        relative: bool,
    },
    SET {
        destination: SetDestination,
        data: u8,
    },
}
impl InstructionOperands {
    const fn discrim(&self) -> u16 {
        match self {
            InstructionOperands::JMP { .. } => 0b000,
            InstructionOperands::WAIT { .. } => 0b001,
            InstructionOperands::IN { .. } => 0b010,
            InstructionOperands::OUT { .. } => 0b011,
            InstructionOperands::PUSH { .. } => 0b100,
            InstructionOperands::PULL { .. } => 0b100,
            InstructionOperands::MOV { .. } => 0b101,
            InstructionOperands::IRQ { .. } => 0b110,
            InstructionOperands::SET { .. } => 0b111,
        }
    }

    const fn operands(&self) -> Result<(u8, u8), OperandError> {
        Ok(match *self {
            InstructionOperands::JMP { condition, address } => (condition as u8, address),
            InstructionOperands::WAIT {
                polarity,
                source,
                index,
                relative,
            } => {
                if relative && !matches!(source, WaitSource::IRQ) {
                    return Err(OperandError::RelativeMustNotBeSetWithoutWaitsourceIRQ);
                }
                if matches!(source, WaitSource::IRQ) && index > 7 {
                    return Err(OperandError::IndexOutOfRange(index));
                }
                (
                    (polarity as u8) << 2 | (source as u8),
                    index | (if relative { 0b10000 } else { 0 }),
                )
            }
            InstructionOperands::IN { source, bit_count } => (source as u8, bit_count),
            InstructionOperands::OUT {
                destination,
                bit_count,
            } => (destination as u8, bit_count & 0b11111),
            InstructionOperands::PUSH { if_full, block } => {
                ((if_full as u8) << 1 | (block as u8), 0)
            }
            InstructionOperands::PULL { if_empty, block } => {
                (1 << 2 | (if_empty as u8) << 1 | (block as u8), 0)
            }
            InstructionOperands::MOV {
                destination,
                op,
                source,
            } => (destination as u8, (op as u8) << 3 | (source as u8)),
            InstructionOperands::IRQ {
                clear,
                wait,
                index,
                relative,
            } => {
                if index > 7 {
                    return Err(OperandError::IndexOutOfRange(index));
                }
                (
                    (clear as u8) << 1 | (wait as u8),
                    index | (if relative { 0b10000 } else { 0 }),
                )
            }
            InstructionOperands::SET { destination, data } => (destination as u8, data),
        })
    }

    /// Encode these operands into binary representation.
    /// Note that this output does not take side set and delay into account.
    pub const fn encode(&self) -> Result<u16, OperandError> {
        let mut data: u16 = 0;
        data |= self.discrim() << 13;
        let (o0, o1) = match self.operands() {
            Ok((o0, o1)) => (o0, o1),
            Err(e) => return Err(e),
        };
        data |= (o0 as u16) << 5;
        data |= o1 as u16;
        Ok(data)
    }

    /// Decode operands from binary representation.
    /// Note that this output does not take side set and delay into account.
    pub fn decode(instruction: u16) -> Option<Self> {
        let discrim = instruction >> 13;
        let o0 = ((instruction >> 5) & 0b111) as u8;
        let o1 = (instruction & 0b11111) as u8;
        match discrim {
            0b000 => JmpCondition::try_from(o0)
                .ok()
                .map(|condition| InstructionOperands::JMP {
                    condition,
                    address: o1,
                }),
            0b001 => {
                WaitSource::try_from(o0 & 0b011)
                    .ok()
                    .map(|source| InstructionOperands::WAIT {
                        polarity: (o0 >> 2).try_into().unwrap(),
                        source,
                        index: if source == WaitSource::IRQ {
                            o1 & 0b00111
                        } else {
                            o1
                        },
                        relative: source == WaitSource::IRQ && (o1 & 0b10000) != 0,
                    })
            }
            0b010 => InSource::try_from(o0)
                .ok()
                .map(|source| InstructionOperands::IN {
                    source,
                    bit_count: o1,
                }),
            0b011 => {
                OutDestination::try_from(o0)
                    .ok()
                    .map(|destination| InstructionOperands::OUT {
                        destination,
                        bit_count: o1,
                    })
            }
            0b100 => {
                let if_flag = o0 & 0b010 != 0;
                let block = o0 & 0b001 != 0;
                if o1 != 0 {
                    None
                } else if o0 & 0b100 == 0 {
                    Some(InstructionOperands::PUSH {
                        if_full: if_flag,
                        block,
                    })
                } else {
                    Some(InstructionOperands::PULL {
                        if_empty: if_flag,
                        block,
                    })
                }
            }
            0b101 => match (
                MovDestination::try_from(o0).ok(),
                MovOperation::try_from((o1 >> 3) & 0b11).ok(),
                MovSource::try_from(o1 & 0b111).ok(),
            ) {
                (Some(destination), Some(op), Some(source)) => Some(InstructionOperands::MOV {
                    destination,
                    op,
                    source,
                }),
                _ => None,
            },
            0b110 => {
                if o0 & 0b100 == 0 {
                    Some(InstructionOperands::IRQ {
                        clear: o0 & 0b010 != 0,
                        wait: o0 & 0b001 != 0,
                        index: o1 & 0b01111,
                        relative: o1 & 0b10000 != 0,
                    })
                } else {
                    None
                }
            }
            0b111 => {
                SetDestination::try_from(o0)
                    .ok()
                    .map(|destination| InstructionOperands::SET {
                        destination,
                        data: o1,
                    })
            }
            _ => None,
        }
    }
}

/// A PIO instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    pub(crate) operands: InstructionOperands,
    pub(crate) delay: u8,
    pub(crate) side_set: Option<u8>,
}
impl Instruction {
    /// Encode a single instruction.
    pub const fn encode(&self, side_set: SideSet) -> Result<u16, OperandError> {
        let delay_max = (1 << (5 - side_set.bits)) - 1;
        let mut data = match self.operands.encode() {
            Ok(data) => data,
            Err(e) => return Err(e),
        };

        if self.delay > delay_max {
            return Err(OperandError::DelayOutOfRange {
                delay: self.delay,
                max: delay_max,
            });
        }

        let side_set = if let Some(s) = self.side_set {
            let max = (1 << side_set.bits) - 1;
            if s > max {
                return Err(OperandError::SideSetOutOfRange { out: s, max });
            }
            let s = (s as u16) << (5 - side_set.bits);
            if side_set.opt {
                s | 0b10000
            } else {
                s
            }
        } else if side_set.bits > 0 && !side_set.opt {
            return Err(OperandError::MissingSideSetParameter);
        } else {
            0
        };

        data |= ((self.delay as u16) | side_set) << 8;

        Ok(data)
    }

    /// Decode a single instruction.
    pub fn decode(instruction: u16, side_set: SideSet) -> Option<Instruction> {
        InstructionOperands::decode(instruction).map(|operands| {
            let data = ((instruction >> 8) & 0b11111) as u8;

            let delay = data & ((1 << (5 - side_set.bits)) - 1);

            let has_side_set = side_set.bits > 0 && (!side_set.opt || data & 0b10000 > 0);
            let side_set_data =
                (data & if side_set.opt { 0b01111 } else { 0b11111 }) >> (5 - side_set.bits);

            let side_set = if has_side_set {
                Some(side_set_data)
            } else {
                None
            };

            Instruction {
                operands,
                delay,
                side_set,
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionOrWord {
    Instruction(Instruction),
    Word(u16),
}
impl InstructionOrWord {
    pub fn instruction(&self) -> Option<Instruction> {
        if let InstructionOrWord::Instruction(i) = self {
            Some(*i)
        } else {
            None
        }
    }
}
impl From<Instruction> for InstructionOrWord {
    fn from(value: Instruction) -> Self {
        InstructionOrWord::Instruction(value)
    }
}
impl From<u16> for InstructionOrWord {
    fn from(value: u16) -> Self {
        InstructionOrWord::Word(value)
    }
}
