pub const InstructionType = enum(u5) {
    alu = 0,
    mem = 1,
    cjmp = 2, // Conditional jump
    cset = 3, // Conditional set
    call = 4, // Calls, and return
    _,
};

pub const AluOpcode = enum(u3) {
    add = 0,
    sub = 1,
    xor = 2,
    brs = 3, // Bit reset
    @"and" = 4,
    nand = 5,
    nor = 6,
    @"or" = 7,
};

pub const MemOpcode = enum(u3) {
    blod = 0, // Load, 8bit
    bstr = 1, // Store
    hlod = 2, // 16bit
    hstr = 3,
    lod = 4, // 32 bit
    str = 5,
    _,
};

pub const Instruction = packed struct {
    payload: packed union {
        s_format: SFormat,
    },
    type: InstructionType,

    /// Used for alu and mem operations
    pub const SFormat = packed struct(u27) {
        secondary_operand: SecondaryOperand,
        shift_source_is_register: bool,
        primary_operand_reg: u5,
        target_reg: u5,
        opcode: u3,
        pub const SecondaryOperand = packed union {
            imm: packed struct {
                value_to_shift: u8,
                shift_by: u5, // Always imm, shift type is ror
            },
            reg: packed struct {
                value_to_shift_reg: u5,
                shift_type: ShiftType,
                is_shifted_by_reg: bool,
                shift_by: u5, // Imm, or reg
            },
        };

        const ShiftType = enum(u2) {
            lsl = 0,
            lsr = 1,
            asr = 2,
            ror = 3,
        };
    };
};
