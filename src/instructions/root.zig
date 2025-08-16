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
    str = 0, // Store, 32bit
    lod = 1, // Load
    hstr = 2, // 16bit
    hlod = 3,
    bstr = 4, // 8 bit
    blod = 5,
    _,
};

pub const Instruction = packed struct {
    type: InstructionType,
    payload: packed union {
        s_format: SFormat,
    },

    /// Used for alu and mem operations
    pub const SFormat = packed struct(u27) {
        opcode: u3,
        /// Source, or destination reg
        target_reg: u5,
        primary_operand_reg: u5,
        shift_source_is_register: bool,
        secondary_operand: SecondaryOperand,

        pub const SecondaryOperand = packed union {
            imm: packed struct {
                shift_by: u5, // Always imm, shift type is ror
                value_to_shift: u8,
            },
            reg: packed struct {
                shift_by: u5, // Imm, or reg
                is_shifted_by_reg: bool,
                shift_type: ShiftType,
                value_to_shift_reg: u5,
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
