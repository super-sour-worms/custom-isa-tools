pub const Register = enum(u5) {
    zr = 0,
    pc = 1,
    r2 = 2,
    r3 = 3,
    r4 = 4,
    r5 = 5,
    r6 = 6,
    r7 = 7,
    r8 = 8,
    r9 = 9,
    r10 = 10,
    r11 = 11,
    r12 = 12,
    r13 = 13,
    r14 = 14,
    r15 = 15,
    r16 = 16,
    r17 = 17,
    r18 = 18,
    r19 = 19,
    r20 = 20,
    r21 = 21,
    r22 = 22,
    r23 = 23,
    r24 = 24,
    r25 = 25,
    r26 = 26,
    r27 = 27,
    r28 = 28,
    r29 = 29,
    r30 = 30,
    r31 = 31,
};

pub const InstructionType = enum(u5) {
    /// Arithmetical-Logical Unit operation, LMov
    alu = 0,
    /// Memory
    mem = 1,
    /// Conditional jump
    cjmp = 2,
    /// Long jump
    ljmp = 3,
    _,
};

pub const AluOpcode = enum(u3) {
    /// addition
    /// a + b
    add = 0,
    /// subtraction
    /// a - b
    sub = 1,
    /// exclusive or
    /// a ^ b
    xor = 2,
    /// bit reset
    /// a & ~b
    brs = 3,
    /// binary and
    /// a & b
    @"and" = 4,
    /// binary nand
    /// ~(a & b)
    nand = 5,
    /// binary nor
    /// ~(a | b)
    nor = 6,
    /// binary or
    /// a | b
    @"or" = 7,

    // mine compared to arm7tdmi:
    // 4: ADD Rd,Rn,Op2 ; add
    // 2: SUB Rd,Rn,Op2 ; subtract
    // 1: EOR Rd,Rn,Op2 ; XOR logical
    // E: BIC Rd,Rn,Op2 ; bit clear
    // 0: AND Rd,Rn,Op2 ; AND logical
    // "nand" not present in arm
    // "nor" not present in arm
    // C: ORR Rd,Rn,Op2 ; OR logical
    //
    // not present in CSP:
    // 3: RSB Rd,Rn,Op2 ; subtract reversed
    // F: MVN Rd,Op2    ; not

};

pub const MemOpcode = enum(u3) {
    /// store 1 byte
    str1 = 0,
    /// load 1 byte
    lod1 = 1,
    /// store 2 bytes
    str2 = 2,
    /// load 2 bytes
    lod2 = 3,
    /// store 4 bytes
    str4 = 4,
    /// load 4 bytes
    lod4 = 5,
    _,
};

pub const Condition = enum(u3) {
    /// equal
    eq = 0,
    /// not equal
    neq = 1,
    /// signed less
    sles = 2,
    /// signed less or equal
    sleq = 3,
    /// unsigned less
    ules = 4,
    /// unsigned less or equal
    uleq = 5,
    _,
};

pub const Instruction = packed struct(u32) {
    payload: Payload,
    type: InstructionType,

    pub const Payload = packed union {
        s_or_m_format: SOrMFormat,
        lm_format: LMFormat,
        cj_format: CJFormat,
        lj_format: LJFormat,
    };

    /// S for "Shift". Used for Alu, Mem and CSet
    /// M for "Memory". same as S, except immediate op2 is signed 14 bit immediate without shift,
    /// so it makes sense to make them one type
    pub const SOrMFormat = packed struct(u27) {
        secondary: SecondaryOperand,
        secondary_is_register: bool,
        primary: Register,
        target: Register,
        opcode: u3,
        pub const SecondaryOperand = packed union {
            s_format_imm: SFormatImm,
            m_format_imm: i13,
            reg: Reg,
            pub const SFormatImm = packed struct(u13) {
                value_to_shift: u8,
                shift_by: u5, // Always imm, shift type is ror
            };
            pub const Reg = packed struct {
                value_to_shift: Register,
                shift_type: ShiftType,
                is_shifted_by_reg: bool,
                shift_by: ShiftBy,
                pub const ShiftBy = packed union { imm: u5, reg: Register };
            };
        };

        pub const ShiftType = enum(u2) {
            lsl = 0,
            lsr = 1,
            asr = 2,
            rol = 3,
        };
    };

    /// LM for "Long Move"
    pub const LMFormat = packed struct(u27) {
        imm_low: u14,
        target: Register,
        _: u5 = 0,
        imm_high: u2,
        is_high_half: bool,
    };

    /// CJ for "Conditional Jump"
    pub const CJFormat = packed struct(u27) {
        /// Signed offset. Represents instructions, not bytes. Has +-32KiB range.
        pc_offset: i14,
        comparand2: Register,
        comparand1: Register,
        condition: Condition,
    };

    /// LJ for "Long Jump"
    pub const LJFormat = packed struct(u27) {
        base_reg: Register,
        link_reg: Register,
        /// Signed offset. Represents instructions, not bytes. Has +-256KiB range.
        offset: i17,
    };
};
