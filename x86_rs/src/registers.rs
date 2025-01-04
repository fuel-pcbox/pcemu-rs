#[repr(u8)]
pub enum Reg8 {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
}

impl From<u8> for Reg8 {
    fn from(val: u8) -> Reg8 {
        match val {
            0 => Reg8::AL,
            1 => Reg8::CL,
            2 => Reg8::DL,
            3 => Reg8::BL,
            4 => Reg8::AH,
            5 => Reg8::CH,
            6 => Reg8::DH,
            7 => Reg8::BH,
            _ => panic!(),
        }
    }
}

impl From<Reg8> for &'static str {
    fn from(val: Reg8) -> &'static str {
        match val {
            Reg8::AL => "al",
            Reg8::CL => "cl",
            Reg8::DL => "dl",
            Reg8::BL => "bl",
            Reg8::AH => "ah",
            Reg8::CH => "ch",
            Reg8::DH => "dh",
            Reg8::BH => "bh",
        }
    }
}

#[repr(u8)]
pub enum Reg8REX {
    AL,
    CL,
    DL,
    BL,
    SPL,
    BPL,
    SIL,
    DIL,
    R8B,
    R9B,
    R10B,
    R11B,
    R12B,
    R13B,
    R14B,
    R15B,
}

#[repr(u8)]
pub enum Reg16 {
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
    R8W,
    R9W,
    R10W,
    R11W,
    R12W,
    R13W,
    R14W,
    R15W,
}

impl From<u8> for Reg16 {
    fn from(val: u8) -> Reg16 {
        match val {
            0 => Reg16::AX,
            1 => Reg16::CX,
            2 => Reg16::DX,
            3 => Reg16::BX,
            4 => Reg16::SP,
            5 => Reg16::BP,
            6 => Reg16::SI,
            7 => Reg16::DI,
            8 => Reg16::R8W,
            9 => Reg16::R9W,
            10 => Reg16::R10W,
            11 => Reg16::R11W,
            12 => Reg16::R12W,
            13 => Reg16::R13W,
            14 => Reg16::R14W,
            15 => Reg16::R15W,
            _ => panic!(),
        }
    }
}

impl From<Reg16> for &'static str {
    fn from(val: Reg16) -> &'static str {
        match val {
            Reg16::AX => "ax",
            Reg16::CX => "cx",
            Reg16::DX => "dx",
            Reg16::BX => "bx",
            Reg16::SP => "sp",
            Reg16::BP => "bp",
            Reg16::SI => "si",
            Reg16::DI => "di",
            Reg16::R8W => "r8w",
            Reg16::R9W => "r9w",
            Reg16::R10W => "r10w",
            Reg16::R11W => "r11w",
            Reg16::R12W => "r12w",
            Reg16::R13W => "r13w",
            Reg16::R14W => "r14w",
            Reg16::R15W => "r15w",
        }
    }
}

#[repr(u8)]
pub enum Reg32 {
    EAX,
    ECX,
    EDX,
    EBX,
    ESP,
    EBP,
    ESI,
    EDI,
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,
    R15D,
}

impl From<u8> for Reg32 {
    fn from(val: u8) -> Reg32 {
        match val {
            0 => Reg32::EAX,
            1 => Reg32::ECX,
            2 => Reg32::EDX,
            3 => Reg32::EBX,
            4 => Reg32::ESP,
            5 => Reg32::EBP,
            6 => Reg32::ESI,
            7 => Reg32::EDI,
            8 => Reg32::R8D,
            9 => Reg32::R9D,
            10 => Reg32::R10D,
            11 => Reg32::R11D,
            12 => Reg32::R12D,
            13 => Reg32::R13D,
            14 => Reg32::R14D,
            15 => Reg32::R15D,
            _ => panic!(),
        }
    }
}

impl From<Reg32> for &'static str {
    fn from(val: Reg32) -> &'static str {
        match val {
            Reg32::EAX => "eax",
            Reg32::ECX => "ecx",
            Reg32::EDX => "edx",
            Reg32::EBX => "ebx",
            Reg32::ESP => "esp",
            Reg32::EBP => "ebp",
            Reg32::ESI => "esi",
            Reg32::EDI => "edi",
            Reg32::R8D => "r8d",
            Reg32::R9D => "r9d",
            Reg32::R10D => "r10d",
            Reg32::R11D => "r11d",
            Reg32::R12D => "r12d",
            Reg32::R13D => "r13d",
            Reg32::R14D => "r14d",
            Reg32::R15D => "r15d",
        }
    }
}

#[repr(u8)]
pub enum Reg64 {
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[repr(u8)]
#[derive(Clone, Copy)]
pub enum SegReg {
    ES,
    CS,
    SS,
    DS,
    FS,
    GS,
}

impl From<u8> for SegReg {
    fn from(val: u8) -> SegReg {
        match val {
            0 => SegReg::ES,
            1 => SegReg::CS,
            2 => SegReg::SS,
            3 => SegReg::DS,
            4 => SegReg::FS,
            5 => SegReg::GS,
            _ => panic!(),
        }
    }
}

impl From<SegReg> for &'static str {
    fn from(val: SegReg) -> &'static str {
        match val {
            SegReg::ES => "es",
            SegReg::CS => "cs",
            SegReg::SS => "ss",
            SegReg::DS => "ds",
            SegReg::FS => "fs",
            SegReg::GS => "gs",
        }
    }
}

#[repr(u64)]
pub enum Flags {
    Carry = 0,
    Parity = 2,
    Adjust = 4,
    Zero = 6,
    Sign = 7,
    Trap = 8,
    Interrupt = 9,
    Direction = 10,
    Overflow = 11,
    IOPL = 12,
    NestedTask = 14,
    Resume = 16,
    VirtualMode = 17,
    Alignment = 18,
    VirtualInterruptFlag = 19,
    VirtualInterruptPending = 20,
    Id = 21,
}

#[derive(Clone, Copy)]
pub union FprMmxReg {
    pub uq: u64,
    pub sq: i64,
    pub ul: [u32; 2],
    pub sl: [i32; 2],
    pub uw: [u16; 4],
    pub sw: [i16; 4],
    pub ub: [u8; 8],
    pub sb: [i8; 8],
    pub f: [f32; 2],
}

#[derive(Clone, Copy)]
pub struct FprReg {
    pub exp: u16,
    pub mantissa: FprMmxReg,
}

#[derive(Clone, Copy)]
pub union SseReg {
    pub uq: [u64; 2],
    pub sq: [i64; 2],
    pub ul: [u32; 4],
    pub sl: [i32; 4],
    pub uw: [u16; 8],
    pub sw: [i16; 8],
    pub ub: [u8; 16],
    pub sb: [i8; 16],
    pub f: [f32; 4],
    pub df: [f64; 4],
}

#[derive(Clone, Copy)]
pub struct YmmReg {
    pub xmm: [SseReg; 2],
}

#[derive(Clone, Copy)]
pub struct ZmmReg {
    pub ymm: [YmmReg; 2],
}

#[derive(Clone, Copy)]
pub struct SegmentRegister {
    pub base: u64,
    pub limit: u64,
    pub limit_low: u64,
    pub limit_high: u64,
    pub flags: u16,
    pub selector: u16,
}

impl SegmentRegister {
    pub fn new() -> SegmentRegister {
        SegmentRegister {
            base: 0,
            limit: 0xffffu64,
            limit_low: 0,
            limit_high: 0xffffu64,
            flags: 0x0093,
            selector: 0,
        }
    }
}

#[repr(u8)]
#[derive(PartialEq, Clone, Copy)]
pub enum AddrType16 {
    BxSi,
    BxDi,
    BpSi,
    BpDi,
    Si,
    Di,
    Bp,
    Bx,
}

pub fn format_offset_for_disasm(
    addr_type: Option<AddrType16>,
    disp_type: Option<DisplacementType>,
    offset: u64,
) -> String {
    let disp_type_str = match disp_type {
        Some(DisplacementType::Byte) => Some("byte"),
        Some(DisplacementType::Word) => Some("word"),
        None => None,
    };
    let addr_type_str = match addr_type {
        Some(AddrType16::BxSi) => Some("bx+si"),
        Some(AddrType16::BxDi) => Some("bx+di"),
        Some(AddrType16::BpSi) => Some("bp+si"),
        Some(AddrType16::BpDi) => Some("bp+di"),
        Some(AddrType16::Si) => Some("si"),
        Some(AddrType16::Di) => Some("di"),
        Some(AddrType16::Bp) => {
            if disp_type == None {
                Some("bp")
            } else {
                disp_type_str
            }
        }
        Some(AddrType16::Bx) => Some("bx"),
        None => None,
    };
    if addr_type_str == None {
        if disp_type_str == None {
            panic!();
        }

        format!("[{} {:x}]", disp_type_str.unwrap(), offset)
    } else {
        format!(
            "[{}+{} {:x}]",
            addr_type_str.unwrap(),
            disp_type_str.unwrap(),
            offset
        )
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum DisplacementType {
    Byte,
    Word,
}

#[derive(Clone, Copy)]
pub enum Operand {
    Register(u8),
    Address(SegReg, u64),
}

#[derive(Clone, Copy)]
pub struct OpcodeParams {
    pub reg: u8,
    pub rm: Operand,
}

#[derive(Clone, Copy)]
pub struct Registers {
    pub gpr: [u64; 16],
    pub fpr: [FprReg; 8],
    pub zmm: [ZmmReg; 32],
    pub cr0: u64,
    pub cr2: u64,
    pub cr3: u64,
    pub cr4: u64,
    pub cr8: u64,
    pub dr0123: [u64; 4],
    pub dr6: u64,
    pub dr7: u64,
    pub segs: [SegmentRegister; 6],
    pub rip: u64,
    pub flags: u64,
}

impl Registers {
    pub fn new() -> Registers {
        let resetxmm = SseReg { uq: [0, 0] };
        Registers {
            gpr: [0; 16],
            fpr: [FprReg {
                exp: 0,
                mantissa: FprMmxReg { uq: 0 },
            }; 8],
            zmm: [ZmmReg {
                ymm: [YmmReg { xmm: [resetxmm; 2] }; 2],
            }; 32],
            cr0: 0,
            cr2: 0,
            cr3: 0,
            cr4: 0,
            cr8: 0,
            dr0123: [0; 4],
            dr6: 0,
            dr7: 0,
            segs: [SegmentRegister::new(); 6],
            rip: 0xfff0,
            flags: 0x0000000000000002u64,
        }
    }

    pub fn reset() -> Registers {
        let mut regs = Registers::new();
        regs.segs[SegReg::CS as usize].base = 0xf0000;
        regs.segs[SegReg::CS as usize].selector = 0xf000;
        regs
    }

    #[inline(always)]
    pub fn read8(self, reg: Reg8) -> u8 {
        let regval = reg as usize;
        if regval & 4 == 4 {
            ((self.gpr[regval & 3] >> 8) & 0xff) as u8
        } else {
            self.gpr[regval & 3] as u8
        }
    }

    #[inline(always)]
    pub fn write8(&mut self, reg: Reg8, data: u8) {
        let regval = reg as usize;
        if regval & 4 == 4 {
            self.gpr[regval & 3] &= !0xff00;
            self.gpr[regval & 3] |= (data as u64) << 8;
        } else {
            self.gpr[regval & 3] &= !0xff;
            self.gpr[regval & 3] |= data as u64;
        }
    }

    #[inline(always)]
    pub fn read8rex(self, reg: Reg8REX) -> u8 {
        let regval = reg as usize;
        self.gpr[regval] as u8
    }

    #[inline(always)]
    pub fn write8rex(&mut self, reg: Reg8REX, data: u8) {
        let regval = reg as usize;
        self.gpr[regval] &= !0xff;
        self.gpr[regval] |= data as u64;
    }

    #[inline(always)]
    pub fn read16(self, reg: Reg16) -> u16 {
        let regval = reg as usize;
        self.gpr[regval] as u16
    }

    #[inline(always)]
    pub fn write16(&mut self, reg: Reg16, data: u16) {
        let regval = reg as usize;
        self.gpr[regval] &= !0xffff;
        self.gpr[regval] |= data as u64;
    }

    #[inline(always)]
    pub fn read32(self, reg: Reg32) -> u32 {
        let regval = reg as usize;
        self.gpr[regval] as u32
    }

    #[inline(always)]
    pub fn write32(&mut self, reg: Reg32, data: u32) {
        let regval = reg as usize;
        self.gpr[regval] = data as u64; //32-bit writes zero the upper 32 bits of a register.
    }

    #[inline(always)]
    pub fn read64(self, reg: Reg64) -> u64 {
        let regval = reg as usize;
        self.gpr[regval]
    }

    #[inline(always)]
    pub fn write64(&mut self, reg: Reg64, data: u64) {
        let regval = reg as usize;
        self.gpr[regval] = data;
    }

    pub fn readseg_realmode(self, reg: SegReg) -> u16 {
        self.segs[reg as usize].selector
    }

    pub fn writeseg_realmode(&mut self, reg: SegReg, data: u16) {
        let regval = reg as usize;
        self.segs[regval].selector = data;
        self.segs[regval].base = (data as u64) << 4;
    }

    pub fn writeseg(&mut self, reg: SegReg, data: u16) {
        if self.cr0 & 1 == 1 {
            todo!();
        }
        else {
            self.writeseg_realmode(reg, data);
        }
    }

    pub fn getflag(self, flag: Flags) -> u64 {
        (self.flags >> (flag as u64)) & 1
    }

    pub fn setflag(&mut self, flag: Flags, data: u64) {
        let flagval = flag as u64;
        self.flags &= !(1 << flagval);
        self.flags |= (data & 1) << flagval;
    }
}