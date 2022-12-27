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

fn format_offset_for_disasm(
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

    pub fn getflag(self, flag: Flags) -> u64 {
        (self.flags >> (flag as u64)) & 1
    }

    pub fn setflag(&mut self, flag: Flags, data: u64) {
        let flagval = flag as u64;
        self.flags &= !(1 << flagval);
        self.flags |= (data & 1) << flagval;
    }
}

pub trait CpuBus {
    fn mem_read8(&mut self, addr: u64) -> u8;
    fn mem_write8(&mut self, addr: u64, data: u8);
    fn mem_read16(&mut self, addr: u64) -> u16;
    fn mem_write16(&mut self, addr: u64, data: u16);
    fn mem_read32(&mut self, addr: u64) -> u32;
    fn mem_write32(&mut self, addr: u64, data: u32);
    fn mem_read64(&mut self, addr: u64) -> u64;
    fn mem_write64(&mut self, addr: u64, data: u64);

    fn io_read8(&mut self, addr: u16) -> u8;
    fn io_write8(&mut self, addr: u16, data: u8);
    fn io_read16(&mut self, addr: u16) -> u16;
    fn io_write16(&mut self, addr: u16, data: u16);
    fn io_read32(&mut self, addr: u16) -> u32;
    fn io_write32(&mut self, addr: u16, data: u32);
}

#[derive(PartialEq, Clone, Copy)]
pub enum Use32OpFlags {
    Bits16,
    Bits32,
}

#[derive(PartialEq, Clone, Copy)]
pub enum Use32AddrFlags {
    Bits16,
    Bits32,
}

#[derive(Clone, Copy)]
pub struct Cpu {
    pub regs: Registers,
    pub segment_override: Option<SegReg>,
    pub use_32op_default: Use32OpFlags,
    pub use_32op_prefix: bool,
    pub use_32addr_default: Use32AddrFlags,
    pub use_32addr_prefix: bool,
    modrm_disp: u64,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            regs: Registers::reset(),
            segment_override: None,
            use_32op_default: Use32OpFlags::Bits16,
            use_32op_prefix: false,
            use_32addr_default: Use32AddrFlags::Bits16,
            use_32addr_prefix: false,
            modrm_disp: 0,
        }
    }

    pub fn mem_read8<T: CpuBus>(&mut self, bus: &mut T, addr: u64) -> u8 {
        bus.mem_read8(addr)
    }

    pub fn mem_write8<T: CpuBus>(&mut self, bus: &mut T, addr: u64, data: u8) {
        bus.mem_write8(addr, data)
    }

    pub fn mem_read16<T: CpuBus>(&mut self, bus: &mut T, addr: u64) -> u16 {
        bus.mem_read16(addr)
    }

    pub fn mem_write16<T: CpuBus>(&mut self, bus: &mut T, addr: u64, data: u16) {
        bus.mem_write16(addr, data)
    }

    pub fn mem_read32<T: CpuBus>(&mut self, bus: &mut T, addr: u64) -> u32 {
        bus.mem_read32(addr)
    }

    pub fn mem_write32<T: CpuBus>(&mut self, bus: &mut T, addr: u64, data: u32) {
        bus.mem_write32(addr, data)
    }

    pub fn mem_read64<T: CpuBus>(&mut self, bus: &mut T, addr: u64) -> u64 {
        bus.mem_read64(addr)
    }

    pub fn mem_write64<T: CpuBus>(&mut self, bus: &mut T, addr: u64, data: u64) {
        bus.mem_write64(addr, data)
    }

    pub fn io_read8<T: CpuBus>(&mut self, bus: &mut T, addr: u16) -> u8 {
        bus.io_read8(addr)
    }

    pub fn io_write8<T: CpuBus>(&mut self, bus: &mut T, addr: u16, data: u8) {
        bus.io_write8(addr, data)
    }

    pub fn io_read16<T: CpuBus>(&mut self, bus: &mut T, addr: u16) -> u16 {
        bus.io_read16(addr)
    }

    pub fn io_write16<T: CpuBus>(&mut self, bus: &mut T, addr: u16, data: u16) {
        bus.io_write16(addr, data)
    }

    pub fn io_read32<T: CpuBus>(&mut self, bus: &mut T, addr: u16) -> u32 {
        bus.io_read32(addr)
    }

    pub fn io_write32<T: CpuBus>(&mut self, bus: &mut T, addr: u16, data: u32) {
        bus.io_write32(addr, data)
    }

    pub fn get_addr_type_from_modrm16(modrm: u8) -> Option<AddrType16> {
        let mode = (modrm & 0xc0) >> 6;
        let rm = modrm & 7;
        match rm {
            0 => Some(AddrType16::BxSi),
            1 => Some(AddrType16::BxDi),
            2 => Some(AddrType16::BpSi),
            3 => Some(AddrType16::BpDi),
            4 => Some(AddrType16::Si),
            5 => Some(AddrType16::Di),
            6 => {
                if mode == 0 {
                    None
                } else {
                    Some(AddrType16::Bp)
                }
            }
            7 => Some(AddrType16::Bx),
            _ => panic!("Invalid address type!"),
        }
    }

    pub fn get_disp_type_from_modrm(modrm: u8) -> Option<DisplacementType> {
        let mode = (modrm & 0xc0) >> 6;
        let rm = modrm & 7;
        match mode {
            0 => {
                if rm == 6 {
                    Some(DisplacementType::Word)
                } else {
                    None
                }
            }
            1 => Some(DisplacementType::Byte),
            2 => Some(DisplacementType::Word),
            _ => panic!("Invalid displacement type!"),
        }
    }

    pub fn get_offset(&self, addr_type: AddrType16, offset: u64) -> u64 {
        let base = match addr_type {
            AddrType16::BxSi => self.regs.read16(Reg16::BX) + self.regs.read16(Reg16::SI),
            AddrType16::BxDi => self.regs.read16(Reg16::BX) + self.regs.read16(Reg16::DI),
            AddrType16::BpSi => self.regs.read16(Reg16::BP) + self.regs.read16(Reg16::SI),
            AddrType16::BpDi => self.regs.read16(Reg16::BP) + self.regs.read16(Reg16::DI),
            AddrType16::Si => self.regs.read16(Reg16::SI),
            AddrType16::Di => self.regs.read16(Reg16::DI),
            AddrType16::Bp => self.regs.read16(Reg16::BP),
            AddrType16::Bx => self.regs.read16(Reg16::BX),
        };
        base as u64 + offset
    }

    pub fn get_operand_seg(
        &self,
        addr_type: Option<AddrType16>,
        disp_type: Option<DisplacementType>,
    ) -> SegReg {
        match self.segment_override {
            Some(segment) => segment,
            None => match addr_type {
                Some(AddrType16::BpSi) => SegReg::SS,
                Some(AddrType16::BpDi) => SegReg::SS,
                Some(AddrType16::Bp) => {
                    if disp_type == Some(DisplacementType::Word) {
                        SegReg::SS
                    } else {
                        SegReg::DS
                    }
                }
                _ => SegReg::DS,
            },
        }
    }

    pub fn get_opcode_params_from_modrm<T: CpuBus>(
        &mut self,
        bus: &mut T,
        modrm: u8,
    ) -> OpcodeParams {
        let mode = (modrm & 0xc0) >> 6;
        let reg = (modrm & 0x38) >> 3;
        let rm = modrm & 7;

        match mode {
            0 => {
                let addr_type = Cpu::get_addr_type_from_modrm16(modrm);
                let disp_type = Cpu::get_disp_type_from_modrm(modrm);
                let addr: u64;
                let segment: SegReg = self.get_operand_seg(addr_type, disp_type);
                match disp_type {
                    None => {
                        self.modrm_disp = 0;
                    }
                    Some(DisplacementType::Byte) => {
                        self.modrm_disp = self.mem_read8(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        ) as u64;
                        self.regs.rip = self.regs.rip.wrapping_add(1);
                    }
                    Some(DisplacementType::Word) => {
                        self.modrm_disp = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        ) as u64;
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                    }
                }
                match addr_type {
                    None => {
                        addr = self.modrm_disp;
                    }
                    Some(_) => {
                        addr = self.get_offset(addr_type.unwrap(), self.modrm_disp);
                    }
                }
                let operand_rm = Operand::Address(segment, addr);
                let operand_reg = reg;
                OpcodeParams {
                    reg: operand_reg,
                    rm: operand_rm,
                }
            }
            1 => {
                let addr_type = Cpu::get_addr_type_from_modrm16(modrm);
                let addr: u64;
                self.modrm_disp = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                ) as u64;
                let segment: SegReg = self.get_operand_seg(addr_type, Some(DisplacementType::Byte));
                self.regs.rip = self.regs.rip.wrapping_add(1);
                match addr_type {
                    None => panic!("Invalid address type for this ModR/M type!"),
                    Some(_) => {
                        addr = self.get_offset(addr_type.unwrap(), self.modrm_disp);
                    }
                }
                let operand_rm = Operand::Address(segment, addr);
                let operand_reg = reg;
                OpcodeParams {
                    reg: operand_reg,
                    rm: operand_rm,
                }
            }
            2 => {
                let addr_type = Cpu::get_addr_type_from_modrm16(modrm);
                let addr: u64;
                self.modrm_disp = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                ) as u64;
                let segment: SegReg = self.get_operand_seg(addr_type, Some(DisplacementType::Word));
                self.regs.rip = self.regs.rip.wrapping_add(2);
                match addr_type {
                    None => panic!("Invalid address type for this ModR/M type!"),
                    Some(_) => {
                        addr = self.get_offset(addr_type.unwrap(), self.modrm_disp);
                    }
                }
                let operand_rm = Operand::Address(segment, addr);
                let operand_reg = reg;
                OpcodeParams {
                    reg: operand_reg,
                    rm: operand_rm,
                }
            }
            3 => {
                let operand_rm = Operand::Register(rm);
                let operand_reg = reg;
                OpcodeParams {
                    reg: operand_reg,
                    rm: operand_rm,
                }
            }
            _ => panic!("Unimplemented ModR/M mode!"),
        }
    }

    fn setznp8(&mut self, data: u8) {
        if data == 0 {
            self.regs.setflag(Flags::Zero, 1);
        } else {
            self.regs.setflag(Flags::Zero, 0);
        }
        if data & 0x80 == 0x80 {
            self.regs.setflag(Flags::Sign, 1);
        } else {
            self.regs.setflag(Flags::Sign, 0);
        }

        if data.count_ones() & 1 == 1 {
            self.regs.setflag(Flags::Parity, 1);
        } else {
            self.regs.setflag(Flags::Parity, 0);
        }
    }

    fn setznp16(&mut self, data: u16) {
        if data == 0 {
            self.regs.setflag(Flags::Zero, 1);
        } else {
            self.regs.setflag(Flags::Zero, 0);
        }
        if data & 0x8000 == 0x8000 {
            self.regs.setflag(Flags::Sign, 1);
        } else {
            self.regs.setflag(Flags::Sign, 0);
        }

        if data.count_ones() & 1 == 1 {
            self.regs.setflag(Flags::Parity, 1);
        } else {
            self.regs.setflag(Flags::Parity, 0);
        }
    }

    fn setznp32(&mut self, data: u32) {
        if data == 0 {
            self.regs.setflag(Flags::Zero, 1);
        } else {
            self.regs.setflag(Flags::Zero, 0);
        }
        if data & 0x80000000 == 0x80000000 {
            self.regs.setflag(Flags::Sign, 1);
        } else {
            self.regs.setflag(Flags::Sign, 0);
        }

        if data.count_ones() & 1 == 1 {
            self.regs.setflag(Flags::Parity, 1);
        } else {
            self.regs.setflag(Flags::Parity, 0);
        }
    }

    fn setznp64(&mut self, data: u64) {
        if data == 0 {
            self.regs.setflag(Flags::Zero, 1);
        } else {
            self.regs.setflag(Flags::Zero, 0);
        }
        if data & 0x8000000000000000u64 == 0x8000000000000000u64 {
            self.regs.setflag(Flags::Sign, 1);
        } else {
            self.regs.setflag(Flags::Sign, 0);
        }

        if data.count_ones() & 1 == 1 {
            self.regs.setflag(Flags::Parity, 1);
        } else {
            self.regs.setflag(Flags::Parity, 0);
        }
    }

    pub fn tick<T: CpuBus>(&mut self, bus: &mut T) {
        let addr = self.regs.segs[SegReg::CS as usize].base + self.regs.rip;
        let opcode = self.mem_read8(bus, addr);
        println!(
            "opcode: {:x} cs: {:x} rip: {:x}",
            opcode,
            self.regs.segs[SegReg::CS as usize].selector,
            self.regs.rip
        );
        self.regs.rip = self.regs.rip.wrapping_add(1);

        match opcode {
            0x0a => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                if let Operand::Register(rm) = opcode_params.rm {
                    println!(
                        "or {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(opcode_params.reg)),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(rm))
                    );
                    let rm = self.regs.read8(rm.into());
                    let result = self.regs.read8(opcode_params.reg.into()) | rm;
                    self.regs.write8(opcode_params.reg.into(), result);
                    self.setznp8(result);
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    println!(
                        "or {}, {}:{}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(opcode_params.reg)),
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        )
                    );
                    let rm = self.mem_read8(bus, self.regs.segs[segment as usize].base + ea);
                    let result = self.regs.read8(opcode_params.reg.into()) | rm;
                    self.regs.write8(opcode_params.reg.into(), result);
                    self.setznp8(result);
                }
            }
            0x0c => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("or al, {:x}", imm);
                let result = self.regs.read8(Reg8::AL) | imm;
                self.setznp8(result);
            }
            0x0d => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    let imm = self.mem_read32(
                        bus,
                        self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                    );
                    self.regs.rip = self.regs.rip.wrapping_add(4);
                    println!("or eax, {:x}", imm);
                    let result = self.regs.read32(Reg32::EAX) | imm;
                    self.setznp32(result);
                } else {
                    let imm = self.mem_read16(
                        bus,
                        self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                    );
                    self.regs.rip = self.regs.rip.wrapping_add(2);
                    println!("or ax, {:x}", imm);
                    let result = self.regs.read16(Reg16::AX) | imm;
                    self.setznp16(result);
                }
            }
            0x0f => {
                let opcode2 = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("opcode2: {:x}", opcode2);
                match opcode2 {
                    0x01 => {
                        let modrm = self.mem_read8(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(1);
                        match modrm & 0x38 {
                            0x20 => {
                                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                                if let Operand::Register(rm) = opcode_params.rm {
                                    println!(
                                        "smsw {}",
                                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm))
                                    );
                                    self.regs.write16(rm.into(), self.regs.cr0 as u16);
                                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                                    println!(
                                        "smsw {}:{}",
                                        Into::<&'static str>::into(segment),
                                        format_offset_for_disasm(
                                            Cpu::get_addr_type_from_modrm16(modrm),
                                            Cpu::get_disp_type_from_modrm(modrm),
                                            self.modrm_disp
                                        )
                                    );
                                    self.mem_write16(
                                        bus,
                                        self.regs.segs[segment as usize].base + ea,
                                        self.regs.cr0 as u16,
                                    );
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    0x08 => {
                        println!("invd");
                    }
                    0x09 => {
                        println!("wbinvd");
                    }
                    0x20 => {
                        let modrm = self.mem_read8(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(1);
                        let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                        let reg = opcode_params.reg;
                        match reg {
                            0 => {
                                if let Operand::Register(rm) = opcode_params.rm {
                                    println!(
                                        "mov {}, cr0",
                                        Into::<&'static str>::into(<u8 as Into<Reg32>>::into(rm))
                                    );
                                    self.regs
                                        .write32(rm.into(), self.regs.cr0 as u32 | 0x7fffffe0u32);
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    0x22 => {
                        let modrm = self.mem_read8(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(1);
                        let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                        let reg = opcode_params.reg;
                        match reg {
                            0 => {
                                if let Operand::Register(rm) = opcode_params.rm {
                                    println!(
                                        "mov cr0, {}",
                                        Into::<&'static str>::into(<u8 as Into<Reg32>>::into(rm))
                                    );
                                    self.regs.cr0 = self.regs.read32(rm.into()) as u64;
                                }
                            }
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            0x12 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                if let Operand::Register(rm) = opcode_params.rm {
                    println!(
                        "adc {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(opcode_params.reg)),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(rm))
                    );
                    let rm = self.regs.read8(rm.into());
                    let carry = self.regs.getflag(Flags::Carry);
                    let reg = self.regs.read8(opcode_params.reg.into());
                    let result = self
                        .regs
                        .read8(opcode_params.reg.into())
                        .wrapping_add(rm)
                        .wrapping_add(carry as u8);
                    self.regs.write8(opcode_params.reg.into(), result);
                    self.setznp8(result);

                    if ((reg ^ rm) & 0x80 == 0x80) && ((reg ^ result) & 0x80) == 0x80 {
                        self.regs.setflag(Flags::Overflow, 1);
                    } else {
                        self.regs.setflag(Flags::Overflow, 0);
                    }

                    if ((result ^ rm ^ reg) & 0x10) == 0x10 {
                        self.regs.setflag(Flags::Adjust, 1);
                    } else {
                        self.regs.setflag(Flags::Adjust, 0);
                    }

                    if (rm & 0x80) > (result & 0x80) {
                        self.regs.setflag(Flags::Carry, 1);
                    } else {
                        self.regs.setflag(Flags::Carry, 0);
                    }
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    println!(
                        "adc {}, {}:{}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(opcode_params.reg)),
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        )
                    );
                    let rm = self.mem_read8(bus, self.regs.segs[segment as usize].base + ea);
                    let carry = self.regs.getflag(Flags::Carry);
                    let reg = self.regs.read8(opcode_params.reg.into());
                    let result = self
                        .regs
                        .read8(opcode_params.reg.into())
                        .wrapping_add(rm)
                        .wrapping_add(carry as u8);
                    self.regs.write8(opcode_params.reg.into(), result);
                    self.setznp8(result);

                    if ((reg ^ rm) & 0x80 == 0x80) && ((reg ^ result) & 0x80) == 0x80 {
                        self.regs.setflag(Flags::Overflow, 1);
                    } else {
                        self.regs.setflag(Flags::Overflow, 0);
                    }

                    if ((result ^ rm ^ reg) & 0x10) == 0x10 {
                        self.regs.setflag(Flags::Adjust, 1);
                    } else {
                        self.regs.setflag(Flags::Adjust, 0);
                    }

                    if (rm & 0x80) > (result & 0x80) {
                        self.regs.setflag(Flags::Carry, 1);
                    } else {
                        self.regs.setflag(Flags::Carry, 0);
                    }
                }
            }
            0x24 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("and al, {:x}", imm);
                let result = self.regs.read8(Reg8::AL) & imm;
                self.setznp8(result);
            }
            0x25 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    let imm = self.mem_read32(
                        bus,
                        self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                    );
                    self.regs.rip = self.regs.rip.wrapping_add(4);
                    println!("and eax, {:x}", imm);
                    let result = self.regs.read32(Reg32::EAX) & imm;
                    self.setznp32(result);
                } else {
                    let imm = self.mem_read16(
                        bus,
                        self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                    );
                    self.regs.rip = self.regs.rip.wrapping_add(2);
                    println!("and ax, {:x}", imm);
                    let result = self.regs.read16(Reg16::AX) & imm;
                    self.setznp16(result);
                }
            }
            0x26 => {
                self.segment_override = Some(SegReg::ES);
                self.tick(bus);
            }
            0x2e => {
                self.segment_override = Some(SegReg::CS);
                self.tick(bus);
            }
            0x33 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    if let Operand::Register(rm) = opcode_params.rm {
                        println!(
                            "xor {}, {}",
                            Into::<&'static str>::into(<u8 as Into<Reg32>>::into(
                                opcode_params.reg
                            )),
                            Into::<&'static str>::into(<u8 as Into<Reg32>>::into(rm))
                        );
                        let rm = self.regs.read32(rm.into());
                        let result = self.regs.read32(opcode_params.reg.into()) | rm;
                        self.regs.write32(opcode_params.reg.into(), result);
                        self.setznp32(result);
                    } else if let Operand::Address(segment, ea) = opcode_params.rm {
                        println!(
                            "xor {}, {}:{}",
                            Into::<&'static str>::into(<u8 as Into<Reg32>>::into(
                                opcode_params.reg
                            )),
                            Into::<&'static str>::into(segment),
                            format_offset_for_disasm(
                                Cpu::get_addr_type_from_modrm16(modrm),
                                Cpu::get_disp_type_from_modrm(modrm),
                                self.modrm_disp
                            )
                        );
                        let rm = self.mem_read32(bus, self.regs.segs[segment as usize].base + ea);
                        let result = self.regs.read32(opcode_params.reg.into()) | rm;
                        self.regs.write32(opcode_params.reg.into(), result);
                        self.setznp32(result);
                    }
                } else {
                    if let Operand::Register(rm) = opcode_params.rm {
                        println!(
                            "xor {}, {}",
                            Into::<&'static str>::into(<u8 as Into<Reg16>>::into(
                                opcode_params.reg
                            )),
                            Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm))
                        );
                        let rm = self.regs.read16(rm.into());
                        let result = self.regs.read16(opcode_params.reg.into()) | rm;
                        self.regs.write16(opcode_params.reg.into(), result);
                        self.setznp16(result);
                    } else if let Operand::Address(segment, ea) = opcode_params.rm {
                        println!(
                            "xor {}, {}:{}",
                            Into::<&'static str>::into(<u8 as Into<Reg16>>::into(
                                opcode_params.reg
                            )),
                            Into::<&'static str>::into(segment),
                            format_offset_for_disasm(
                                Cpu::get_addr_type_from_modrm16(modrm),
                                Cpu::get_disp_type_from_modrm(modrm),
                                self.modrm_disp
                            )
                        );
                        let rm = self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                        let result = self.regs.read16(opcode_params.reg.into()) | rm;
                        self.regs.write16(opcode_params.reg.into(), result);
                        self.setznp16(result);
                    }
                }
            }
            0x34 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("xor al, {:x}", imm);
                let result = self.regs.read8(Reg8::AL) ^ imm;
                self.setznp8(result);
            }
            0x35 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    let imm = self.mem_read32(
                        bus,
                        self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                    );
                    self.regs.rip = self.regs.rip.wrapping_add(4);
                    println!("xor eax, {:x}", imm);
                    let result = self.regs.read32(Reg32::EAX) ^ imm;
                    self.setznp32(result);
                } else {
                    let imm = self.mem_read16(
                        bus,
                        self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                    );
                    self.regs.rip = self.regs.rip.wrapping_add(2);
                    println!("xor ax, {:x}", imm);
                    let result = self.regs.read16(Reg16::AX) ^ imm;
                    self.setznp16(result);
                }
            }
            0x36 => {
                self.segment_override = Some(SegReg::SS);
                self.tick(bus);
            }
            0x3c => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("cmp al, {:x}", imm);
                let result = self.regs.read8(Reg8::AL).wrapping_sub(imm);
                self.setznp8(result);

                if ((result ^ imm) & (result ^ self.regs.read8(Reg8::AL)) & 0x80) == 0x80 {
                    self.regs.setflag(Flags::Overflow, 1);
                } else {
                    self.regs.setflag(Flags::Overflow, 0);
                }

                if ((result ^ self.regs.read8(Reg8::AL) ^ imm) & 0x10) == 0x10 {
                    self.regs.setflag(Flags::Adjust, 1);
                } else {
                    self.regs.setflag(Flags::Adjust, 0);
                }

                if imm > result {
                    self.regs.setflag(Flags::Carry, 1);
                } else {
                    self.regs.setflag(Flags::Carry, 0);
                }
            }
            0x3d => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("cmp ax, {:x}", imm);
                let result = self.regs.read16(Reg16::AX).wrapping_sub(imm);
                self.setznp16(result);

                if ((result ^ imm) & (result ^ self.regs.read16(Reg16::AX)) & 0x8000) == 0x8000 {
                    self.regs.setflag(Flags::Overflow, 1);
                } else {
                    self.regs.setflag(Flags::Overflow, 0);
                }

                if ((result ^ self.regs.read16(Reg16::AX) ^ imm) & 0x10) == 0x10 {
                    self.regs.setflag(Flags::Adjust, 1);
                } else {
                    self.regs.setflag(Flags::Adjust, 0);
                }

                if imm > result {
                    self.regs.setflag(Flags::Carry, 1);
                } else {
                    self.regs.setflag(Flags::Carry, 0);
                }
            }
            0x3e => {
                self.segment_override = Some(SegReg::DS);
                self.tick(bus);
            }
            0x66 => {
                self.use_32op_prefix = true;
                println!("OPSIZE:");
                self.tick(bus);
            }
            0x70 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jo {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Overflow) == 1 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x71 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jno {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Overflow) == 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x72 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jc {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Carry) == 1 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x73 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jnc {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Carry) == 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x74 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jz {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Zero) == 1 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x75 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jnz {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Zero) == 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x76 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jbe {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Zero) == 1 || self.regs.getflag(Flags::Carry) == 1 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x77 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jnbe {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Zero) != 0 && self.regs.getflag(Flags::Carry) != 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x78 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "js {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Sign) == 1 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x79 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jns {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Sign) == 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x7a => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jp {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Parity) == 1 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x7b => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jnp {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Parity) == 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x7c => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jl {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Sign) != self.regs.getflag(Flags::Overflow) {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x7d => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jnl {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.getflag(Flags::Sign) == self.regs.getflag(Flags::Overflow) {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x7e => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jle {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                let check =
                    self.regs.getflag(Flags::Overflow) == 1 || self.regs.getflag(Flags::Zero) == 1;
                let check_real;
                if check {
                    check_real = 1;
                } else {
                    check_real = 0;
                }
                if self.regs.getflag(Flags::Sign) != check_real {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x7f => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jnle {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                let check =
                    self.regs.getflag(Flags::Overflow) == 1 && self.regs.getflag(Flags::Zero) == 0;
                let check_real;
                if check {
                    check_real = 1;
                } else {
                    check_real = 0;
                }

                if self.regs.getflag(Flags::Sign) == check_real {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0x81 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                match modrm & 0x38 {
                    0x38 => {
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "cmp {}, {:x}",
                                Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                                imm
                            );
                            let result = self.regs.read16(rm.into()).wrapping_sub(imm);
                            self.setznp16(result);

                            if ((result ^ imm) & (result ^ self.regs.read16(rm.into())) & 0x8000)
                                == 0x8000
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ self.regs.read16(rm.into()) ^ imm) & 0x10) == 0x10 {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if imm > result {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        }
                        if let Operand::Address(segment, ea) = opcode_params.rm {
                            println!(
                                "cmp {}:{}, {:x}",
                                Into::<&'static str>::into(segment),
                                format_offset_for_disasm(
                                    Cpu::get_addr_type_from_modrm16(modrm),
                                    Cpu::get_disp_type_from_modrm(modrm),
                                    self.modrm_disp
                                ),
                                imm
                            );
                            let rm =
                                self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                            let result = rm.wrapping_sub(imm);
                            self.setznp16(result);

                            if ((result ^ imm) & (result ^ rm) & 0x8000) == 0x8000 {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ rm ^ imm) & 0x10) == 0x10 {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if imm > result {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        }
                    }
                    _ => todo!(),
                }
            }
            0x8b => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let reg = opcode_params.reg;
                if let Operand::Register(rm) = opcode_params.rm {
                    self.regs.write16(reg.into(), self.regs.read16(rm.into()));
                    println!(
                        "mov {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(reg)),
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm))
                    );
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    let rm = self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                    self.regs.write16(reg.into(), rm);
                    println!(
                        "mov {}, {}:{}",
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(reg)),
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        )
                    );
                }
            }
            0x8c => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let seg = opcode_params.reg;
                if let Operand::Register(rm) = opcode_params.rm {
                    self.regs.write16(
                        rm.into(),
                        self.regs.segs[Into::<u8>::into(seg) as usize].selector,
                    );
                    println!(
                        "mov {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                        Into::<&'static str>::into(<u8 as Into<SegReg>>::into(seg))
                    );
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    self.mem_write16(
                        bus,
                        self.regs.segs[segment as usize].base + ea,
                        self.regs.segs[Into::<u8>::into(seg) as usize].selector,
                    );
                    println!(
                        "mov {}:{}, {}",
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        ),
                        Into::<&'static str>::into(<u8 as Into<SegReg>>::into(seg))
                    );
                }
            }
            0x8e => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let seg = opcode_params.reg;
                if let Operand::Register(rm) = opcode_params.rm {
                    self.regs
                        .writeseg_realmode(seg.into(), self.regs.read16(rm.into()));
                    println!(
                        "mov {}, {}",
                        Into::<&'static str>::into(<u8 as Into<SegReg>>::into(seg)),
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm))
                    );
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    let rm = self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                    self.regs.writeseg_realmode(seg.into(), rm);
                    println!(
                        "mov {}, {}:{}",
                        Into::<&'static str>::into(<u8 as Into<SegReg>>::into(seg)),
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        )
                    );
                }
            }
            0xa8 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("test al, {:x}", imm);
                let result = self.regs.read8(Reg8::AL) & imm;
                self.setznp8(result);
            }
            0xb0 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("mov al, {:x}", imm);
                self.regs.write8(Reg8::AL, imm);
            }
            0xb1 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("mov cl, {:x}", imm);
                self.regs.write8(Reg8::CL, imm);
            }
            0xb2 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("mov dl, {:x}", imm);
                self.regs.write8(Reg8::DL, imm);
            }
            0xb3 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("mov bl, {:x}", imm);
                self.regs.write8(Reg8::BL, imm);
            }
            0xb4 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("mov ah, {:x}", imm);
                self.regs.write8(Reg8::AH, imm);
            }
            0xb5 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("mov ch, {:x}", imm);
                self.regs.write8(Reg8::CH, imm);
            }
            0xb6 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("mov dh, {:x}", imm);
                self.regs.write8(Reg8::DH, imm);
            }
            0xb7 => {
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("mov bh, {:x}", imm);
                self.regs.write8(Reg8::BH, imm);
            }
            0xb8 => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("mov ax, {:x}", imm);
                self.regs.write16(Reg16::AX, imm);
            }
            0xb9 => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("mov cx, {:x}", imm);
                self.regs.write16(Reg16::CX, imm);
            }
            0xba => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("mov dx, {:x}", imm);
                self.regs.write16(Reg16::DX, imm);
            }
            0xbb => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("mov bx, {:x}", imm);
                self.regs.write16(Reg16::BX, imm);
            }
            0xbc => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("mov sp, {:x}", imm);
                self.regs.write16(Reg16::SP, imm);
            }
            0xbd => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("mov bp, {:x}", imm);
                self.regs.write16(Reg16::BP, imm);
            }
            0xbe => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("mov si, {:x}", imm);
                self.regs.write16(Reg16::SI, imm);
            }
            0xbf => {
                let imm = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("mov di, {:x}", imm);
                self.regs.write16(Reg16::DI, imm);
            }
            0xe3 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "jcxz {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                if self.regs.read16(Reg16::CX) == 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0xe4 => {
                let port = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("in al, {:x}", port);
                let data = self.io_read8(bus, port as u16);
                self.regs.write8(Reg8::AL, data);
            }
            0xe5 => {
                let port = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("in ax, {:x}", port);
                let data = self.io_read16(bus, port as u16);
                self.regs.write16(Reg16::AX, data);
            }
            0xe6 => {
                let port = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("out al, {:x}", port);
                self.io_write8(bus, port as u16, self.regs.read8(Reg8::AL));
            }
            0xe7 => {
                let port = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("out ax, {:x}", port);
                self.io_write16(bus, port as u16, self.regs.read16(Reg16::AX));
            }
            0xe9 => {
                let offset = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("jmp near {:x}", (self.regs.rip as u16).wrapping_add(offset));
                self.regs.rip = (self.regs.rip as u16).wrapping_add(offset) as u64;
            }
            0xea => {
                let newip = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                let newcs = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                println!("jmp far {:x}:{:x}", newcs, newip);

                //no pmode yet lol
                self.regs.writeseg_realmode(SegReg::CS, newcs);
                self.regs.rip = newip as u64;
            }
            0xf5 => {
                println!("cmc");
                self.regs
                    .setflag(Flags::Carry, !self.regs.getflag(Flags::Carry) & 1);
            }
            0xf6 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                match modrm & 0x38 {
                    0x00 => {
                        let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                        let imm = self.mem_read8(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(1);
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "test {}, {:x}",
                                Into::<&'static str>::into(<u8 as Into<Reg8>>::into(rm)),
                                imm
                            );
                            let result = self.regs.read8(rm.into()) & imm;
                            self.setznp8(result);
                        } else if let Operand::Address(segment, ea) = opcode_params.rm {
                            println!(
                                "test {}:{}, {:x}",
                                Into::<&'static str>::into(segment),
                                format_offset_for_disasm(
                                    Cpu::get_addr_type_from_modrm16(modrm),
                                    Cpu::get_disp_type_from_modrm(modrm),
                                    self.modrm_disp
                                ),
                                imm
                            );
                            let rm =
                                self.mem_read8(bus, self.regs.segs[segment as usize].base + ea);
                            let result = rm & imm;
                            self.setznp8(result);
                        }
                    }
                    _ => todo!(),
                }
            }
            0xf8 => {
                println!("clc");
                self.regs.setflag(Flags::Carry, 0);
            }
            0xf9 => {
                println!("stc");
                self.regs.setflag(Flags::Carry, 1);
            }
            0xfa => {
                println!("cli");
                self.regs.setflag(Flags::Interrupt, 0);
            }
            0xfc => {
                println!("cld");
                self.regs.setflag(Flags::Direction, 0);
            }
            0xfd => {
                println!("std");
                self.regs.setflag(Flags::Direction, 1);
            }
            _ => {
                todo!();
            }
        }
        self.segment_override = None;
        self.use_32op_prefix = false;
        self.use_32addr_prefix = false;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_init() {
        let _cpu = Cpu::new();
    }

    #[test]
    fn test_rw8() {
        let mut regs = Registers::new();
        assert_eq!(regs.read8(Reg8::AL), 0);

        regs.write8(Reg8::AL, 5);
        assert_eq!(regs.read8(Reg8::AL), 5);

        regs.write8(Reg8::AH, 2);
        assert_eq!(regs.read8(Reg8::AL), 5);
        assert_eq!(regs.read8(Reg8::AH), 2);

        regs.write8(Reg8::CL, 3);
        assert_eq!(regs.read8(Reg8::AL), 5);
        assert_eq!(regs.read8(Reg8::CL), 3);
        assert_eq!(regs.read8(Reg8::AH), 2);

        regs.write8rex(Reg8REX::SPL, 16);
        assert_eq!(regs.read8(Reg8::AL), 5);
        assert_eq!(regs.read8(Reg8::CL), 3);
        assert_eq!(regs.read8(Reg8::AH), 2);
        assert_eq!(regs.read8rex(Reg8REX::SPL), 16);
        assert_eq!(regs.read16(Reg16::AX), 0x0205);
    }
}
