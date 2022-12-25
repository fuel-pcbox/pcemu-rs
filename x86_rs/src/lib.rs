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
pub enum SegReg {
    ES,
    CS,
    SS,
    DS,
    FS,
    GS,
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
            flags: 0x0000000000000002u64
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
        }
        else {
            self.gpr[regval & 3] as u8
        }
    }

    #[inline(always)]
    pub fn write8(&mut self, reg: Reg8, data: u8) {
        let regval = reg as usize;
        if regval & 4 == 4 {
            self.gpr[regval & 3] &= !0xff00;
            self.gpr[regval & 3] |= (data as u64) << 8;
        }
        else {
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
    pub fn read16(&mut self, reg: Reg16) -> u16 {
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
    pub fn read32(&mut self, reg: Reg32) -> u32 {
        let regval = reg as usize;
        self.gpr[regval] as u32
    }

    #[inline(always)]
    pub fn write32(&mut self, reg: Reg32, data: u32) {
        let regval = reg as usize;
        self.gpr[regval] = data as u64; //32-bit writes zero the upper 32 bits of a register.
    }

    #[inline(always)]
    pub fn read64(&mut self, reg: Reg64) -> u64 {
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

#[derive(Clone, Copy)]
pub struct Cpu {
    pub regs: Registers
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            regs: Registers::reset()
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

    pub fn tick<T: CpuBus>(&mut self, bus: &mut T) {
        let addr = self.regs.segs[SegReg::CS as usize].base + self.regs.rip;
        let opcode = self.mem_read8(bus, addr);
        self.regs.rip = self.regs.rip.wrapping_add(1);
        println!("opcode: {:x}", opcode);
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
