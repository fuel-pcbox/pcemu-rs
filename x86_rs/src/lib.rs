use crate::registers::*;

pub mod registers;

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

#[derive(PartialEq, Clone, Copy)]
pub enum RepStatus {
    Repe,
    Repne,
}

#[derive(Clone, Copy)]
pub struct Cpu {
    pub regs: Registers,
    pub segment_override: Option<SegReg>,
    pub use_32op_default: Use32OpFlags,
    pub use_32op_prefix: bool,
    pub use_32addr_default: Use32AddrFlags,
    pub use_32addr_prefix: bool,
    pub use_32stack: Use32AddrFlags,
    pub rep_status: Option<RepStatus>,
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
            use_32stack: Use32AddrFlags::Bits16,
            rep_status: None,
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

    pub fn push_16<T: CpuBus>(&mut self, bus: &mut T, data: u16) {
        if self.use_32stack == Use32AddrFlags::Bits32 {
            self.mem_write16(
                bus,
                (self.regs.segs[SegReg::SS as usize].base + self.regs.read32(Reg32::ESP) as u64)
                    .wrapping_sub(2),
                data,
            );
            self.regs
                .write32(Reg32::ESP, self.regs.read32(Reg32::ESP).wrapping_sub(2));
        } else {
            self.mem_write16(
                bus,
                (self.regs.segs[SegReg::SS as usize].base + self.regs.read16(Reg16::SP) as u64)
                    .wrapping_sub(2),
                data,
            );
            self.regs
                .write16(Reg16::SP, self.regs.read16(Reg16::SP).wrapping_sub(2));
        }
    }

    pub fn pop_16<T: CpuBus>(&mut self, bus: &mut T) -> u16 {
        if self.use_32stack == Use32AddrFlags::Bits32 {
            let result = self.mem_read16(
                bus,
                self.regs.segs[SegReg::SS as usize].base + self.regs.read32(Reg32::ESP) as u64,
            );
            self.regs
                .write32(Reg32::ESP, self.regs.read32(Reg32::ESP).wrapping_add(2));
            result
        } else {
            let result = self.mem_read16(
                bus,
                self.regs.segs[SegReg::SS as usize].base + self.regs.read16(Reg16::SP) as u64,
            );
            self.regs
                .write16(Reg16::SP, self.regs.read16(Reg16::SP).wrapping_add(2));
            result
        }
    }

    pub fn push_32<T: CpuBus>(&mut self, bus: &mut T, data: u32) {
        if self.use_32stack == Use32AddrFlags::Bits32 {
            self.mem_write32(
                bus,
                (self.regs.segs[SegReg::SS as usize].base + self.regs.read32(Reg32::ESP) as u64)
                    .wrapping_sub(4),
                data,
            );
            self.regs
                .write32(Reg32::ESP, self.regs.read32(Reg32::ESP).wrapping_sub(4));
        } else {
            self.mem_write32(
                bus,
                (self.regs.segs[SegReg::SS as usize].base + self.regs.read16(Reg16::SP) as u64)
                    .wrapping_sub(4),
                data,
            );
            self.regs
                .write16(Reg16::SP, self.regs.read16(Reg16::SP).wrapping_sub(4));
        }
    }

    pub fn pop_32<T: CpuBus>(&mut self, bus: &mut T) -> u32 {
        if self.use_32stack == Use32AddrFlags::Bits32 {
            let result = self.mem_read32(
                bus,
                self.regs.segs[SegReg::SS as usize].base + self.regs.read32(Reg32::ESP) as u64,
            );
            self.regs
                .write32(Reg32::ESP, self.regs.read32(Reg32::ESP).wrapping_add(4));
            result
        } else {
            let result = self.mem_read32(
                bus,
                self.regs.segs[SegReg::SS as usize].base + self.regs.read16(Reg16::SP) as u64,
            );
            self.regs
                .write16(Reg16::SP, self.regs.read16(Reg16::SP).wrapping_add(4));
            result
        }
    }

    pub fn jump_near_16<T: CpuBus>(&mut self, bus: &mut T) {
        let offset = self.mem_read16(
            bus,
            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
        );
        self.regs.rip = self.regs.rip.wrapping_add(2);
        println!("jmp near {:x}", (self.regs.rip as u16).wrapping_add(offset));
        self.regs.rip = (self.regs.rip as u16).wrapping_add(offset) as u64;
    }

    pub fn jump_far_16<T: CpuBus>(&mut self, bus: &mut T) {
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

    pub fn cpu_alu<T: CpuBus>(&mut self, bus: &mut T, op: u8, modrm: u8, imm: Option<u64>) {
        self.regs.rip = self.regs.rip.wrapping_add(1);
        let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);

        match op & 0x38 {
            0x00 => {
                print!("add ");
            }
            0x08 => {
                print!("or ");
            }
            0x10 => {
                print!("adc ");
            }
            0x18 => {
                print!("sbb ");
            }
            0x20 => {
                print!("and ");
            }
            0x28 => {
                print!("sub ");
            }
            0x30 => {
                print!("xor ");
            }
            0x38 => {
                print!("cmp ");
            }
            _ => panic!("invalid alu op"),
        }

        let mut rm: u64 = 0;
        let mut reg: u64 = 0;
        let result: u64;
        let mut store_result = true;
        let carry = self.regs.getflag(Flags::Carry);

        match op & 7 {
            0 => {
                if let Operand::Register(op_rm) = opcode_params.rm {
                    println!(
                        "{}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(op_rm)),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(opcode_params.reg))
                    );
                    rm = self.regs.read8(op_rm.into()) as u64;
                    reg = self.regs.read8(opcode_params.reg.into()) as u64;
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    println!(
                        "{}:{}, {}",
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        ),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(opcode_params.reg)),
                    );
                    rm = self.mem_read8(bus, self.regs.segs[segment as usize].base + ea) as u64;
                    reg = self.regs.read8(opcode_params.reg.into()) as u64;
                }
            }
            1 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    if let Operand::Register(op_rm) = opcode_params.rm {
                        println!(
                            "{}, {}",
                            Into::<&'static str>::into(<u8 as Into<Reg32>>::into(op_rm)),
                            Into::<&'static str>::into(<u8 as Into<Reg32>>::into(
                                opcode_params.reg
                            ))
                        );
                        rm = self.regs.read32(op_rm.into()) as u64;
                        reg = self.regs.read32(opcode_params.reg.into()) as u64;
                    } else if let Operand::Address(segment, ea) = opcode_params.rm {
                        println!(
                            "{}:{}, {}",
                            Into::<&'static str>::into(segment),
                            format_offset_for_disasm(
                                Cpu::get_addr_type_from_modrm16(modrm),
                                Cpu::get_disp_type_from_modrm(modrm),
                                self.modrm_disp
                            ),
                            Into::<&'static str>::into(<u8 as Into<Reg32>>::into(
                                opcode_params.reg
                            )),
                        );
                        rm =
                            self.mem_read32(bus, self.regs.segs[segment as usize].base + ea) as u64;
                        reg = self.regs.read32(opcode_params.reg.into()) as u64;
                    }
                } else {
                    if let Operand::Register(op_rm) = opcode_params.rm {
                        println!(
                            "{}, {}",
                            Into::<&'static str>::into(<u8 as Into<Reg16>>::into(op_rm)),
                            Into::<&'static str>::into(<u8 as Into<Reg16>>::into(
                                opcode_params.reg
                            ))
                        );
                        rm = self.regs.read16(op_rm.into()) as u64;
                        reg = self.regs.read16(opcode_params.reg.into()) as u64;
                    } else if let Operand::Address(segment, ea) = opcode_params.rm {
                        println!(
                            "{}:{}, {}",
                            Into::<&'static str>::into(segment),
                            format_offset_for_disasm(
                                Cpu::get_addr_type_from_modrm16(modrm),
                                Cpu::get_disp_type_from_modrm(modrm),
                                self.modrm_disp
                            ),
                            Into::<&'static str>::into(<u8 as Into<Reg16>>::into(
                                opcode_params.reg
                            )),
                        );
                        rm =
                            self.mem_read16(bus, self.regs.segs[segment as usize].base + ea) as u64;
                        reg = self.regs.read16(opcode_params.reg.into()) as u64;
                    }
                }
            }
            2 => {
                if let Operand::Register(op_rm) = opcode_params.rm {
                    println!(
                        "{}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(opcode_params.reg)),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(op_rm))
                    );
                    rm = self.regs.read8(op_rm.into()) as u64;
                    reg = self.regs.read8(opcode_params.reg.into()) as u64;
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    println!(
                        "{}, {}:{}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(opcode_params.reg)),
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        )
                    );
                    rm = self.mem_read8(bus, self.regs.segs[segment as usize].base + ea) as u64;
                    reg = self.regs.read8(opcode_params.reg.into()) as u64;
                }
            }
            3 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    if let Operand::Register(op_rm) = opcode_params.rm {
                        println!(
                            "{}, {}",
                            Into::<&'static str>::into(<u8 as Into<Reg32>>::into(
                                opcode_params.reg
                            )),
                            Into::<&'static str>::into(<u8 as Into<Reg32>>::into(op_rm))
                        );
                        rm = self.regs.read32(op_rm.into()) as u64;
                        reg = self.regs.read32(opcode_params.reg.into()) as u64;
                    } else if let Operand::Address(segment, ea) = opcode_params.rm {
                        println!(
                            "{}, {}:{}",
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
                        rm =
                            self.mem_read32(bus, self.regs.segs[segment as usize].base + ea) as u64;
                        reg = self.regs.read32(opcode_params.reg.into()) as u64;
                    }
                } else {
                    if let Operand::Register(op_rm) = opcode_params.rm {
                        println!(
                            "{}, {}",
                            Into::<&'static str>::into(<u8 as Into<Reg16>>::into(
                                opcode_params.reg
                            )),
                            Into::<&'static str>::into(<u8 as Into<Reg16>>::into(op_rm))
                        );
                        rm = self.regs.read16(op_rm.into()) as u64;
                        reg = self.regs.read16(opcode_params.reg.into()) as u64;
                    } else if let Operand::Address(segment, ea) = opcode_params.rm {
                        println!(
                            "{}, {}:{}",
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
                        rm =
                            self.mem_read16(bus, self.regs.segs[segment as usize].base + ea) as u64;
                        reg = self.regs.read16(opcode_params.reg.into()) as u64;
                    }
                }
            }
            4 => {
                println!("al, {:x}", imm.unwrap() as u8);
                rm = imm.unwrap() as u8 as u64;
                reg = self.regs.read8(Reg8::AL) as u64;
            }
            5 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("eax, {:x}", imm.unwrap() as u32);
                    rm = imm.unwrap() as u32 as u64;
                    reg = self.regs.read32(Reg32::EAX) as u64;
                } else {
                    println!("ax, {:x}", imm.unwrap() as u16);
                    rm = imm.unwrap() as u16 as u64;
                    reg = self.regs.read16(Reg16::AX) as u64;
                }
            }
            _ => panic!("invalid alu op"),
        }

        match op & 0x38 {
            0x00 | 0x10 => {
                if (op & 0x38) == 0x00 {
                    result = reg.wrapping_add(rm);
                } else {
                    result = reg.wrapping_add(rm).wrapping_add(carry as u64);
                }
                match op & 7 {
                    0 | 2 | 4 => {
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
                    1 | 3 | 5 => {
                        if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                            || (self.use_32op_default == Use32OpFlags::Bits16
                                && self.use_32op_prefix)
                        {
                            if ((reg ^ rm) & 0x80000000 == 0x80000000)
                                && ((reg ^ result) & 0x80000000) == 0x80000000
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ rm ^ reg) & 0x10) == 0x10 {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if (rm & 0x80000000) > (result & 0x80000000) {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        } else {
                            if ((reg ^ rm) & 0x8000 == 0x8000)
                                && ((reg ^ result) & 0x8000) == 0x8000
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ rm ^ reg) & 0x10) == 0x10 {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if (rm & 0x8000) > (result & 0x8000) {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        }
                    }
                    _ => panic!("invalid alu op"),
                }
            }
            0x08 => {
                result = reg | rm;
                match op & 7 {
                    0 | 2 | 4 => self.setznp8(result as u8),
                    1 | 3 | 5 => {
                        if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                            || (self.use_32op_default == Use32OpFlags::Bits16
                                && self.use_32op_prefix)
                        {
                            self.setznp32(result as u32);
                        } else {
                            self.setznp16(result as u16);
                        }
                    }
                    _ => panic!("invalid alu op"),
                }
            }
            0x18 | 0x28 | 0x38 => {
                if (op & 0x38) == 0x28 || (op & 0x38) == 0x38 {
                    result = reg.wrapping_sub(rm);
                    if (op & 0x38) == 0x38 {
                        store_result = false;
                    }
                } else {
                    result = reg.wrapping_sub(rm.wrapping_add(carry as u64));
                }
                match op & 7 {
                    0 | 2 | 4 => {
                        if ((result ^ rm) & 0x80 == 0x80) && ((reg ^ result) & 0x80) == 0x80 {
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
                    1 | 3 | 5 => {
                        if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                            || (self.use_32op_default == Use32OpFlags::Bits16
                                && self.use_32op_prefix)
                        {
                            if ((reg ^ rm) & 0x80000000 == 0x80000000)
                                && ((reg ^ result) & 0x80000000) == 0x80000000
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ rm ^ reg) & 0x10) == 0x10 {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if (rm & 0x80000000) > (result & 0x80000000) {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        } else {
                            if ((reg ^ rm) & 0x8000 == 0x8000)
                                && ((reg ^ result) & 0x8000) == 0x8000
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ rm ^ reg) & 0x10) == 0x10 {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if (rm & 0x8000) > (result & 0x8000) {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        }
                    }
                    _ => panic!("invalid alu op"),
                }
            }
            0x20 => {
                result = reg & rm;
                match op & 7 {
                    0 | 2 | 4 => self.setznp8(result as u8),
                    1 | 3 | 5 => {
                        if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                            || (self.use_32op_default == Use32OpFlags::Bits16
                                && self.use_32op_prefix)
                        {
                            self.setznp32(result as u32);
                        } else {
                            self.setznp16(result as u16);
                        }
                    }
                    _ => panic!("invalid alu op"),
                }
            }
            0x30 => {
                result = reg ^ rm;
                match op & 7 {
                    0 | 2 | 4 => self.setznp8(result as u8),
                    1 | 3 | 5 => {
                        if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                            || (self.use_32op_default == Use32OpFlags::Bits16
                                && self.use_32op_prefix)
                        {
                            self.setznp32(result as u32);
                        } else {
                            self.setznp16(result as u16);
                        }
                    }
                    _ => panic!("invalid alu op"),
                }
            }
            _ => panic!("invalid alu op"),
        }

        if store_result {
            match op & 7 {
                0 => {
                    if let Operand::Register(op_rm) = opcode_params.rm {
                        self.regs.write8(op_rm.into(), result as u8);
                    } else if let Operand::Address(segment, ea) = opcode_params.rm {
                        self.mem_write8(
                            bus,
                            self.regs.segs[segment as usize].base + ea,
                            result as u8,
                        );
                    }
                }
                1 => {
                    if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                        || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                    {
                        if let Operand::Register(op_rm) = opcode_params.rm {
                            self.regs.write32(op_rm.into(), result as u32);
                        } else if let Operand::Address(segment, ea) = opcode_params.rm {
                            self.mem_write32(
                                bus,
                                self.regs.segs[segment as usize].base + ea,
                                result as u32,
                            );
                        }
                    } else {
                        if let Operand::Register(op_rm) = opcode_params.rm {
                            self.regs.write16(op_rm.into(), result as u16);
                        } else if let Operand::Address(segment, ea) = opcode_params.rm {
                            self.mem_write16(
                                bus,
                                self.regs.segs[segment as usize].base + ea,
                                result as u16,
                            );
                        }
                    }
                }
                2 => {
                    self.regs.write8(opcode_params.reg.into(), result as u8);
                }
                3 => {
                    if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                        || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                    {
                        self.regs.write32(opcode_params.reg.into(), result as u32);
                    } else {
                        self.regs.write16(opcode_params.reg.into(), result as u16);
                    }
                }
                4 => {
                    self.regs.write8(Reg8::AL, result as u8);
                }
                5 => {
                    if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                        || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                    {
                        self.regs.write32(Reg32::EAX, result as u32);
                    } else {
                        self.regs.write16(Reg16::AX, result as u16);
                    }
                }
                _ => panic!("invalid alu op"),
            }
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
            0x00..=0x03
            | 0x08..=0x0b
            | 0x10..=0x13
            | 0x18..=0x1b
            | 0x20..=0x23
            | 0x28..=0x2b
            | 0x30..=0x33
            | 0x38..=0x3b => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.cpu_alu(bus, opcode, modrm, None);
            }
            0x04 | 0x05 | 0x0c | 0x0d | 0x14 | 0x15 | 0x1c | 0x1d | 0x24 | 0x25 | 0x2c | 0x2d
            | 0x34 | 0x35 | 0x3c | 0x3d => {
                let imm: u64;
                if opcode & 1 == 1 {
                    if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                        || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                    {
                        imm = self.mem_read32(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        ) as u64;
                        self.regs.rip = self.regs.rip.wrapping_add(4);
                    } else {
                        imm = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        ) as u64;
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                    }
                    self.cpu_alu(bus, opcode, 0, Some(imm));
                } else {
                    imm = self.mem_read8(
                        bus,
                        self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                    ) as u64;
                    self.cpu_alu(bus, opcode, 0, Some(imm));
                }
            }
            0x06 => {
                println!("push es");
                self.push_16(bus, self.regs.segs[SegReg::ES as usize].selector);
            }
            0x07 => {
                println!("pop es");
                let popped = self.pop_16(bus);
                self.regs.writeseg(SegReg::ES, popped);
            }
            0x0e => {
                println!("push cs");
                self.push_16(bus, self.regs.segs[SegReg::CS as usize].selector);
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
                    0x80 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jo {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Overflow) == 1 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x81 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jno {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Overflow) == 0 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x82 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jc {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Carry) == 1 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x83 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jnc {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Carry) == 0 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x84 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jz {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Zero) == 1 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x85 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jnz {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Zero) == 0 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x86 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jbe {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Zero) == 1
                            || self.regs.getflag(Flags::Carry) == 1
                        {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x87 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jnbe {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Zero) != 0
                            && self.regs.getflag(Flags::Carry) != 0
                        {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x88 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "js {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Sign) == 1 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x89 => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jns {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Sign) == 0 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x8a => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jp {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Parity) == 1 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x8b => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jnp {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Parity) == 0 {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x8c => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jl {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Sign) != self.regs.getflag(Flags::Overflow) {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x8d => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jnl {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        if self.regs.getflag(Flags::Sign) == self.regs.getflag(Flags::Overflow) {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x8e => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jle {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        let check = self.regs.getflag(Flags::Overflow) == 1
                            || self.regs.getflag(Flags::Zero) == 1;
                        let check_real;
                        if check {
                            check_real = 1;
                        } else {
                            check_real = 0;
                        }
                        if self.regs.getflag(Flags::Sign) != check_real {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    0x8f => {
                        let offset = self.mem_read16(
                            bus,
                            self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                        );
                        self.regs.rip = self.regs.rip.wrapping_add(2);
                        println!(
                            "jnle {:x}",
                            (self.regs.rip as u16).wrapping_add(offset as i16 as u16)
                        );
                        let check = self.regs.getflag(Flags::Overflow) == 1
                            || self.regs.getflag(Flags::Zero) == 1;
                        let check_real;
                        if check {
                            check_real = 1;
                        } else {
                            check_real = 0;
                        }
                        if self.regs.getflag(Flags::Sign) == check_real {
                            self.regs.rip =
                                (self.regs.rip as u16).wrapping_add(offset as i16 as u16) as u64;
                        }
                    }
                    _ => todo!(),
                }
            }
            0x16 => {
                println!("push ss");
                self.push_16(bus, self.regs.segs[SegReg::SS as usize].selector);
            }
            0x17 => {
                println!("pop ss");
                let popped = self.pop_16(bus);
                self.regs.writeseg(SegReg::SS, popped);
            }
            0x1e => {
                println!("push ds");
                self.push_16(bus, self.regs.segs[SegReg::DS as usize].selector);
            }
            0x1f => {
                println!("pop ds");
                let popped = self.pop_16(bus);
                self.regs.writeseg(SegReg::DS, popped);
            }
            0x26 => {
                self.segment_override = Some(SegReg::ES);
                println!("ES:");
                self.tick(bus);
            }
            0x2e => {
                self.segment_override = Some(SegReg::CS);
                println!("CS:");
                self.tick(bus);
            }
            0x36 => {
                self.segment_override = Some(SegReg::SS);
                println!("SS:");
                self.tick(bus);
            }
            0x3e => {
                self.segment_override = Some(SegReg::DS);
                println!("DS:");
                self.tick(bus);
            }
            0x40 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("inc eax");
                    self.regs
                        .write32(Reg32::EAX, self.regs.read32(Reg32::EAX).wrapping_add(1));
                    self.setznp32(self.regs.read32(Reg32::EAX));
                } else {
                    println!("inc ax");
                    self.regs
                        .write16(Reg16::AX, self.regs.read16(Reg16::AX).wrapping_add(1));
                    self.setznp16(self.regs.read16(Reg16::AX));
                }
            }
            0x41 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("inc ecx");
                    self.regs
                        .write32(Reg32::ECX, self.regs.read32(Reg32::ECX).wrapping_add(1));
                    self.setznp32(self.regs.read32(Reg32::ECX));
                } else {
                    println!("inc cx");
                    self.regs
                        .write16(Reg16::CX, self.regs.read16(Reg16::CX).wrapping_add(1));
                    self.setznp16(self.regs.read16(Reg16::CX));
                }
            }
            0x42 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("inc edx");
                    self.regs
                        .write32(Reg32::EDX, self.regs.read32(Reg32::EDX).wrapping_add(1));
                    self.setznp32(self.regs.read32(Reg32::EDX));
                } else {
                    println!("inc dx");
                    self.regs
                        .write16(Reg16::DX, self.regs.read16(Reg16::DX).wrapping_add(1));
                    self.setznp16(self.regs.read16(Reg16::DX));
                }
            }
            0x43 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("inc ebx");
                    self.regs
                        .write32(Reg32::EBX, self.regs.read32(Reg32::EBX).wrapping_add(1));
                    self.setznp32(self.regs.read32(Reg32::EBX));
                } else {
                    println!("inc bx");
                    self.regs
                        .write16(Reg16::BX, self.regs.read16(Reg16::BX).wrapping_add(1));
                    self.setznp16(self.regs.read16(Reg16::BX));
                }
            }
            0x44 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("inc esp");
                    self.regs
                        .write32(Reg32::ESP, self.regs.read32(Reg32::ESP).wrapping_add(1));
                    self.setznp32(self.regs.read32(Reg32::ESP));
                } else {
                    println!("inc sp");
                    self.regs
                        .write16(Reg16::SP, self.regs.read16(Reg16::SP).wrapping_add(1));
                    self.setznp16(self.regs.read16(Reg16::SP));
                }
            }
            0x45 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("inc ebp");
                    self.regs
                        .write32(Reg32::EBP, self.regs.read32(Reg32::EBP).wrapping_add(1));
                    self.setznp32(self.regs.read32(Reg32::EBP));
                } else {
                    println!("inc bp");
                    self.regs
                        .write16(Reg16::BP, self.regs.read16(Reg16::BP).wrapping_add(1));
                    self.setznp16(self.regs.read16(Reg16::BP));
                }
            }
            0x46 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("inc esi");
                    self.regs
                        .write32(Reg32::ESI, self.regs.read32(Reg32::ESI).wrapping_add(1));
                    self.setznp32(self.regs.read32(Reg32::ESI));
                } else {
                    println!("inc si");
                    self.regs
                        .write16(Reg16::SI, self.regs.read16(Reg16::SI).wrapping_add(1));
                    self.setznp16(self.regs.read16(Reg16::SI));
                }
            }
            0x47 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("inc edi");
                    self.regs
                        .write32(Reg32::EDI, self.regs.read32(Reg32::EDI).wrapping_add(1));
                    self.setznp32(self.regs.read32(Reg32::EDI));
                } else {
                    println!("inc di");
                    self.regs
                        .write16(Reg16::DI, self.regs.read16(Reg16::DI).wrapping_add(1));
                    self.setznp16(self.regs.read16(Reg16::DI));
                }
            }
            0x48 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("dec eax");
                    self.regs
                        .write32(Reg32::EAX, self.regs.read32(Reg32::EAX).wrapping_sub(1));
                    self.setznp32(self.regs.read32(Reg32::EAX));
                } else {
                    println!("dec ax");
                    self.regs
                        .write16(Reg16::AX, self.regs.read16(Reg16::AX).wrapping_sub(1));
                    self.setznp16(self.regs.read16(Reg16::AX));
                }
            }
            0x49 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("dec ecx");
                    self.regs
                        .write32(Reg32::ECX, self.regs.read32(Reg32::ECX).wrapping_sub(1));
                    self.setznp32(self.regs.read32(Reg32::ECX));
                } else {
                    println!("dec cx");
                    self.regs
                        .write16(Reg16::CX, self.regs.read16(Reg16::CX).wrapping_sub(1));
                    self.setznp16(self.regs.read16(Reg16::CX));
                }
            }
            0x4a => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("dec edx");
                    self.regs
                        .write32(Reg32::EDX, self.regs.read32(Reg32::EDX).wrapping_sub(1));
                    self.setznp32(self.regs.read32(Reg32::EDX));
                } else {
                    println!("dec dx");
                    self.regs
                        .write16(Reg16::DX, self.regs.read16(Reg16::DX).wrapping_sub(1));
                    self.setznp16(self.regs.read16(Reg16::DX));
                }
            }
            0x4b => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("dec ebx");
                    self.regs
                        .write32(Reg32::EBX, self.regs.read32(Reg32::EBX).wrapping_sub(1));
                    self.setznp32(self.regs.read32(Reg32::EBX));
                } else {
                    println!("dec bx");
                    self.regs
                        .write16(Reg16::BX, self.regs.read16(Reg16::BX).wrapping_sub(1));
                    self.setznp16(self.regs.read16(Reg16::BX));
                }
            }
            0x4c => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("dec esp");
                    self.regs
                        .write32(Reg32::ESP, self.regs.read32(Reg32::ESP).wrapping_sub(1));
                    self.setznp32(self.regs.read32(Reg32::ESP));
                } else {
                    println!("dec sp");
                    self.regs
                        .write16(Reg16::SP, self.regs.read16(Reg16::SP).wrapping_sub(1));
                    self.setznp16(self.regs.read16(Reg16::SP));
                }
            }
            0x4d => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("dec ebp");
                    self.regs
                        .write32(Reg32::EBP, self.regs.read32(Reg32::EBP).wrapping_sub(1));
                    self.setznp32(self.regs.read32(Reg32::EBP));
                } else {
                    println!("dec bp");
                    self.regs
                        .write16(Reg16::BP, self.regs.read16(Reg16::BP).wrapping_sub(1));
                    self.setznp16(self.regs.read16(Reg16::BP));
                }
            }
            0x4e => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("dec esi");
                    self.regs
                        .write32(Reg32::ESI, self.regs.read32(Reg32::ESI).wrapping_sub(1));
                    self.setznp32(self.regs.read32(Reg32::ESI));
                } else {
                    println!("dec si");
                    self.regs
                        .write16(Reg16::SI, self.regs.read16(Reg16::SI).wrapping_sub(1));
                    self.setznp16(self.regs.read16(Reg16::SI));
                }
            }
            0x4f => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("dec edi");
                    self.regs
                        .write32(Reg32::EDI, self.regs.read32(Reg32::EDI).wrapping_sub(1));
                    self.setznp32(self.regs.read32(Reg32::EDI));
                } else {
                    println!("dec di");
                    self.regs
                        .write16(Reg16::DI, self.regs.read16(Reg16::DI).wrapping_sub(1));
                    self.setznp16(self.regs.read16(Reg16::DI));
                }
            }
            0x50 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("push eax");
                    self.push_32(bus, self.regs.read32(Reg32::EAX));
                } else {
                    println!("push ax");
                    self.push_16(bus, self.regs.read16(Reg16::AX));
                }
            }
            0x51 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("push ecx");
                    self.push_32(bus, self.regs.read32(Reg32::ECX));
                } else {
                    println!("push cx");
                    self.push_16(bus, self.regs.read16(Reg16::CX));
                }
            }
            0x52 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("push edx");
                    self.push_32(bus, self.regs.read32(Reg32::EDX));
                } else {
                    println!("push dx");
                    self.push_16(bus, self.regs.read16(Reg16::DX));
                }
            }
            0x53 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("push ebx");
                    self.push_32(bus, self.regs.read32(Reg32::EBX));
                } else {
                    println!("push bx");
                    self.push_16(bus, self.regs.read16(Reg16::BX));
                }
            }
            0x54 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("push esp");
                    self.push_32(bus, self.regs.read32(Reg32::ESP));
                } else {
                    println!("push sp");
                    self.push_16(bus, self.regs.read16(Reg16::SP));
                }
            }
            0x55 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("push ebp");
                    self.push_32(bus, self.regs.read32(Reg32::EBP));
                } else {
                    println!("push bp");
                    self.push_16(bus, self.regs.read16(Reg16::BP));
                }
            }
            0x56 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("push esi");
                    self.push_32(bus, self.regs.read32(Reg32::ESI));
                } else {
                    println!("push si");
                    self.push_16(bus, self.regs.read16(Reg16::SI));
                }
            }
            0x57 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("push edi");
                    self.push_32(bus, self.regs.read32(Reg32::EDI));
                } else {
                    println!("push di");
                    self.push_16(bus, self.regs.read16(Reg16::DI));
                }
            }
            0x58 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("pop eax");
                    let popped = self.pop_32(bus);
                    self.regs.write32(Reg32::EAX, popped);
                } else {
                    println!("pop ax");
                    let popped = self.pop_16(bus);
                    self.regs.write16(Reg16::AX, popped);
                }
            }
            0x59 => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("pop ecx");
                    let popped = self.pop_32(bus);
                    self.regs.write32(Reg32::ECX, popped);
                } else {
                    println!("pop cx");
                    let popped = self.pop_16(bus);
                    self.regs.write16(Reg16::CX, popped);
                }
            }
            0x5a => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("pop edx");
                    let popped = self.pop_32(bus);
                    self.regs.write32(Reg32::EDX, popped);
                } else {
                    println!("pop dx");
                    let popped = self.pop_16(bus);
                    self.regs.write16(Reg16::DX, popped);
                }
            }
            0x5b => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("pop ebx");
                    let popped = self.pop_32(bus);
                    self.regs.write32(Reg32::EBX, popped);
                } else {
                    println!("pop bx");
                    let popped = self.pop_16(bus);
                    self.regs.write16(Reg16::BX, popped);
                }
            }
            0x5c => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("pop esp");
                    let popped = self.pop_32(bus);
                    self.regs.write32(Reg32::ESP, popped);
                } else {
                    println!("pop sp");
                    let popped = self.pop_16(bus);
                    self.regs.write16(Reg16::SP, popped);
                }
            }
            0x5d => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("pop ebp");
                    let popped = self.pop_32(bus);
                    self.regs.write32(Reg32::EBP, popped);
                } else {
                    println!("pop bp");
                    let popped = self.pop_16(bus);
                    self.regs.write16(Reg16::BP, popped);
                }
            }
            0x5e => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("pop esi");
                    let popped = self.pop_32(bus);
                    self.regs.write32(Reg32::ESI, popped);
                } else {
                    println!("pop si");
                    let popped = self.pop_16(bus);
                    self.regs.write16(Reg16::SI, popped);
                }
            }
            0x5f => {
                if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                    || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                {
                    println!("pop edi");
                    let popped = self.pop_32(bus);
                    self.regs.write32(Reg32::EDI, popped);
                } else {
                    println!("pop di");
                    let popped = self.pop_16(bus);
                    self.regs.write16(Reg16::DI, popped);
                }
            }
            0x64 => {
                self.segment_override = Some(SegReg::FS);
                println!("FS:");
                self.tick(bus);
            }
            0x65 => {
                self.segment_override = Some(SegReg::GS);
                println!("GS:");
                self.tick(bus);
            }
            0x66 => {
                self.use_32op_prefix = true;
                println!("OPSIZE:");
                self.tick(bus);
            }
            0x67 => {
                self.use_32addr_prefix = true;
                println!("ADDRSIZE:");
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
            0x80 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                match modrm & 0x38 {
                    0x38 => {
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "cmp {}, {:x}",
                                Into::<&'static str>::into(<u8 as Into<Reg8>>::into(rm)),
                                imm
                            );
                            let result = self.regs.read8(rm.into()).wrapping_sub(imm);
                            self.setznp8(result);

                            if ((result ^ imm) & (result ^ self.regs.read8(rm.into())) & 0x80)
                                == 0x80
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ self.regs.read8(rm.into()) ^ imm) & 0x10) == 0x10 {
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
                                self.mem_read8(bus, self.regs.segs[segment as usize].base + ea);
                            let result = rm.wrapping_sub(imm);
                            self.setznp8(result);

                            if ((result ^ imm) & (result ^ rm) & 0x80) == 0x80 {
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
                self.regs.rip = self.regs.rip.wrapping_add(2);
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
            0x83 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let imm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                match modrm & 0x38 {
                    0x00 => {
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "add {}, {:x}",
                                Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                                imm
                            );
                            let result = self.regs.read16(rm.into()).wrapping_add(imm as u16);
                            self.setznp16(result);

                            if ((imm as u16 ^ self.regs.read16(rm.into())) & 0x8000 == 0x8000)
                                && ((imm as u16 ^ result) & 0x8000) == 0x8000
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ self.regs.read16(rm.into()) ^ imm as u16) & 0x10) == 0x10
                            {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if (self.regs.read16(rm.into()) & 0x8000) > (result & 0x8000) {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        }
                        if let Operand::Address(segment, ea) = opcode_params.rm {
                            println!(
                                "add {}:{}, {:x}",
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
                            let result = rm.wrapping_add(imm as u16);
                            self.setznp16(result);

                            if ((imm as u16 ^ rm) & 0x8000 == 0x8000)
                                && ((imm as u16 ^ result) & 0x8000) == 0x8000
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ rm ^ imm as u16) & 0x10) == 0x10 {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if (rm & 0x8000) > (result & 0x8000) {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        }
                    }
                    0x38 => {
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "cmp {}, {:x}",
                                Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                                imm
                            );
                            let result = self.regs.read16(rm.into()).wrapping_sub(imm as u16);
                            self.setznp16(result);

                            if ((result ^ imm as u16)
                                & (result ^ self.regs.read16(rm.into()))
                                & 0x8000)
                                == 0x8000
                            {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ self.regs.read16(rm.into()) ^ imm as u16) & 0x10) == 0x10
                            {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if imm as u16 > result {
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
                            let result = rm.wrapping_sub(imm as u16);
                            self.setznp16(result);

                            if ((result ^ imm as u16) & (result ^ rm) & 0x8000) == 0x8000 {
                                self.regs.setflag(Flags::Overflow, 1);
                            } else {
                                self.regs.setflag(Flags::Overflow, 0);
                            }

                            if ((result ^ rm ^ imm as u16) & 0x10) == 0x10 {
                                self.regs.setflag(Flags::Adjust, 1);
                            } else {
                                self.regs.setflag(Flags::Adjust, 0);
                            }

                            if imm as u16 > result {
                                self.regs.setflag(Flags::Carry, 1);
                            } else {
                                self.regs.setflag(Flags::Carry, 0);
                            }
                        }
                    }
                    _ => todo!(),
                }
            }
            0x84 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let reg = opcode_params.reg;
                if let Operand::Register(rm) = opcode_params.rm {
                    let result = self.regs.read8(reg.into()) & self.regs.read8(rm.into());
                    println!(
                        "test {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(rm)),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(reg))
                    );
                    self.setznp8(result);
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    let result = self.regs.read8(reg.into())
                        & self.mem_read8(bus, self.regs.segs[segment as usize].base + ea);
                    println!(
                        "test {}:{}, {}",
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        ),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(reg))
                    );
                    self.setznp8(result);
                }
            }
            0x85 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let reg = opcode_params.reg;
                if let Operand::Register(rm) = opcode_params.rm {
                    let result = self.regs.read16(reg.into()) & self.regs.read16(rm.into());
                    println!(
                        "test {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(reg))
                    );
                    self.setznp16(result);
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    let result = self.regs.read16(reg.into())
                        & self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                    println!(
                        "test {}:{}, {}",
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        ),
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(reg))
                    );
                    self.setznp16(result);
                }
            }
            0x88 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let reg = opcode_params.reg;
                if let Operand::Register(rm) = opcode_params.rm {
                    self.regs.write8(rm.into(), self.regs.read8(reg.into()));
                    println!(
                        "mov {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(rm)),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(reg))
                    );
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    self.mem_write8(
                        bus,
                        self.regs.segs[segment as usize].base + ea,
                        self.regs.read8(reg.into()),
                    );
                    println!(
                        "mov {}:{}, {}",
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        ),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(reg))
                    );
                }
            }
            0x89 => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let reg = opcode_params.reg;
                if let Operand::Register(rm) = opcode_params.rm {
                    self.regs.write16(rm.into(), self.regs.read16(reg.into()));
                    println!(
                        "mov {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(reg))
                    );
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    self.mem_write16(
                        bus,
                        self.regs.segs[segment as usize].base + ea,
                        self.regs.read16(reg.into()),
                    );
                    println!(
                        "mov {}:{}, {}",
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        ),
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(reg))
                    );
                }
            }
            0x8a => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let reg = opcode_params.reg;
                if let Operand::Register(rm) = opcode_params.rm {
                    self.regs.write8(reg.into(), self.regs.read8(rm.into()));
                    println!(
                        "mov {}, {}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(reg)),
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(rm))
                    );
                } else if let Operand::Address(segment, ea) = opcode_params.rm {
                    let rm = self.mem_read8(bus, self.regs.segs[segment as usize].base + ea);
                    self.regs.write8(reg.into(), rm);
                    println!(
                        "mov {}, {}:{}",
                        Into::<&'static str>::into(<u8 as Into<Reg8>>::into(reg)),
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        )
                    );
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
            0x8d => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                let reg = opcode_params.reg;
                if let Operand::Address(segment, ea) = opcode_params.rm {
                    self.regs.write16(reg.into(), ea as u16);
                    println!(
                        "lea {}, {}:{}",
                        Into::<&'static str>::into(<u8 as Into<Reg16>>::into(reg)),
                        Into::<&'static str>::into(segment),
                        format_offset_for_disasm(
                            Cpu::get_addr_type_from_modrm16(modrm),
                            Cpu::get_disp_type_from_modrm(modrm),
                            self.modrm_disp
                        )
                    );
                } else {
                    panic!("ILLEGAL LEA OPCODE");
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
            0x90 => {
                println!("nop");
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
            0xaa => {
                println!("stosb");
                let mut segment = SegReg::ES;
                if let Some(segmentoverride) = self.segment_override {
                    segment = segmentoverride;
                }

                if (self.use_32addr_default == Use32AddrFlags::Bits32 && !self.use_32addr_prefix)
                    || (self.use_32addr_default == Use32AddrFlags::Bits16 && self.use_32addr_prefix)
                {
                    self.mem_write8(
                        bus,
                        self.regs.segs[segment as usize].base + self.regs.read32(Reg32::EDI) as u64,
                        self.regs.read8(Reg8::AL),
                    );
                    if self.regs.getflag(Flags::Direction) == 0 {
                        self.regs
                            .write32(Reg32::EDI, self.regs.read32(Reg32::EDI).wrapping_add(1));
                    } else {
                        self.regs
                            .write32(Reg32::EDI, self.regs.read32(Reg32::EDI).wrapping_sub(1));
                    }
                } else {
                    self.mem_write8(
                        bus,
                        self.regs.segs[segment as usize].base + self.regs.read16(Reg16::DI) as u64,
                        self.regs.read8(Reg8::AL),
                    );
                    if self.regs.getflag(Flags::Direction) == 0 {
                        self.regs
                            .write16(Reg16::DI, self.regs.read16(Reg16::DI).wrapping_add(1));
                    } else {
                        self.regs
                            .write16(Reg16::DI, self.regs.read16(Reg16::DI).wrapping_sub(1));
                    }
                }
                if self.rep_status != None {
                    self.regs
                        .write32(Reg32::ECX, self.regs.read32(Reg32::ECX).wrapping_sub(1));
                    if self.regs.read32(Reg32::ECX) != 0 {
                        self.regs.rip = self.regs.rip.wrapping_sub(2);
                        if self.use_32addr_prefix {
                            self.regs.rip = self.regs.rip.wrapping_sub(1);
                        }
                    }
                }
            }
            0xab => {
                let mut segment = SegReg::ES;
                if let Some(segmentoverride) = self.segment_override {
                    segment = segmentoverride;
                }

                if (self.use_32addr_default == Use32AddrFlags::Bits32 && !self.use_32addr_prefix)
                    || (self.use_32addr_default == Use32AddrFlags::Bits16 && self.use_32addr_prefix)
                {
                    if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                        || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                    {
                        println!("stosd");
                        self.mem_write32(
                            bus,
                            self.regs.segs[segment as usize].base
                                + self.regs.read32(Reg32::EDI) as u64,
                            self.regs.read32(Reg32::EAX),
                        );
                        if self.regs.getflag(Flags::Direction) == 0 {
                            self.regs
                                .write32(Reg32::EDI, self.regs.read32(Reg32::EDI).wrapping_add(4));
                        } else {
                            self.regs
                                .write32(Reg32::EDI, self.regs.read32(Reg32::EDI).wrapping_sub(4));
                        }
                    } else {
                        println!("stosw");
                        self.mem_write16(
                            bus,
                            self.regs.segs[segment as usize].base
                                + self.regs.read32(Reg32::EDI) as u64,
                            self.regs.read16(Reg16::AX),
                        );
                        if self.regs.getflag(Flags::Direction) == 0 {
                            self.regs
                                .write32(Reg32::EDI, self.regs.read32(Reg32::EDI).wrapping_add(2));
                        } else {
                            self.regs
                                .write32(Reg32::EDI, self.regs.read32(Reg32::EDI).wrapping_sub(2));
                        }
                    }
                } else {
                    if (self.use_32op_default == Use32OpFlags::Bits32 && !self.use_32op_prefix)
                        || (self.use_32op_default == Use32OpFlags::Bits16 && self.use_32op_prefix)
                    {
                        println!("stosd");
                        self.mem_write32(
                            bus,
                            self.regs.segs[segment as usize].base
                                + self.regs.read16(Reg16::DI) as u64,
                            self.regs.read32(Reg32::EAX),
                        );
                        if self.regs.getflag(Flags::Direction) == 0 {
                            self.regs
                                .write16(Reg16::DI, self.regs.read16(Reg16::DI).wrapping_add(4));
                        } else {
                            self.regs
                                .write16(Reg16::DI, self.regs.read16(Reg16::DI).wrapping_sub(4));
                        }
                    } else {
                        println!("stosw");
                        self.mem_write16(
                            bus,
                            self.regs.segs[segment as usize].base
                                + self.regs.read16(Reg16::DI) as u64,
                            self.regs.read16(Reg16::AX),
                        );
                        if self.regs.getflag(Flags::Direction) == 0 {
                            self.regs
                                .write16(Reg16::DI, self.regs.read16(Reg16::DI).wrapping_add(2));
                        } else {
                            self.regs
                                .write16(Reg16::DI, self.regs.read16(Reg16::DI).wrapping_sub(2));
                        }
                    }
                }
                if self.rep_status != None {
                    self.regs
                        .write32(Reg32::ECX, self.regs.read32(Reg32::ECX).wrapping_sub(1));
                    if self.regs.read32(Reg32::ECX) != 0 {
                        self.regs.rip = self.regs.rip.wrapping_sub(2);
                        if self.use_32op_prefix {
                            self.regs.rip = self.regs.rip.wrapping_sub(1);
                        }
                        if self.use_32addr_prefix {
                            self.regs.rip = self.regs.rip.wrapping_sub(1);
                        }
                    }
                }
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
            0xc3 => {
                println!("ret near");
                self.regs.rip = self.pop_16(bus) as u64;
            }
            0xe0 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "loopnz {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                self.regs
                    .write16(Reg16::CX, self.regs.read16(Reg16::CX).wrapping_sub(1));
                if self.regs.read16(Reg16::CX) != 0 && self.regs.getflag(Flags::Zero) == 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0xe1 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "loopz {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                self.regs
                    .write16(Reg16::CX, self.regs.read16(Reg16::CX).wrapping_sub(1));
                if self.regs.read16(Reg16::CX) != 0 && self.regs.getflag(Flags::Zero) == 1 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
            }
            0xe2 => {
                let offset = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!(
                    "loop {:x}",
                    (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16)
                );
                self.regs
                    .write16(Reg16::CX, self.regs.read16(Reg16::CX).wrapping_sub(1));
                if self.regs.read16(Reg16::CX) != 0 {
                    self.regs.rip =
                        (self.regs.rip as u16).wrapping_add(offset as i8 as i16 as u16) as u64;
                }
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
            0xe8 => {
                let offset = self.mem_read16(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(2);
                self.push_16(bus, self.regs.rip as u16);
                println!(
                    "call near {:x}",
                    (self.regs.rip as u16).wrapping_add(offset)
                );
                self.regs.rip = (self.regs.rip as u16).wrapping_add(offset) as u64;
            }
            0xe9 => {
                self.jump_near_16(bus);
            }
            0xea => {
                self.jump_far_16(bus);
            }
            0xee => {
                let port = self.regs.read16(Reg16::DX);
                println!("out dx, al");
                self.io_write8(bus, port, self.regs.read8(Reg8::AL));
            }
            0xef => {
                let port = self.regs.read16(Reg16::DX);
                println!("out dx, ax");
                self.io_write16(bus, port, self.regs.read16(Reg16::AX));
            }
            0xf2 => {
                self.rep_status = Some(RepStatus::Repne);
                println!("REPNE:");
                self.tick(bus);
            }
            0xf3 => {
                self.rep_status = Some(RepStatus::Repe);
                println!("REPE:");
                self.tick(bus);
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
            0xfb => {
                println!("sti");
                self.segment_override = None;
                self.use_32op_prefix = false;
                self.use_32addr_prefix = false;
                self.rep_status = None;
                self.tick(bus);
                self.regs.setflag(Flags::Interrupt, 1);
            }
            0xfc => {
                println!("cld");
                self.regs.setflag(Flags::Direction, 0);
            }
            0xfd => {
                println!("std");
                self.regs.setflag(Flags::Direction, 1);
            }
            0xff => {
                let modrm = self.mem_read8(
                    bus,
                    self.regs.segs[SegReg::CS as usize].base + self.regs.rip,
                );
                self.regs.rip = self.regs.rip.wrapping_add(1);
                println!("opcodegrpff: {:x}", modrm);
                match modrm & 0x38 {
                    0x00 => {
                        let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "inc {}",
                                Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                            );
                            let result = self.regs.read16(rm.into()).wrapping_add(1);
                            self.setznp16(result);
                        } else if let Operand::Address(segment, ea) = opcode_params.rm {
                            println!(
                                "inc {}:{}",
                                Into::<&'static str>::into(segment),
                                format_offset_for_disasm(
                                    Cpu::get_addr_type_from_modrm16(modrm),
                                    Cpu::get_disp_type_from_modrm(modrm),
                                    self.modrm_disp
                                )
                            );
                            let rm =
                                self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                            let result = rm.wrapping_add(1);
                            self.setznp16(result);
                        }
                    }
                    0x08 => {
                        let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "dec {}",
                                Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                            );
                            let result = self.regs.read16(rm.into()).wrapping_sub(1);
                            self.setznp16(result);
                        } else if let Operand::Address(segment, ea) = opcode_params.rm {
                            println!(
                                "dec {}:{}",
                                Into::<&'static str>::into(segment),
                                format_offset_for_disasm(
                                    Cpu::get_addr_type_from_modrm16(modrm),
                                    Cpu::get_disp_type_from_modrm(modrm),
                                    self.modrm_disp
                                )
                            );
                            let rm =
                                self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                            let result = rm.wrapping_sub(1);
                            self.setznp16(result);
                        }
                    }
                    0x20 => {
                        let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "jmp {}",
                                Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                            );
                            self.regs.rip = self.regs.read16(rm.into()) as u64;
                        } else if let Operand::Address(segment, ea) = opcode_params.rm {
                            println!(
                                "jmp {}:{}",
                                Into::<&'static str>::into(segment),
                                format_offset_for_disasm(
                                    Cpu::get_addr_type_from_modrm16(modrm),
                                    Cpu::get_disp_type_from_modrm(modrm),
                                    self.modrm_disp
                                )
                            );
                            let rm =
                                self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                            self.regs.rip = rm as u64;
                        }
                    }
                    0x30 => {
                        let opcode_params = self.get_opcode_params_from_modrm(bus, modrm);
                        if let Operand::Register(rm) = opcode_params.rm {
                            println!(
                                "push {}",
                                Into::<&'static str>::into(<u8 as Into<Reg16>>::into(rm)),
                            );
                            self.push_16(bus, self.regs.read16(rm.into()));
                        } else if let Operand::Address(segment, ea) = opcode_params.rm {
                            println!(
                                "push {}:{}",
                                Into::<&'static str>::into(segment),
                                format_offset_for_disasm(
                                    Cpu::get_addr_type_from_modrm16(modrm),
                                    Cpu::get_disp_type_from_modrm(modrm),
                                    self.modrm_disp
                                )
                            );
                            let rm =
                                self.mem_read16(bus, self.regs.segs[segment as usize].base + ea);
                            self.push_16(bus, rm);
                            
                        }
                    }
                    _ => todo!(),
                }
            }
            _ => {
                todo!();
            }
        }
        self.segment_override = None;
        self.use_32op_prefix = false;
        self.use_32addr_prefix = false;
        self.rep_status = None;
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
