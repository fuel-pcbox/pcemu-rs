use std::fs;
use x86_rs::*;

struct TestBus {
    pub lowram: Vec<u8>,
    pub biosrom: Vec<u8>,
}

impl CpuBus for TestBus {
    fn mem_read8(&mut self, addr: u64) -> u8 {
        if addr < 0xa0000 {
            self.lowram[addr as usize]
        } else if (0xe0000..=0xfffff).contains(&addr) | (0xfffe0000..=0xffffffff).contains(&addr) {
            self.biosrom[(addr & 0x1ffff) as usize]
        } else {
            0xff
        }
    }

    fn mem_write8(&mut self, addr: u64, data: u8) {
        if addr < 0xa0000 {
            self.lowram[addr as usize] = data;
        }
    }

    fn mem_read16(&mut self, addr: u64) -> u16 {
        self.mem_read8(addr) as u16 | ((self.mem_read8(addr + 1) as u16) << 8)
    }

    fn mem_write16(&mut self, addr: u64, data: u16) {
        self.mem_write8(addr, data as u8);
        self.mem_write8(addr + 1, (data >> 8) as u8);
    }

    fn mem_read32(&mut self, addr: u64) -> u32 {
        self.mem_read16(addr) as u32 | ((self.mem_read16(addr + 2) as u32) << 16)
    }

    fn mem_write32(&mut self, addr: u64, data: u32) {
        self.mem_write16(addr, data as u16);
        self.mem_write16(addr + 2, (data >> 16) as u16);
    }

    fn mem_read64(&mut self, addr: u64) -> u64 {
        self.mem_read32(addr) as u64 | ((self.mem_read32(addr + 4) as u64) << 32)
    }

    fn mem_write64(&mut self, addr: u64, data: u64) {
        self.mem_write32(addr, data as u32);
        self.mem_write32(addr + 4, (data >> 32) as u32);
    }

    fn io_read8(&mut self, addr: u16) -> u8 {
        //todo!();
        0xff
    }

    fn io_write8(&mut self, addr: u16, data: u8) {
        //todo!();
    }

    fn io_read16(&mut self, addr: u16) -> u16 {
        self.io_read8(addr) as u16 | ((self.io_read8(addr + 1) as u16) << 8)
    }

    fn io_write16(&mut self, addr: u16, data: u16) {
        self.io_write8(addr, data as u8);
        self.io_write8(addr + 1, (data >> 8) as u8);
    }

    fn io_read32(&mut self, addr: u16) -> u32 {
        self.io_read16(addr) as u32 | ((self.io_read16(addr + 2) as u32) << 16)
    }

    fn io_write32(&mut self, addr: u16, data: u32) {
        self.io_write16(addr, data as u16);
        self.io_write16(addr + 2, (data >> 16) as u16);
    }
}

impl TestBus {
    pub fn new() -> TestBus {
        let bios = fs::read("BIOS-bochs-latest").unwrap();
        TestBus {
            lowram: vec![0; 0xa0000],
            biosrom: bios,
        }
    }
}

fn main() {
    let mut cpu = Cpu::new();
    let mut test_bus = TestBus::new();

    for _i in 0.. {
        cpu.tick(&mut test_bus);
    }
}
