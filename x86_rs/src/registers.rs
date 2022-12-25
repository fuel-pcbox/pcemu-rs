#[repr(u8)]
enum Reg8 {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH
}

#[repr(u8)]
enum Reg8REX {
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
    R15B
}

#[repr(u8)]
enum Reg16 {
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI
}

#[repr(u8)]
enum Reg32 {
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
enum Reg64 {
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
    R15
}

#[repr(u8)]
enum SegReg {
    ES,
    CS,
    SS,
    DS,
    FS,
    GS
}

pub union FprMmxReg {
    pub uq: u64,
    pub sq: i64,
    pub ul: [u32; 2],
    pub sl: [i32; 2],
    pub uw: [u16; 4],
    pub sw: [i16; 4],
    pub ub: [u8; 8],
    pub sb: [i8; 8],
    pub f: [f32; 2]
}

pub struct FprReg {
    pub exp: u16,
    pub mantissa: FprMmxReg
}

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

pub struct YmmReg {
    pub xmm: [SseReg; 2]
}

pub struct ZmmReg {
    pub ymm: [YmmReg; 2]
}

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
}

impl Registers {
    pub fn new() -> Registers {
        unsafe {
            let resetxmm = SseReg {uq: {0,0}};
            Registers {
                gpr: [0; 16],
                fpr: [FprReg {
                    exp: 0,
                    mantissa: FprMmxReg {
                        uq: 0,
                    }
                }; 8]
                zmm: [ZmmReg {
                    ymm: [YmmReg {
                        xmm: [resetxmm; 2]
                    }]
                }],
                cr0: 0,
                cr2: 0,
                cr3: 0,
                cr4: 0,
                cr8: 0,
                dr0123: [0; 4],
                dr6: 0,
                dr7: 0,
            }
        }
    }
}