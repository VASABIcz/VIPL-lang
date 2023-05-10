use crate::asm::asmLib::Register;

pub struct RegisterRecord {
    pub reg: Register,
    pub aquires: usize
}

impl RegisterRecord {
    pub fn new(reg: Register) -> Self {
        Self {
            reg,
            aquires: 0,
        }
    }
}

pub struct RegisterManager {
    pub registers: Vec<RegisterRecord>
}

impl RegisterManager {
    pub fn new() -> Self {
        let mut regs = vec![
            Register::R13,
            Register::R12,
            Register::R11,
            Register::R10,
            Register::R9,
            Register::R8,
            Register::Rcx,
            Register::Rdx,
            Register::Rsi,
            Register::Rdi,
            Register::Rax
        ];

        regs.reverse();

        Self {
            registers: regs.into_iter().map(|it| {
                RegisterRecord::new(it)
            }).collect::<Vec<_>>(),
        }
    }
}

impl RegisterManager {
    // acquires gona use

    pub fn aquireSpecific(&mut self, reg: Register) -> bool {
        let res = self.registers.iter().find(|it| {
            it.reg == reg
        }).unwrap();

        res.aquires != 0
    }

    pub fn release(&mut self, reg: Register) -> bool {
        let res = self.registers.iter_mut().find(|it| {
            it.reg == reg
        }).unwrap();

        res.aquires-=1;

        res.aquires > 0
    }

    pub fn aquireAny(&mut self) -> (Register, bool) {
        let mut counter = 0usize;

        loop {
            let a = self.registers.get(counter).unwrap();
            let b = match self.registers.get(counter+1) {
                None => {
                    return (a.reg.clone(), a.aquires > 0)
                }
                Some(v) => v,
            };

            if b.aquires <= a.aquires {
                counter += 1;
                continue
            }
            return (a.reg.clone(), a.aquires > 0)
        }
    }
}