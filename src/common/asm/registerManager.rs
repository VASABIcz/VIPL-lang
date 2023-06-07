use crate::asm::asmLib::Register;
use core::slice::GetManyMutError;

pub struct RegisterRecord {
    pub reg: Register,
    pub aquires: usize,
}

impl RegisterRecord {
    pub fn new(reg: Register) -> Self {
        Self { reg, aquires: 0 }
    }
}

#[derive(Default)]
pub struct RegisterManager {
    pub registers: Vec<RegisterRecord>,
    aquires: Vec<Register>,
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
            Register::Rax,
        ];

        regs.reverse();

        Self {
            registers: regs
                .into_iter()
                .map(|it| RegisterRecord::new(it))
                .collect::<Vec<_>>(),
            aquires: vec![],
        }
    }
}

impl RegisterManager {
    // acquires gona use

    pub fn aquireSpecific(&mut self, reg: Register) -> bool {
        let res = self.registers.iter_mut().find(|it| it.reg == reg).unwrap();

        res.aquires += 1;

        self.aquires.push(reg);

        res.aquires != 1
    }

    pub fn release(&mut self, reg: Register) -> bool {
        let res = self.registers.iter_mut().find(|it| it.reg == reg).unwrap();

        let r = self.aquires.pop().unwrap();

        if r != reg {
            panic!(
                "registers restored in invalid order {:?} vs {:?} | {:?}",
                r, reg, self.aquires
            )
        }

        res.aquires -= 1;

        res.aquires > 0
    }

    pub fn aquireAny(&mut self) -> (Register, bool) {
        let mut counter = 0usize;

        loop {
            let items = match self.registers.get_many_mut([counter, counter + 1]) {
                Ok(v) => v,
                Err(_) => {
                    let a = self.registers.get_mut(counter).unwrap();
                    self.aquires.push(a.reg);
                    a.aquires += 1;
                    return (a.reg, a.aquires > 1);
                }
            };

            if items[1].aquires <= items[0].aquires {
                counter += 1;
                continue;
            }
            self.aquires.push(items[0].reg);
            items[0].aquires += 1;

            return (items[0].reg, items[0].aquires > 1);
        }
    }

    pub fn usedRegisters(&self) -> Vec<Register> {
        let mut buf = vec![];

        for reg in &self.registers {
            if reg.aquires > 0 {
                buf.push(reg.reg)
            }
        }

        return buf;
    }
}
