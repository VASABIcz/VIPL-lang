use std::fmt::Debug;

use crate::asm::asmLib::{AsmGen, AsmLocation, AsmValue, Register};
use crate::asm::asmLib::Register::{Rcx, Rdi, Rdx, Rsi};
use crate::asm::nasmGen::NasmGen;
use crate::asm::registerManager::RegisterManager;
use crate::ast::BinaryOp;
use crate::vm::value::Value;

static ARG_REGS: [Register; 6] = [Rdi, Rsi, Rdx, Rcx, Register::R8, Register::R8];

pub trait JITR<REG: PartialEq + Clone + Copy + Debug> {
    fn getAnyReg(&mut self) -> REG;

    fn getTempReg(&mut self) -> REG;

    fn getSpecificReg(&mut self, reg: REG) -> REG;

    fn callCABIRet(&mut self, location: AsmLocation, args: &[AsmValue], ret: REG);

    fn callCABI(&mut self, location: AsmLocation, args: &[AsmValue]);

    fn performArithmetic(&mut self, op: BinaryOp);

    fn performArithmeticFloat(&mut self, op: BinaryOp);

    fn push(&mut self, value: usize);

    fn getStackRegister(&self) -> REG;
    fn getVMReg(&self) -> REG;
    fn getFrameReg(&self) -> REG;
    fn getLocalsReg(&self) -> REG;
}

pub struct X86JITR {
    nasm: NasmGen,
    regs: RegisterManager,
    regsStack: Vec<Register>
}


impl JITR<Register> for X86JITR {
    fn getAnyReg(&mut self) -> Register {
        let r = self.regs.aquireAny();

        if r.1 {
            self.nasm.push(r.0.into());
        }

        self.regsStack.push(r.0);
        r.0
    }

    fn getTempReg(&mut self) -> Register {
        let r = self.regs.aquireAny();

        if r.1 {
            self.nasm.push(r.0.into());
        }

        r.0
    }

    fn getSpecificReg(&mut self, reg: Register) {
        let r = self.regs.aquireSpecific(reg);

        if r {
            self.nasm.push(reg.into());
        }

        self.regsStack.push(reg);
    }

    fn callCABIRet(&mut self, location: AsmLocation, args: &[AsmValue], ret: Register) {
        todo!()
    }

    fn callCABI(&mut self, location: AsmLocation, args: &[AsmValue]) {
        for used in self.regs.usedRegisters() {
            self.nasm.push(used.into())
        }

        for (i, v) in args.iter().enumerate() {
            self.nasm.mov(ARG_REGS[i].into(), v.clone());
        }

        let needsAlignment = (self.nasm.getStackOffset()) % 2 == 0;

        if needsAlignment {
            self.nasm.offsetStack(-1);
        }

        self.nasm.call(location);

        if needsAlignment {
            self.nasm.offsetStack(1);
        }

        for (i, v) in args.iter().enumerate() {
            self.nasm.mov(ARG_REGS[i].into(), v.clone());
        }

        for used in self.regs.usedRegisters() {
            self.nasm.push(used.into())
        }
    }

    fn performArithmetic(&mut self, op: BinaryOp) {
        todo!()
    }

    fn performArithmeticFloat(&mut self, op: BinaryOp) {
        todo!()
    }

    fn push(&mut self, value: usize) {
        todo!()
    }

    fn getStackRegister(&self) -> Register {
        todo!()
    }

    fn getVMReg(&self) -> Register {
        todo!()
    }

    fn getFrameReg(&self) -> Register {
        todo!()
    }

    fn getLocalsReg(&self) -> Register {
        todo!()
    }
}

