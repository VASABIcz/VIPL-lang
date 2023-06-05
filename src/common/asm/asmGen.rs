use crate::asm::asmLib::Register::{
    Rax, Rbx, Rcx, Rdi, Rdx, Rsi, Rsp, R10, R11, R12, R13, R14, R15,
};
use crate::asm::asmLib::{AsmGen, AsmLocation, AsmValue, Concrete, Register};
use crate::asm::registerManager::RegisterManager;
use crate::ffi::NativeWrapper;
use crate::viplDbg;
use crate::vm::dataType::{DataType, RawDataType};
use crate::vm::namespace::Namespace;
use crate::vm::vm::{JmpType, OpCode, VirtualMachine};
use libc::THOUSEP;
use std::collections::HashMap;
use std::fmt::Pointer;
use std::mem::size_of;
use offset::{Offset, offset_of};
use crate::bytecodeGen::SymbolicOpcode;
use crate::vm::nativeObjects::UntypedObject;
use crate::vm::value::Value;

const DEBUG: bool = false;
const VM_REG: Register = Register::R15;
const STACK_REG: Register = Register::R14;
const LOCALS_REG: Register = Register::Rbx;

/*
rdi, rsi, rdx, rcx, r8, r9

rax - FFI return value

rbx - VIPL locals ptr

rcx - FFI fourth argument
rdx - FFI third argument
rsi - FFI second argument
rdi - FFI first argument

rbp - STACK base
rsp - STACK ptr

r8  - FFI fith argument
r9  - FFI sixth argument

r10 - general register
r11 - general register

r12 - VIPL opcode arg1
r13 - VIPL opcode arg2

r14 - VIPL stack ptr
r15 - VIPL vm ptr
 */

static ARG_REGS: [Register; 6] = [Rdi, Rsi, Rdx, Rcx, Register::R8, Register::R8];

pub struct Jitter<T: AsmGen> {
    pub gen: T,
    pub regs: RegisterManager,
    pub stack: Vec<Register>,
}

impl<T: AsmGen> Jitter<T> {
    pub fn new(gen: T) -> Self {
        Self {
            gen,
            regs: RegisterManager::new(),
            stack: vec![],
        }
    }
}

impl<T: AsmGen> Jitter<T> {
    fn debugPrint(&mut self, text: &str) {
        if !DEBUG {
            return;
        }
        self.gen.beginIgnore();

        let t = text.replace('\"', "");
        self.gen.comment(&format!("debugPrint {}", text));

        self.acquireRegister(Rax);
        self.acquireRegister(Rdi);
        self.acquireRegister(Rsi);
        self.acquireRegister(Rdx);

        self.gen.mov(Rax.into(), 1.into());
        self.gen.mov(Rdi.into(), 1.into());
        let lejbl = self.gen.makeString(&t);
        self.gen.lea(Rsi.into(), Concrete::Lejbl(lejbl).into());
        self.gen.mov(Rdx.into(), (t.len()).into());
        self.gen.sysCall();

        self.releaseRegister(Rdx);
        self.releaseRegister(Rsi);
        self.releaseRegister(Rdi);
        self.releaseRegister(Rax);

        self.gen.endIgnore();
    }

    fn withTempReg<F: Fn(&mut Jitter<T>,Register)>(&mut self, f: F) -> Register {
        let r = self.acquireTemp();
        f(self, r);
        self.releaseRegister(r);

        r
    }

    fn acquireScoped<F: Fn(&mut Jitter<T>)>(&mut self, reg: Register, f: F) {
        self.acquireRegister(reg);
        f(self);
        self.releaseRegister(reg);
    }

    fn saveAquiredRegisters(&mut self, except: &[Register]) {
        self.gen.comment("saveAquiredRegisters");
        let rs = self.regs.usedRegisters();

        for r in rs {
            if except.contains(&r) {
                continue;
            }
            self.gen.push(r.into());
        }
    }

    fn restoreAquiredRegisters(&mut self, except: &[Register]) {
        self.gen.comment("restoreAquiredRegisters");
        let mut rs = self.regs.usedRegisters();

        rs.reverse();

        for r in rs {
            if except.contains(&r) {
                continue;
            }
            self.gen.pop(r);
        }
    }

    fn callFfi<Base, Field>(&mut self, f: Offset<Base, Field>, args: &[AsmValue], ret: Option<Register>) {
        self.safeCall(AsmLocation::Indexing(VM_REG.into(), f.as_u32() as isize), args, ret)
    }

    fn safeCall(&mut self, f: AsmLocation, args: &[AsmValue], ret: Option<Register>) {
        let argsCount = args.len();
        let mut excluded = vec![];
        excluded.extend(&ARG_REGS[..argsCount]);
        excluded.extend(ret);

        if argsCount > 6 {
            panic!("more than 6 args is not supported yet")
        }

        self.gen.comment("occupies args registers");
        for (i, arg) in args.iter().enumerate() {
            let r = ARG_REGS[i];

            self.acquireRegister(r);

            self.gen.mov(r.into(), arg.clone());
        }

        self.saveAquiredRegisters(&excluded);
        self.saveCoreRegs();

        let offset = self.gen.getStackOffset();

        println!("current offset is {}", offset);

        let needsAlignment = (offset) % 2 == 0;

        if needsAlignment {
            self.allocateStack(1);
        }

        self.gen.comment("perform call");
        self.gen.call(f);

        if let Some(r) = ret {
            self.gen.mov(r.into(), Rax.into())
        }

        if needsAlignment {
            self.freeStack(1);
        }

        self.restoreCoreRegs();
        self.restoreAquiredRegisters(&excluded);

        self.gen.comment("restore args registers");
        for i in 0..argsCount {
            self.releaseRegister(ARG_REGS[(argsCount - 1) - i])
        }
    }

    fn genCall(
        &mut self,
        namespaceID: usize,
        functionID: usize,
        returns: bool,
        argsCount: usize,
        localsCount: usize,
    ) {
        self.gen.comment(&format!(
            "call {}:{} -> {}",
            namespaceID, functionID, returns
        ));

        self.gen.comment("push args to stack");

        let mut locals = vec![];

        for _ in 0..argsCount {
            let r = self.pop();
            locals.push(r);
        }

        locals.reverse();

        for r in locals {
            self.gen.push(r.into());
        }

        let retReg = if returns {
            Some(self.acquireAny())
        }
        else {
            None
        };

        if argsCount < localsCount {
            self.allocateStack(localsCount - argsCount);
        }

        self.gen.comment("save pointer to stack args");
        // FIXME will cause bugs when there is no free registers and needs to reuse one by saving its content onto a stack

        let r = self.withTempReg(|s, r| {
            s.gen.mov(r.into(), Rsp.into());
        });


        self.callFfi(
            offset_of!(NativeWrapper::lCall),
            &[R15.into(), functionID.into(), namespaceID.into(), r.into()],
            retReg
        );

        self.freeStack(localsCount);
    }

    fn allocateStack(&mut self, values: usize) {
        self.gen.offsetStack(-(values as isize))
    }

    fn freeStack(&mut self, values: usize) {
        self.gen.offsetStack(values as isize)
    }

    fn debugCrash(&mut self, asmValue: AsmValue) {
        if !DEBUG {
            return;
        }

        self.gen
            .comment(&format!("debugCrash {}", asmValue.clone().toString()));
        self.gen.mov(Rax.into(), 60.into());
        self.gen.mov(Rdi.into(), asmValue.into());
        self.gen.sysCall();
    }

    fn printDigit(&mut self, asmValue: AsmValue) {
        if !DEBUG {
            return;
        }
        self.gen.beginIgnore();

        self.gen
            .comment(&format!("printDigit {}", asmValue.clone().toString()));

        println!(
            "offset {}",
            offset_of!(NativeWrapper::printDigit).as_u32() as isize
        );

        self.callFfi(offset_of!(NativeWrapper::printDigit),
            &[asmValue],
            None
        );

        self.gen.endIgnore();
    }

    pub fn vmOffset(&self, offset: u32) -> AsmLocation {
        AsmLocation::Indexing(R15.into(), offset as isize)
    }

    pub fn saveCoreRegs(&mut self) {
        // self.gen.push(VM_REG.into());
        // self.gen.push(STACK_REG.into());
        // self.gen.push(LOCALS_REG.into());
    }

    pub fn restoreCoreRegs(&mut self) {
        // self.gen.pop(LOCALS_REG);
        // self.gen.pop(STACK_REG);
        // self.gen.pop(VM_REG);
    }

    fn pushStr(&mut self, s: usize) {
        self.gen.comment(&format!("pushStr {}", s));

        let reg = self.acquireAny();

        self.callFfi(offset_of!(NativeWrapper::stringCached), &[R15.into(), R14.into(), s.into()], Some(reg));
    }

    fn acquireRegister(&mut self, reg: Register) {
        if self.regs.aquireSpecific(reg) {
            self.gen.push(reg.into())
        }
        if DEBUG {
            println!("acquiring {:?}", reg);
        }
    }

    fn acquireAny(&mut self) -> Register {
        let aq = self.regs.aquireAny();

        if aq.1 {
            self.gen.push(aq.0.into());
        }

        if DEBUG {
            println!("acquiring {:?}", aq.0);
        }
        self.stack.push(aq.0);
        aq.0
    }

    fn acquireTemp(&mut self) -> Register {
        let aq = self.regs.aquireAny();

        if aq.1 {
            self.gen.push(aq.0.into());
        }

        if DEBUG {
            println!("acquiring {:?}", aq.0);
        }
        aq.0
    }

    fn releaseRegister(&mut self, reg: Register) {
        if DEBUG {
            println!("releasing {:?}", reg);
        }
        if self.regs.release(reg) {
            self.gen.pop(reg)
        }
    }

    fn initCode(&mut self) {
        self.gen.comment("init code");
        self.gen.mov(R15.into(), Rdi.into()); // vm ptr
        self.gen.mov(R14.into(), Rsi.into()); // frame ptr
        self.gen.mov(Rbx.into(), AsmValue::Indexing(R14.into(), 0)); // locals ptr

        // printDigit(Rsp.into());

        self.gen.newLine();
    }

    fn pop(&mut self) -> Register {
        let a = self.stack.pop().unwrap();
        self.releaseRegister(a);

        a
    }

    fn getTop(&mut self) -> Register {
        self.stack.last().unwrap().clone()
    }

    pub fn generateAssembly(
        &mut self,
        opCodes: &[SymbolicOpcode],
        vm: &VirtualMachine,
        namespace: &Namespace,
        returns: bool,
    ) {
        let mut lastRet = -1isize;

        self.initCode();

        for (i, oo) in opCodes.iter().enumerate() {
            self.generateInstruction(&vm, namespace, returns, &mut lastRet, i, oo);
        }
    }

    pub fn generateInstruction(&mut self, vm: &VirtualMachine, namespace: &Namespace, returns: bool, lastRet: &mut isize, i: usize, oo: &SymbolicOpcode) -> bool {
        match oo {
            SymbolicOpcode::Op(op) => {
                println!("genning {:?}", op);
                self.debugPrint(&format!("executing {:?}\n", op));
                self.gen.comment(&format!("=== start opcode {:?}", op));

                match op {
                    OpCode::PushInt(v) => {
                        let r = self.acquireAny();
                        self.gen.mov(r.into(), (*v).into());
                    }
                    OpCode::PushIntOne => {
                        let r = self.acquireAny();
                        self.gen.mov(r.into(), 1.into());
                    }
                    OpCode::PushIntZero => {
                        let r = self.acquireAny();
                        self.gen.mov(r.into(), 0.into());
                    }
                    OpCode::PushFloat(f) => todo!(),
                    OpCode::PushBool(b) => {
                        let r = self.acquireAny();
                        self.gen.mov(r.into(), ((*b) as usize).into());
                    }
                    OpCode::PushChar(c) => {
                        let r = self.acquireAny();
                        self.gen.mov(r.into(), ((*c) as usize).into());
                    }
                    OpCode::Pop => {
                        self.pop();
                    }
                    OpCode::Dup => {
                        let r = self.getTop();
                        let r1 = self.acquireAny();
                        self.gen.mov(r1.into(), r.into());
                    }
                    OpCode::Swap => {
                        let r = self.stack.pop().unwrap();
                        let r1 = self.stack.pop().unwrap();
                        self.stack.push(r1);
                        self.stack.push(r);
                    }
                    OpCode::GetLocal { index } => {
                        let r = self.acquireAny();
                        self.gen
                            .mov(r.into(), AsmValue::Indexing(Rbx.into(), *index as isize));
                    }
                    OpCode::SetLocal { index } => {
                        let r = self.pop();
                        self.gen
                            .mov(AsmLocation::Indexing(Rbx.into(), *index as isize), r.into());
                    }
                    OpCode::Jmp { offset, jmpType } => {
                        unreachable!()
                    }
                    OpCode::Return => {
                        if i - 1 == *lastRet as usize {
                            *lastRet = i as isize;
                            return true;
                        }
                        *lastRet = i as isize;
                        if returns {
                            match self.stack.pop() {
                                None => {}
                                Some(r) => {
                                    self.gen.mov(Rax.into(), r.into());
                                }
                            }
                        }
                        self.printDigit(Rax.into());
                        self.gen.ret()
                    }
                    OpCode::Add(t) => match t {
                        RawDataType::Int => {
                            let r2 = self.pop();
                            let r1 = self.getTop();
                            self.gen.add(r1.into(), r2.into());
                        }
                        _ => todo!(),
                    },
                    OpCode::Sub(t) => match t {
                        RawDataType::Int => {
                            let r2 = self.pop();
                            let r1 = self.getTop();
                            self.gen.sub(r1.into(), r2.into());
                        }
                        _ => todo!(),
                    },
                    OpCode::Div(_) => todo!(),
                    OpCode::Mul(t) => match t {
                        RawDataType::Int => {
                            let r2 = self.pop();
                            let r1 = self.getTop();
                            self.gen.imul(r1.into(), r2.into());
                        }
                        _ => todo!(),
                    },
                    OpCode::Equals(t) => {
                        let r2 = self.pop();
                        let r1 = self.getTop();

                        self.acquireScoped(Rax, |s| {
                            s.gen.xor(Rax.into(), Rax.into());
                            s.gen.compare(r1.into(), r2.into());
                            s.gen.sete(r1);
                        });
                    }
                    OpCode::Greater(t) => match t {
                        RawDataType::Int | RawDataType::Bool | RawDataType::Char => {
                            let r2 = self.pop();
                            let r1 = self.getTop();

                            self.acquireScoped(Rax, |s| {
                                s.gen.xor(Rax.into(), Rax.into());
                                s.gen.compare(r1.into(), r2.into());
                                s.gen.setl(r1);
                            });
                        }
                        _ => todo!(),
                    },
                    OpCode::Less(t) => match t {
                        RawDataType::Int | RawDataType::Bool | RawDataType::Char => {
                            let r2 = self.pop();
                            let r1 = self.getTop();

                            self.acquireScoped(Rax, |s| {
                                s.gen.xor(Rax.into(), Rax.into());
                                s.gen.compare(r1.into(), r2.into());
                                s.gen.setg(r1);
                            });
                        }
                        _ => todo!(),
                    },
                    OpCode::Or => {
                        let r2 = self.pop();
                        let r1 = self.getTop();

                        self.gen.or(r1.into(), r2.into());
                    }
                    OpCode::And => {
                        let r2 = self.pop();
                        let r1 = self.getTop();

                        self.gen.and(r1.into(), r2.into());
                    }
                    OpCode::Not => {
                        let r1 = self.getTop();
                        self.gen.not(r1.into());
                    }
                    OpCode::StrNew(v) => {
                        self.pushStr(*v)
                    }
                    OpCode::SCall { id } => {
                        let f = namespace.getFunctionMeta(*id).unwrap();
                        let argsCount = f.argsCount;
                        let returns = f.returns();

                        self.genCall(namespace.id, *id, returns, argsCount, f.localsMeta.len())
                    }
                    OpCode::LCall { namespace, id } => {
                        let f = &vm.getNamespace(*namespace as usize).getFunction(*id as usize).0;
                        let returns = f.returns();
                        let argsCount = f.argsCount;

                        self.genCall(*namespace as usize, *id as usize, returns, argsCount, f.localsMeta.len())
                    }
                    OpCode::GetLocalZero => {
                        let r = self.acquireAny();
                        self.gen.mov(r.into(), AsmValue::Indexing(Rbx.into(), 0));
                    }
                    OpCode::SetLocalZero => {
                        let r = self.pop();
                        self.gen.mov(AsmLocation::Indexing(Rbx.into(), 0), r.into());
                    }
                    OpCode::LessInt => {
                        let r2 = self.pop();
                        let r1 = self.getTop();

                        self.acquireScoped(Rax, |s| {
                            s.gen.xor(Rax.into(), Rax.into());
                            s.gen.compare(r1.into(), r2.into());
                            s.gen.sete(r1);
                        });
                    }
                    OpCode::SubInt => {
                        let r2 = self.pop();
                        let r1 = self.getTop();
                        self.gen.sub(r1.into(), r2.into());
                    }
                    OpCode::MulInt => {
                        let r2 = self.pop();
                        let r1 = self.getTop();
                        self.gen.imul(r1.into(), r2.into());
                    }
                    OpCode::AddInt => {
                        let r2 = self.pop();
                        let r1 = self.getTop();
                        self.gen.add(r1.into(), r2.into());
                    }
                    OpCode::ArrayNew => {
                        let r1 = self.pop();
                        let ret = self.acquireAny();

                        self.callFfi(offset_of!(NativeWrapper::arrayNew), &[VM_REG.into(), STACK_REG.into(), r1.into()], Some(ret));
                    }
                    OpCode::New { namespaceID, structID } => {
                        let ret = self.acquireAny();

                        self.callFfi(offset_of!(NativeWrapper::allocateObject),
                                      &[VM_REG.into(), STACK_REG.into(), (*namespaceID).into(), (*structID).into()], Some(ret));
                    }
                    OpCode::SetField { fieldID } => {
                        let value = self.pop();
                        let obj = self.pop();

                        self.gen.mov(AsmLocation::Indexing(obj.into(), (size_of::<UntypedObject>() + fieldID * 8) as isize), value.into());
                    }
                    OpCode::GetField { fieldID } => {
                        let obj = self.pop();
                        let r = self.acquireAny();

                        self.gen.mov(r.into(), AsmValue::Indexing(obj.into(), (size_of::<UntypedObject>() + fieldID * 8) as isize));
                    }
                    OpCode::ArrayStore => {
                        let index = self.pop();
                        let value = self.pop();
                        let arr = self.pop();

                        self.callFfi(
                            offset_of!(NativeWrapper::arrSetValue),
                            &[VM_REG.into(), arr.into(), index.into(), value.into()],
                            None);
                    }
                    OpCode::ArrayLoad => {
                        let index = self.pop();
                        let arr = self.pop();

                        let ret = self.acquireAny();

                        self.callFfi(offset_of!(NativeWrapper::arrGetValue), &[VM_REG.into(), arr.into(), index.into()], Some(ret));
                    }
                    OpCode::Inc { typ, index } => {
                        self.withTempReg(|s, r| {
                            s.gen.mov(r.into(), AsmValue::Indexing(Rbx.into(), *index as isize));

                            s.gen.inc(r);

                            s.gen.mov(AsmLocation::Indexing(Rbx.into(), *index as isize), r.into());
                        });
                    }
                    OpCode::ArrayLength => {
                        let obj = self.pop();
                        let res = self.acquireAny();

                        self.callFfi(offset_of!(NativeWrapper::arrayLen), &[obj.into()], Some(res));
                    }
                    OpCode::StringLength => {
                        let obj = self.pop();
                        let res = self.acquireAny();

                        self.callFfi(offset_of!(NativeWrapper::strLen), &[obj.into()], Some(res))
                    }
                    OpCode::DynamicCall(returns, argCount) => {
                        let lambda = self.pop();

                        for _ in 0..*argCount {
                            let r = self.pop().into();
                            self.gen.push(r);
                        }

                        let ret = if *returns {
                            Some(self.acquireAny())
                        } else {
                            None
                        };

                        let stackPtr = self.acquireTemp();
                        self.gen.mov(stackPtr.into(), Rsp.into());

                        self.callFfi(
                            offset_of!(NativeWrapper::dynamicCall),
                            &[VM_REG.into(), lambda.into(), stackPtr.into()],
                            ret
                        );
                    }
                    OpCode::PushFunction(nId, fId) => {
                        let v = Value::makeFunction(*nId, *fId).asUnsigned();

                        let r = self.acquireAny();
                        self.gen.mov(r.into(), v.into());
                    }
                    e => todo!("{:?}", e),
                }
                self.gen.comment(&format!("=== end opcode {:?}\n", op));
            }
            SymbolicOpcode::Jmp(label, jmpType) => {
                match jmpType {
                    JmpType::True => {
                        let r = self.pop();
                        self.gen.compare(r.into(), 1.into());
                        self.gen.jmpIfOne(format!("LABEL{}", label).into())
                    }
                    JmpType::False => {
                        let r = self.pop();
                        self.gen.compare(r.into(), 0.into());
                        self.gen.jmpIfZero(format!("LABEL{}", label).into())
                    }
                    JmpType::Jmp => self.gen.jmp(format!("LABEL{}", label).into())
                }
            }
            SymbolicOpcode::LoopLabel(l) => {
                self.gen.makeLabel(&format!("LABEL{}", l))
            }
        }
        false
    }
}
