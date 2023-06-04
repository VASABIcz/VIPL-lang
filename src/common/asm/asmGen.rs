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
use offset::offset_of;
use std::collections::HashMap;
use std::fmt::Pointer;
use std::mem::size_of;
use crate::vm::nativeObjects::UntypedObject;

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

        let t = text.replace("\"", "");
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
        let r = self.acquireTemp();
        self.gen.mov(r.into(), Rsp.into());
        self.releaseRegister(r);

        self.safeCall(
            AsmLocation::Indexing(
                R15.into(),
                offset_of!(NativeWrapper::lCall).as_u32() as isize,
            ),
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

        self.safeCall(
            AsmLocation::Indexing(
                R15.into(),
                offset_of!(NativeWrapper::printDigit).as_u32() as isize,
            ),
            &[asmValue],
            None
        );

        self.gen.endIgnore();
    }

    pub fn vmOffset(&self, offset: u32) -> AsmLocation {
        AsmLocation::Indexing(R15.into(), offset as isize)
    }

    pub fn saveCoreRegs(&mut self) {
        self.gen.push(VM_REG.into());
        self.gen.push(STACK_REG.into());
        self.gen.push(LOCALS_REG.into());
    }

    pub fn restoreCoreRegs(&mut self) {
        self.gen.pop(LOCALS_REG);
        self.gen.pop(STACK_REG);
        self.gen.pop(VM_REG);
    }

    fn pushStr(&mut self, s: usize) {
        self.gen.comment(&format!("pushStr {}", s));

        let reg = self.acquireAny();

        self.safeCall(self.vmOffset(offset_of!(NativeWrapper::stringCached).as_u32()), &[R15.into(), R14.into(), s.into()], Some(reg));
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
        opCodes: Vec<OpCode>,
        vm: &VirtualMachine,
        namespace: &Namespace,
        returns: bool,
    ) {
        let mut jmpCounter = 0usize;

        let mut makeLabelsGreatAgain = HashMap::new();
        let mut jmpLookup = HashMap::new();
        let mut lastRet = -1isize;

        self.initCode();

        for (i, op) in opCodes.iter().enumerate() {
            if let OpCode::Jmp { offset, jmpType } = op {
                let o = *offset + 1;
                let xd = (i as i32 + o) as usize;
                let label = match makeLabelsGreatAgain.get(&xd) {
                    None => {
                        let a = format!("JMP{}", jmpCounter);
                        makeLabelsGreatAgain.insert(xd, a.clone());
                        jmpCounter += 1;
                        a
                    }
                    Some(v) => v.clone(),
                };
                jmpLookup.insert(i, label);
            }
        }

        for (i, op) in opCodes.iter().enumerate() {
            println!("genning {:?}", op);
            self.debugPrint(&format!("executing {:?}\n", op));
            self.gen.comment(&format!("start opcode {:?}", op));

            if let Some(v) = makeLabelsGreatAgain.get(&i) {
                self.gen.makeLabel(v)
            }

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
                OpCode::PushFloat(_) => todo!(),
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
                    let l = jmpLookup.get(&i).unwrap().clone();
                    match jmpType {
                        JmpType::True => {
                            let r = self.pop();
                            self.gen.compare(r.into(), 1.into());
                            self.gen.jmpIfOne(l.into())
                        }
                        JmpType::False => {
                            let r = self.pop();
                            self.gen.compare(r.into(), 0.into());
                            self.gen.jmpIfZero(l.into())
                        }
                        JmpType::Jmp => self.gen.jmp(l.into())
                    }
                }
                OpCode::Return => {
                    if i - 1 == lastRet as usize {
                        lastRet = i as isize;
                        continue;
                    }
                    lastRet = i as isize;
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

                    self.acquireRegister(Rax);

                    self.gen.xor(Rax.into(), Rax.into());
                    self.gen.compare(r1.into(), r2.into());
                    self.gen.sete(r1);

                    self.releaseRegister(Rax);
                }
                OpCode::Greater(t) => match t {
                    RawDataType::Int | RawDataType::Bool | RawDataType::Char => {
                        let r2 = self.pop();
                        let r1 = self.getTop();

                        self.acquireRegister(Rax);

                        self.gen.xor(Rax.into(), Rax.into());
                        self.gen.compare(r1.into(), r2.into());
                        self.gen.setl(r1);

                        self.releaseRegister(Rax);
                    }
                    _ => todo!(),
                },
                OpCode::Less(t) => match t {
                    RawDataType::Int | RawDataType::Bool | RawDataType::Char => {
                        let r2 = self.pop();
                        let r1 = self.getTop();

                        self.acquireRegister(Rax);

                        self.gen.xor(Rax.into(), Rax.into());
                        self.gen.compare(r1.into(), r2.into());
                        self.gen.setg(r1);

                        self.releaseRegister(Rax);
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

                    self.acquireRegister(Rax);

                    self.gen.xor(Rax.into(), Rax.into());
                    self.gen.compare(r1.into(), r2.into());
                    self.gen.sete(r1);

                    self.releaseRegister(Rax);
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

                    self.safeCall(self.vmOffset(offset_of!(NativeWrapper::arrayNew).as_u32()), &[VM_REG.into(), STACK_REG.into(), r1.into()], Some(ret));
                }
                OpCode::New { namespaceID, structID } => {
                    let ret = self.acquireAny();

                    self.safeCall(self.vmOffset(offset_of!(NativeWrapper::allocateObject).as_u32()),
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

                    self.safeCall(
                        self.vmOffset(
                            offset_of!(NativeWrapper::arrSetValue).as_u32()
                        ), &[VM_REG.into(), arr.into(), index.into(), value.into()], None);
                }
                OpCode::ArrayLoad => {
                    let index = self.pop();
                    let arr = self.pop();

                    let ret = self.acquireAny();

                    self.safeCall(
                        self.vmOffset(
                            offset_of!(NativeWrapper::arrGetValue).as_u32()
                        ), &[VM_REG.into(), arr.into(), index.into()], Some(ret));
                }
                OpCode::Inc { typ, index } => {
                    let r = self.acquireTemp();

                    self.gen.mov(r.into(), AsmValue::Indexing(Rbx.into(), *index as isize));

                    self.gen.inc(r);

                    self.gen.mov(AsmLocation::Indexing(Rbx.into(), *index as isize), r.into());

                    self.releaseRegister(r);
                }
                OpCode::ArrayLength => {
                    let obj = self.pop();
                    let res = self.acquireAny();

                    self.safeCall(self.vmOffset(offset_of!(NativeWrapper::arrayLen).as_u32()), &[obj.into()], Some(res));
                }
                OpCode::StringLength => {
                    let obj = self.pop();
                    let res = self.acquireAny();

                    self.safeCall(self.vmOffset(offset_of!(NativeWrapper::strLen).as_u32()), &[obj.into()], Some(res))
                }
                OpCode::DynamicCall => {
                    todo!()
                }
                e => todo!("{:?}", e),
            }
            self.gen.comment(&format!("end opcode {:?}\n", op));
        }
    }
}
