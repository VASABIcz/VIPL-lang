use std::collections::HashMap;
use offset::offset_of;
use crate::asm::asmLib::{AsmGen, AsmValue, Concrete, AsmLocation, Register};
use crate::asm::asmLib::Register::{Bl, R10, R11, R12, R13, R14, R15, Rax, Rbx, Rcx, Rdi, Rdx, Rsi, Rsp};
use crate::asm::registerManager::RegisterManager;
use crate::ffi::NativeWrapper;
use crate::vm::dataType::DataType;
use crate::vm::namespace::Namespace;
use crate::vm::vm::{JmpType, OpCode, VirtualMachine};

const DEBUG: bool = true;

/*
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

fn getLocal<T: AsmGen>(this: &mut T, index: usize) {
    this.comment(&format!("getlocal {}", index));
    printDigit(this, Rbx.into());
    this.mov(R10.into(), AsmValue::Indexing(Rbx.into(), index as isize));
    this.push(R10.into());
}

fn saveRegisters<T: AsmGen>(this: &mut T) {
    this.beginIgnore();
    this.comment(&format!("saveRegisters"));
    this.push(Rax.into());
    this.push(Rdi.into());
    this.push(Rsi.into());
    this.push(Rdx.into());
    this.endIgnore();
}

fn restoreRegisters<T: AsmGen>(this: &mut T) {
    this.beginIgnore();
    this.comment(&format!("restoreRegisters"));
    this.pop(Rdx.into());
    this.pop(Rsi.into());
    this.pop(Rdi.into());
    this.pop(Rax.into());
    this.endIgnore();
}

fn debugPrint<T: AsmGen>(this: &mut T, text: &str) {
    if !DEBUG {
        return;
    }
    this.beginIgnore();

    let t = text.replace("\"", "");
    this.comment(&format!("debugPrint {}", text));
    saveRegisters(this);
    this.mov(Rax.into(), 1.into());
    this.mov(Rdi.into(), 1.into());
    let lejbl = this.makeString(&t);
    this.lea(Rsi.into(), Concrete::Lejbl(lejbl).into());
    this.mov(Rdx.into(), (t.len()).into());
    this.sysCall();
    restoreRegisters(this);
    this.endIgnore();
}

fn genCall<T: AsmGen>(this: &mut T, regs: &mut RegisterManager, s: &mut Vec<Register>, namespaceID: usize, functionID: usize, returns: bool, argsCount: usize) {
    this.comment(&format!("call {}:{} -> {}", namespaceID, functionID, returns));

    aquireRegister(this, regs, Rdi);
    aquireRegister(this, regs, Rsi);
    aquireRegister(this, regs, Rdx);
    aquireRegister(this, regs, Rcx);
    aquireRegister(this, regs, Rax);

    this.beginIgnore();

    for _ in 0..argsCount {
        this.push(s.pop().unwrap().into());
    }

    this.mov(Rdi.into(), R15.into());
    this.mov(Rsi.into(), functionID.into());
    this.mov(Rdx.into(), namespaceID.into());
    this.mov(Rcx.into(), Rsp.into());

    this.push(Rbx.into());

    this.call(AsmLocation::Indexing(R15.into(), offset_of!(NativeWrapper::lCall).as_u32() as isize));

    this.pop(Rbx.into());

    this.offsetStack(argsCount as isize); // consume args
    this.endIgnore();

    releaseRegister(this, regs, Rax);
    releaseRegister(this, regs, Rcx);
    releaseRegister(this, regs, Rdx);
    releaseRegister(this, regs, Rsi);
    releaseRegister(this, regs, Rdi);

    if returns {
        printDigit(this, Rax.into());
        let r = aquireAny(this, regs);
        this.mov(r.into(), Rax.into());
        s.push(r);
    }
}

fn debugCrash<T: AsmGen>(this: &mut T, asmValue: AsmValue) {
    if !DEBUG {
        return;
    }

    this.comment(&format!("debugCrash {}", asmValue.clone().toString()));
    this.mov(Rax.into(), 60.into()); // syscall code
    this.mov(Rdi.into(), asmValue.into()); // status code
    this.sysCall();
}

fn printDigit<T: AsmGen>(this: &mut T, asmValue: AsmValue) {
    if !DEBUG {
        return;
    }
    this.beginIgnore();

    this.comment(&format!("printDigit {}", asmValue.clone().toString()));

    println!("offset {}", offset_of!(NativeWrapper::printDigit).as_u32() as isize);


    this.mov(Rdi.into(),asmValue);
    saveRegisters(this);
    this.call(AsmLocation::Indexing(R15.into(), offset_of!(NativeWrapper::printDigit).as_u32() as isize));
    restoreRegisters(this);
    this.endIgnore();
}

fn debugProgram<T: AsmGen>(this: &mut T) {
    initCode(this);
    debugPrint(this, "debug program");

    pushStr(this, "UwU");
    this.pop(R11);

    debugPrint(this, "returning");
    this.ret();
}

fn popNoStore<T: AsmGen>(this: &mut T) {
    this.add(Rsp.into(), 8.into());
}

fn pushNoStore<T: AsmGen>(this: &mut T) {
    this.sub(Rsp.into(), 8.into());
}

fn pushStr<T: AsmGen>(this: &mut T, s: &str) {
    this.comment(&format!("pushStr {}", s));
    let label = this.makeString(s);
    this.mov(Rdi.into(), R15.into());
    this.mov(Rsi.into(), R14.into());
    this.lea(Rdx.into(), label.into());
    this.mov(R10.into(), AsmValue::Indexing(R15.into(), offset_of!(NativeWrapper::stringNew).as_u32() as isize));
    // alignStack(this);
    // pushNoStore(this);
    // debugPrint(this, "push str");
    this.call(R10.into());
    // popNoStore(this);
    this.push(Rax.into());
}

fn setLocal<T: AsmGen>(this: &mut T, index: usize) {
    this.comment(&format!("setLocal {}", index));

    this.pop(R10.into());

    this.mov(AsmLocation::Indexing(Rbx.into(), index as isize), R10.into());
}

fn popStack<T: AsmGen>(this: &mut T) {
    this.comment("popStack");
    this.mov(Rdi.into(), R15.into());
    this.mov(R10.into(), AsmValue::Indexing(R15.into(), offset_of!(NativeWrapper::popValue).as_u32() as isize));
    this.call(R10.into());
    this.push(Rax.into());
}

fn aquireRegister<T: AsmGen>(this: &mut T, regs: &mut RegisterManager, reg: Register) {
    if regs.aquireSpecific(reg) {
        this.push(reg.into())
    }
    println!("aquring {:?}", reg);
}

fn aquireAny<T: AsmGen>(this: &mut T, regs: &mut RegisterManager) -> Register {
    let aq = regs.aquireAny();

    if aq.1 {
        this.push(aq.0.into());
    }

    println!("aquring {:?}", aq.0);
    aq.0
}

fn releaseRegister<T: AsmGen>(this: &mut T, regs: &mut RegisterManager, reg: Register) {
    println!("releasing {:?}", reg);
    if regs.release(reg) {
        this.pop(reg)
    }
}

fn initCode<T: AsmGen>(this: &mut T) {
    this.comment("init code");
    this.mov(R15.into(), Rdi.into()); // vm ptr
    this.mov(R14.into(), Rsi.into()); // frame ptr
    this.mov(Rbx.into(), AsmValue::Indexing(R14.into(), 0)); // locals ptr

    printDigit(this, Rsp.into());

    this.newLine();
}

pub fn generateAssembly<T: AsmGen>(gen: &mut T, opCodes: Vec<OpCode>, vm: &VirtualMachine, namespace: &Namespace, returns: bool) {
    initCode(gen);

    let mut jmpCounter = 0usize;
    let mut regs = RegisterManager::new();
    let mut emulatedStack = vec![];

    let mut makeLabelsGreatAgain = HashMap::new();
    let mut jmpLookup = HashMap::new();
    let mut prevRet = false;

    for (i, op) in opCodes.iter().enumerate() {
        if let OpCode::Jmp { offset, jmpType } = op {
            let o = *offset + 1;
            let xd = (i as isize+o) as usize;
            let label = match makeLabelsGreatAgain.get(&xd) {
                None => {
                    let a = format!("JMP{}", jmpCounter);
                    makeLabelsGreatAgain.insert(xd, a.clone());
                    jmpCounter += 1;
                    a
                }
                Some(v) => {
                    v.clone()
                }
            };
            jmpLookup.insert(i, label);
        }
    }

    for (i, op) in opCodes.iter().enumerate() {
        println!("genning {:?}", op);
        debugPrint(gen, &format!("executing {:?}\n", op));
        gen.comment(&format!("start opcode {:?}", op));

        if let Some(v) = makeLabelsGreatAgain.get(&i) {
            gen.makeLabel(v)
        }

        let mut aqireReg = || {
            let r = aquireAny(gen, &mut regs);
            emulatedStack.push(r);
            r
        };

        match op {
            OpCode::PushInt(v) => {
                let r = aqireReg();
                gen.mov(r.into(), (*v).into());
            },
            OpCode::PushIntOne => {
                let r = aqireReg();
                gen.mov(r.into(), 1.into());
            },
            OpCode::PushIntZero => {
                let r = aqireReg();
                gen.mov(r.into(), 0.into());
            },
            OpCode::PushFloat(_) => todo!(),
            OpCode::PushBool(b) => {
                let r = aqireReg();
                gen.mov(r.into(), ((*b) as usize).into());
            },
            OpCode::PushChar(c) => {
                let r = aqireReg();
                gen.mov(r.into(), ((*c) as usize).into());
            },
            OpCode::Pop => {
                let r = emulatedStack.pop().unwrap();
                releaseRegister(gen, &mut regs, r)
            },
            OpCode::Dup => {
                todo!();
                let r = emulatedStack.pop().unwrap();
            }
            OpCode::GetLocal { index } => {
                let r = aqireReg();
                gen.mov(r.into(), AsmValue::Indexing(Rbx.into(), *index as isize));
            },
            OpCode::SetLocal { index } => {
                let r = emulatedStack.pop().unwrap();
                gen.mov(AsmLocation::Indexing(Rbx.into(), *index as isize), r.into());
                releaseRegister(gen, &mut regs, r)
            },
            OpCode::Jmp { offset, jmpType } => {
                let l = jmpLookup.get(&i).unwrap().clone();
                match jmpType {
                    JmpType::One | JmpType::True => {
                        let r = emulatedStack.pop().unwrap();
                        releaseRegister(gen, &mut regs, r);
                        gen.compare(r.into(), 1.into());
                        gen.jmpIfOne(l.into())
                    },
                    JmpType::Zero | JmpType::False => {
                        let r = emulatedStack.pop().unwrap();
                        releaseRegister(gen, &mut regs, r);
                        gen.compare(r.into(), 0.into());
                        gen.jmpIfZero(l.into())
                    },
                    JmpType::Jmp => gen.jmp(l.into()),
                    _ => todo!()
                }
            }
            OpCode::Return => {
                println!("UwU");
                if prevRet {
                    return;
                }
                prevRet = true;
                // FIXME
                if returns {
                    let r = emulatedStack.pop().unwrap();
                    gen.mov(Rax.into(), r.into());
                    // releaseRegister(gen, &mut regs, r)
                }
                printDigit(gen, Rsp.into());
                gen.ret()
            },
            OpCode::Add(t) => match t {
                DataType::Int => {
                    let r2 = emulatedStack.pop().unwrap();
                    let r1 = emulatedStack.pop().unwrap();
                    gen.add(r1.into(), r2.into());
                    releaseRegister(gen, &mut regs, r2);
                    emulatedStack.push(r1)
                }
                _ => todo!()
            }
            OpCode::Sub(t) => match t {
                DataType::Int => {
                    let r2 = emulatedStack.pop().unwrap();
                    let r1 = emulatedStack.pop().unwrap();
                    gen.sub(r1.into(), r2.into());
                    releaseRegister(gen, &mut regs, r2);
                    emulatedStack.push(r1)
                }
                _ => todo!()
            }
            OpCode::Div(_) => todo!(),
            OpCode::Mul(t) => match t {
                DataType::Int => {
                    let r2 = emulatedStack.pop().unwrap();
                    let r1 = emulatedStack.pop().unwrap();
                    gen.imul(r1.into(), r2.into());
                    releaseRegister(gen, &mut regs, r2);
                    emulatedStack.push(r1)
                }
                _ => todo!()
            }
            OpCode::Equals(t) => {
                todo!()
            }
            OpCode::Greater(t) => match t {
                DataType::Int | DataType::Bool | DataType::Char => {
                    let r2 = emulatedStack.pop().unwrap();
                    let r1 = emulatedStack.pop().unwrap();

                    aquireRegister(gen, &mut regs, Rax);

                    gen.xor(Rax.into(), Rax.into());
                    gen.compare(r1.into(), r2.into());
                    gen.setl(r1.into());

                    releaseRegister(gen,  &mut regs, Rax);

                    releaseRegister(gen, &mut regs, r2);
                    emulatedStack.push(r1);
                }
                _ => todo!()
            }
            OpCode::Less(t) => match t {
                DataType::Int | DataType::Bool | DataType::Char => {
                    let r2 = emulatedStack.pop().unwrap();
                    let r1 = emulatedStack.pop().unwrap();

                    aquireRegister(gen, &mut regs, Rax);

                    gen.xor(Rax.into(), Rax.into());
                    gen.compare(r1.into(), r2.into());
                    gen.setl(r1.into());

                    releaseRegister(gen,  &mut regs, Rax);

                    releaseRegister(gen, &mut regs, r2);
                    emulatedStack.push(r1);
                }
                _ => todo!()
            }
            OpCode::Or => {
                let r2 = emulatedStack.pop().unwrap();
                let r1 = emulatedStack.pop().unwrap();
                gen.or(r1.into(), r2.into());
                releaseRegister(gen, &mut regs, r2);
                emulatedStack.push(r1)
            }
            OpCode::And => {
                let r2 = emulatedStack.pop().unwrap();
                let r1 = emulatedStack.pop().unwrap();
                gen.and(r1.into(), r2.into());
                releaseRegister(gen, &mut regs, r2);
                emulatedStack.push(r1)
            }
            OpCode::Not => {
                let r1 = emulatedStack.pop().unwrap();
                gen.not(r1.into());
                emulatedStack.push(r1)
            }
            OpCode::StrNew(v) => {
                todo!();
                pushStr(gen, v.as_str())
            },
            OpCode::SCall { id } => {
                let f = namespace.getFunctionMeta(*id).unwrap();
                let argsCount = f.argsCount;
                let returns = f.returns();

                genCall(gen, &mut regs, &mut emulatedStack, namespace.id, *id, returns, argsCount)
            },
            OpCode::LCall { namespace, id } => {
                let f = &vm.getNamespace(*namespace).getFunction(*id).0;
                let returns = f.returns();
                let argsCount = f.argsCount;

                genCall(gen, &mut regs, &mut emulatedStack, *namespace, *id, returns, argsCount)
            }
            OpCode::GetLocalZero => {
                let r = aqireReg();
                gen.mov(r.into(), AsmValue::Indexing(Rbx.into(), 0));
            }
            OpCode::SetLocalZero => {
                let r = emulatedStack.pop().unwrap();
                gen.mov(AsmLocation::Indexing(Rbx.into(), 0), r.into());
                releaseRegister(gen, &mut regs, r)
            }
            OpCode::LessInt => {
                let r2 = emulatedStack.pop().unwrap();
                let r1 = emulatedStack.pop().unwrap();

                aquireRegister(gen, &mut regs, Rax);

                gen.xor(Rax.into(), Rax.into());
                gen.compare(r1.into(), r2.into());
                gen.setg(r1.into());

                releaseRegister(gen,  &mut regs, Rax);

                releaseRegister(gen, &mut regs, r2);
                emulatedStack.push(r1);
            }
            OpCode::SubInt => {
                let r2 = emulatedStack.pop().unwrap();
                let r1 = emulatedStack.pop().unwrap();

                gen.sub(r1.into(), r2.into());

                releaseRegister(gen, &mut regs, r2);

                emulatedStack.push(r1)
            }
            OpCode::MulInt => {
                let r2 = emulatedStack.pop().unwrap();
                let r1 = emulatedStack.pop().unwrap();
                gen.imul(r1.into(), r2.into());
                releaseRegister(gen, &mut regs, r2);
                emulatedStack.push(r1)
            }
            OpCode::AddInt => {
                let r2 = emulatedStack.pop().unwrap();
                let r1 = emulatedStack.pop().unwrap();
                gen.add(r1.into(), r2.into());
                releaseRegister(gen, &mut regs, r2);
                emulatedStack.push(r1)
            }
            e => todo!("{:?}", e)
        }
        gen.comment(&format!("end opcode {:?}\n", op));
    }
}