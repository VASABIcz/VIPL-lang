use std::collections::HashMap;
use offset::offset_of;
use crate::asm::asmLib::{AsmGen, AsmValue, Concrete, Location, Register};
use crate::asm::asmLib::Register::{Bl, R10, R11, R12, R13, R14, R15, Rax, Rbx, Rcx, Rdi, Rdx, Rsi, Rsp};
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
    this.comment(&format!("saveRegisters"));
    this.push(Rax.into());
    this.push(Rdi.into());
    this.push(Rsi.into());
    this.push(Rdx.into());
}

fn restoreRegisters<T: AsmGen>(this: &mut T) {
    this.comment(&format!("restoreRegisters"));
    this.pop(Rdx.into());
    this.pop(Rsi.into());
    this.pop(Rdi.into());
    this.pop(Rax.into());
}

fn debugPrint<T: AsmGen>(this: &mut T, text: &str) {
    if !DEBUG {
        return;
    }

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
}

fn genCall<T: AsmGen>(this: &mut T, namespaceID: usize, functionID: usize, returns: bool, argsCount: usize) {
    this.comment(&format!("call {}:{} -> {}", namespaceID, functionID, returns));

    this.mov(Rdi.into(), R15.into());
    this.mov(Rsi.into(), functionID.into());
    this.mov(Rdx.into(), namespaceID.into());
    this.mov(Rcx.into(), Rsp.into());

    this.push(Rbx.into());

    this.call(Location::Indexing(R15.into(), offset_of!(NativeWrapper::lCall).as_u32() as isize));

    this.pop(Rbx.into());

    this.offsetStack(argsCount as isize); // consume args

    if returns {
        printDigit(this, Rax.into());
        this.push(Rax.into());
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

    this.comment(&format!("printDigit {}", asmValue.clone().toString()));

    println!("offset {}", offset_of!(NativeWrapper::printDigit).as_u32() as isize);


    this.mov(Rdi.into(),asmValue);
    saveRegisters(this);
    this.call(Location::Indexing(R15.into(), offset_of!(NativeWrapper::printDigit).as_u32() as isize));
    restoreRegisters(this);
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

    this.mov(Location::Indexing(Rbx.into(), index as isize), R10.into());
}

fn popStack<T: AsmGen>(this: &mut T) {
    this.comment("popStack");
    this.mov(Rdi.into(), R15.into());
    this.mov(R10.into(), AsmValue::Indexing(R15.into(), offset_of!(NativeWrapper::popValue).as_u32() as isize));
    this.call(R10.into());
    this.push(Rax.into());
}

fn initCode<T: AsmGen>(this: &mut T) {
    this.comment("init code");
    this.mov(R15.into(), Rdi.into()); // vm ptr
    this.mov(R14.into(), Rsi.into()); // frame ptr
    this.mov(Rbx.into(), AsmValue::Indexing(R14.into(), 0)); // locals ptr

    printDigit(this, Rsp.into());

    this.newLine();
}

pub fn generateAssembly<T: AsmGen>(generator: &mut T, opCodes: Vec<OpCode>, vm: &VirtualMachine, namespace: &Namespace, returns: bool) {
    initCode(generator);

    let mut jmpCounter = 0usize;

    let mut makeLabelsGreatAgain = HashMap::new();
    let mut jmpLookup = HashMap::new();

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
        debugPrint(generator, &format!("executing {:?}\n", op));
        generator.comment(&format!("start opcode {:?}", op));

        if let Some(v) = makeLabelsGreatAgain.get(&i) {
            generator.makeLabel(v)
        }
        match op {
            OpCode::PushInt(v) => generator.push((*v).into()),
            OpCode::PushIntOne => generator.push(1.into()),
            OpCode::PushIntZero => generator.push(0.into()),
            OpCode::PushFloat(_) => todo!(),
            OpCode::PushBool(b) => generator.push((*b as isize).into()),
            OpCode::PushChar(c) => generator.push((*c as isize).into()),
            OpCode::Pop => {
                printDigit(generator, R12.into());
                generator.pop(R12)
            },
            OpCode::Dup => {
                generator.pop(R12);
                generator.push(R12.into());
                generator.push(R12.into());
            }
            OpCode::GetLocal { index } => getLocal(generator, *index),
            OpCode::SetLocal { index } => setLocal(generator, *index),
            OpCode::Jmp { offset, jmpType } => {
                let l = jmpLookup.get(&i).unwrap().clone();
                match jmpType {
                    JmpType::One | JmpType::True => {
                        generator.pop(R12.into());
                        generator.compare(R12.into(), 1.into());
                        generator.jmpIfOne(l.into())
                    },
                    JmpType::Zero | JmpType::False => {
                        generator.pop(R12.into());
                        generator.compare(R12.into(), 0.into());
                        generator.jmpIfZero(l.into())
                    },
                    JmpType::Jmp => generator.jmp(l.into()),
                    JmpType::Gt => generator.jmpIfGt(l.into()),
                    JmpType::Less => generator.jmpIfLess(l.into()),
                }
            }
            OpCode::Return => {
                // FIXME
                if returns {
                    generator.pop(Rax.into());
                }
                printDigit(generator, Rsp.into());
                generator.ret()
            },
            OpCode::Add(t) => match t {
                DataType::Int => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.add(R12.into(), R13.into());
                    generator.push(R12.into());
                }
                _ => todo!()
            }
            OpCode::Sub(t) => match t {
                DataType::Int => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.sub(R12.into(), R13.into());
                    generator.push(R12.into());
                }
                _ => todo!()
            }
            OpCode::Div(_) => todo!(),
            OpCode::Mul(t) => match t {
                DataType::Int => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.imul(R12.into(), R13.into());
                    generator.push(R12.into());
                }
                _ => todo!()
            }
            OpCode::Equals(t) => match t {
                DataType::Int | DataType::Bool | DataType::Char => {
                    generator.pop(R12);
                    generator.pop(R13);
                    generator.compare(R12.into(), R13.into());
                }
                _ => todo!()
            }
            OpCode::Greater(t) => match t {
                DataType::Int | DataType::Bool | DataType::Char => {
                    generator.pop(R12);
                    generator.pop(R13);

                    generator.xor(Rax.into(), Rax.into());
                    generator.compare(R13.into(), R12.into());
                    generator.setl(R12);

                    generator.push(R12.into())
                }
                _ => todo!()
            }
            OpCode::Less(t) => match t {
                DataType::Int | DataType::Bool | DataType::Char => {
                    generator.pop(R12);
                    generator.pop(R13);

                    generator.xor(Rax.into(), Rax.into());
                    generator.compare(R13.into(), R12.into());
                    generator.setg(R12);

                    generator.push(R12.into())
                }
                _ => todo!()
            }
            OpCode::Or => {
                generator.pop(R12);
                generator.pop(R13);
                generator.or(R12.into(), R13.into());
                generator.push(R12.into());
            }
            OpCode::And => {
                generator.pop(R12);
                generator.pop(R13);
                generator.and(R12.into(), R13.into());
                generator.push(R12.into());
            }
            OpCode::Not => {
                generator.pop(R12);
                generator.not(R12.into());
                generator.push(R12.into());
            }
            OpCode::StrNew(v) => pushStr(generator, v.as_str()),
            OpCode::SCall { id } => {
                let f = namespace.getFunctionMeta(*id).unwrap();
                let argsCount = f.argsCount;
                let returns = f.returns();

                genCall(generator, namespace.id, *id, returns, argsCount)
            },
            OpCode::LCall { namespace, id } => {
                let f = &vm.getNamespace(*namespace).getFunction(*id).0;
                let returns = f.returns();
                let argsCount = f.argsCount;

                genCall(generator, *namespace, *id, returns, argsCount)
            }
            OpCode::GetLocalZero => {
                getLocal(generator, 0)
            }
            OpCode::SetLocalZero => {
                setLocal(generator, 0)
            }
            OpCode::LessInt => {
                generator.pop(R12);
                generator.pop(R13);
                printDigit(generator, R12.into());
                printDigit(generator, R13.into());
                generator.xor(Rax.into(), Rax.into());
                generator.compare(R13.into(), R12.into());
                generator.setg(R12);
                printDigit(generator, R12.into());
                generator.push(R12.into())
            }
            OpCode::SubInt => {
                generator.pop(R13);
                generator.pop(R12);

                printDigit(generator, R12.into());
                printDigit(generator, R13.into());

                generator.sub(R12.into(), R13.into());
                printDigit(generator, R12.into());
                generator.push(R12.into());
            }
            OpCode::MulInt => {
                generator.pop(R12);
                generator.pop(R13);
                generator.imul(R12.into(), R13.into());
                generator.push(R12.into());
            }
            OpCode::AddInt => {
                generator.pop(R12);
                generator.pop(R13);

                printDigit(generator, R12.into());
                printDigit(generator, R13.into());

                generator.add(R12.into(), R13.into());

                printDigit(generator, R12.into());

                generator.push(R12.into());
            }
            e => todo!("{:?}", e)
        }
        generator.comment(&format!("end opcode {:?}\n", op));
    }
}