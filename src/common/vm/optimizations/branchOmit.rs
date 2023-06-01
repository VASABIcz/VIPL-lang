use crate::bytecodeGen::SymbolicOpcode;
use crate::vm::vm::{JmpType, OpCode};

pub fn branchOmit(a: Vec<SymbolicOpcode>) -> (Vec<SymbolicOpcode>, bool) {
    let mut buf = vec![];
    let mut didOptimize = false;
    let mut i = 0;

    while i < a.len() {
        let op = a.get(i).unwrap();
        let nextOp = a.get(i+1);

        match op {
            SymbolicOpcode::Op(OpCode::PushBool(v)) => {
                if let Some(SymbolicOpcode::Jmp(b, a)) = nextOp {
                    match a {
                        JmpType::One => {
                            if *v {
                                buf.push(SymbolicOpcode::Jmp(*b, JmpType::Jmp))
                            }
                        }
                        JmpType::Zero => {
                            if !v {
                                buf.push(SymbolicOpcode::Jmp(*b, JmpType::Jmp))
                            }
                        }
                        JmpType::True => {
                            if *v {
                                buf.push(SymbolicOpcode::Jmp(*b, JmpType::Jmp))
                            }
                        }
                        JmpType::False => {
                            if !v {
                                buf.push(SymbolicOpcode::Jmp(*b, JmpType::Jmp))
                            }
                        }
                        _ => panic!()
                    }
                    i += 2;
                    didOptimize = true;

                    continue
                }
                else {
                    buf.push(op.clone())
                }
            }
            _ => buf.push(op.clone())
        }
        i += 1
    }


    (buf, didOptimize)
}