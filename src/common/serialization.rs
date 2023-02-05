use std::mem::transmute;

use crate::vm::{DataType, JmpType, ObjectMeta, OpCode, RawDataType, RawOpCode, VariableMetadata};
use crate::vm::DataType::Object;
use crate::vm::OpCode::*;

pub fn serialize(ops: &[OpCode]) -> Vec<u8> {
    let mut buf = vec![];

    for op in ops {
        let opId: [u8; 48] = unsafe { std::mem::transmute((*op).clone()) };
        buf.push(opId[0]);

        match op {
            FunBegin => {}
            Or => {}
            And => {}
            Not => {}
            FunEnd => {}
            F2I => {}
            I2F => {}
            Pop => {}
            Dup => {}
            ClassBegin => {}
            ClassEnd => {}
            ArrayLength => {}
            Return => {}

            PushInt(i) => {
                let data = (*i).to_ne_bytes();
                buf.extend_from_slice(&data);
            }
            PushFloat(i) => {
                let data = (*i).to_ne_bytes();
                buf.extend_from_slice(&data);
            }
            PushBool(i) => {
                buf.push(*i as u8);
            }
            Div(t) | Mul(t) | Sub(t) | Add(t) | Equals(t) | Greater(t) | Less(t) | ArrayNew(t)
            | ArrayStore(t) | ArrayLoad(t) => t.toBytes(&mut buf),
            FunReturn { typ } => match typ {
                None => {
                    buf.push(0);
                }
                Some(v) => {
                    buf.push(1);
                    v.toBytes(&mut buf)
                }
            },
            ClassName { name } | New { name } | FunName { name } => {
                let bs = name.escape_default().to_string();
                buf.extend(bs.len().to_ne_bytes());
                buf.extend(bs.as_bytes());
            }
            ClassField { name, typ } | GetField { name, typ } | SetField { name, typ } => {
                let bs = name.escape_default().to_string();
                buf.extend(bs.len().to_ne_bytes());
                buf.extend(bs.as_bytes());
                typ.toBytes(&mut buf);
            }
            Inc { typ, index } | Dec { typ, index } | SetLocal { index, typ } => {
                typ.toBytes(&mut buf);
                buf.extend(index.to_ne_bytes());
            }
            PushLocal { index } => {
                buf.extend(index.to_ne_bytes());
            }
            Jmp { offset, jmpType } => {
                buf.extend(offset.to_ne_bytes());
                jmpType.toBytes(&mut buf);
            }
            Call { encoded } => {
                let be = encoded.escape_default().to_string();
                buf.extend(be.len().to_ne_bytes());
                buf.extend(be.as_bytes());
            }
            LocalVarTable { typ, argsCount } => {
                buf.extend(typ.len().to_ne_bytes());
                for t in &**typ {
                    t.toBytes(&mut buf);
                }
                buf.extend(argsCount.to_ne_bytes());
            }
        }
    }
    buf
}

pub fn getStr(bytes: &[u8], index: usize) -> (String, usize) {
    let d = [
        bytes[index],
        bytes[index + 1],
        bytes[index + 2],
        bytes[index + 3],
        bytes[index + 4],
        bytes[index + 5],
        bytes[index + 6],
        bytes[index + 7],
    ];

    let mut consumed = d.len();

    let n: usize = unsafe { transmute(d) };
    consumed += n;

    let mut buf = vec![];

    for i in 0..n {
        buf.push(bytes[index + d.len() + i])
    }
    (String::from_utf8_lossy(&buf).to_string(), consumed)
}

pub fn getSize(bytes: &[u8], index: usize) -> (usize, usize) {
    let d = [
        bytes[index],
        bytes[index + 1],
        bytes[index + 2],
        bytes[index + 3],
        bytes[index + 4],
        bytes[index + 5],
        bytes[index + 6],
        bytes[index + 7],
    ];

    let consumed = d.len();

    let n: usize = unsafe { transmute(d) };

    (n, consumed)
}

pub fn getFloat(bytes: &[u8], index: usize) -> (f32, usize) {
    let d = [
        bytes[index],
        bytes[index + 1],
        bytes[index + 2],
        bytes[index + 3],
    ];

    let consumed = d.len();

    let n: f32 = unsafe { transmute(d) };

    (n, consumed)
}

pub fn getType(bytes: &[u8], index: usize) -> (DataType, usize) {
    let d = [bytes[index]];

    let mut consumed = d.len();

    let n: RawDataType = unsafe { transmute(d) };

    println!("a {:?}", n);

    let t = match n {
        RawDataType::Int => DataType::Int,
        RawDataType::Float => DataType::Float,
        RawDataType::Bool => DataType::Bool,
        RawDataType::Array => {
            let t = getType(bytes, index + consumed);
            consumed += t.1;
            DataType::Array {
                inner: Box::new(t.0),
            }
        }
        RawDataType::Object => {
            let n = getStr(bytes, index + consumed);
            consumed += n.1;
            let n2 = getSize(bytes, index + consumed);
            consumed += n2.1;
            todo!();

            Object { 0: Box::new(ObjectMeta { name: "".to_string(), generics: Box::new([]) }) }
        }
    };

    println!("b");

    (t, consumed)
}

pub fn getMeta(bytes: &[u8], index: usize) -> (VariableMetadata, usize) {
    let n = getStr(bytes, index);

    let mut consumed = n.1;

    let t = getType(bytes, index + consumed);
    consumed += n.1;

    (
        VariableMetadata {
            name: n.0,
            typ: t.0,
        },
        consumed,
    )
}

pub fn deserialize(data: Vec<u8>) -> Vec<OpCode> {
    let mut buf = vec![];
    let mut skip = 0;

    for (ind, o) in data.iter().enumerate() {
        let i = ind + 1;
        if skip > 0 {
            skip -= 1;
            continue;
        }
        let op: RawOpCode = unsafe { transmute(*o) };
        println!("{:?}", op);

        match op {
            RawOpCode::FunBegin => buf.push(FunBegin),
            RawOpCode::FunName => {
                let s = getStr(&data, i);
                skip += s.1;
                buf.push(FunName { name: s.0 })
            }
            RawOpCode::FunReturn => buf.push(FunReturn { typ: None }),
            RawOpCode::LocalVarTable => {
                let mut offset = i;
                let s = getSize(&data, offset);
                skip += s.1;
                offset += s.1;
                let mut bu = Vec::with_capacity(s.0);
                for _ in 0..s.0 {
                    let v = getMeta(&data, offset);
                    skip += v.1;
                    offset += v.1;
                    bu.push(v.0)
                }
                let siz = getSize(&data, offset);
                skip += siz.1;
                buf.push(LocalVarTable {
                    typ: bu.into_boxed_slice(),
                    argsCount: siz.0,
                })
            }
            RawOpCode::FunEnd => buf.push(FunEnd),
            RawOpCode::F2I => buf.push(F2I),
            RawOpCode::I2F => buf.push(I2F),
            RawOpCode::PushInt => {
                let s = getSize(&data, i);
                skip += s.1;
                buf.push(PushInt(s.0 as isize))
            }
            RawOpCode::PushFloat => {
                let s = getFloat(&data, i);
                println!("push f {} {}", s.0, s.1);
                skip += s.1;
                buf.push(PushFloat(s.0))
            }
            RawOpCode::PushBool => buf.push(PushBool(data[i] != 0)),
            RawOpCode::Pop => buf.push(Pop),
            RawOpCode::Dup => buf.push(Dup),
            RawOpCode::PushLocal => {
                let s = getSize(&data, i);
                skip += s.1;
                buf.push(PushLocal { index: s.0 })
            }
            RawOpCode::SetLocal => {
                println!("UwU");
                let t = getType(&data, i);
                skip += t.1;
                println!("meow {:?}", &t.1);
                let s = getSize(&data, i + t.1);
                skip += s.1;
                buf.push(SetLocal {
                    index: s.0,
                    typ: t.0,
                })
            }
            RawOpCode::Jmp => {
                let s = getSize(&data, i);
                skip += s.1;

                let jmpType: JmpType = unsafe { transmute(data[i + s.1]) };
                skip += 1;

                buf.push(Jmp {
                    offset: s.0 as isize,
                    jmpType,
                })
            }
            RawOpCode::Call => {
                let mut offset = i;

                let encName = getStr(&data, offset);
                skip += encName.1;
                offset += encName.1;

                buf.push(Call { encoded: encName.0 })
            }
            RawOpCode::Return => buf.push(Return),
            RawOpCode::Add => {
                let d = getType(&data, i);
                skip += d.1;
                buf.push(Add(d.0))
            }
            RawOpCode::Sub => {
                let d = getType(&data, i);
                skip += d.1;
                buf.push(Sub(d.0))
            }
            RawOpCode::Div => {
                let d = getType(&data, i);
                skip += d.1;
                buf.push(Div(d.0))
            }
            RawOpCode::Mul => {
                let d = getType(&data, i);
                skip += d.1;
                buf.push(Mul(d.0))
            }
            RawOpCode::Equals => {
                let d = getType(&data, i);
                skip += d.1;
                buf.push(Equals(d.0))
            }
            RawOpCode::Greater => {
                let d = getType(&data, i);
                skip += d.1;
                buf.push(Greater(d.0))
            }
            RawOpCode::Less => {
                let d = getType(&data, i);
                skip += d.1;
                buf.push(Less(d.0))
            }
            RawOpCode::Or => buf.push(Or),
            RawOpCode::And => buf.push(And),
            RawOpCode::Not => buf.push(Not),
            RawOpCode::ClassBegin => buf.push(ClassBegin),
            RawOpCode::ClassName => {}
            RawOpCode::ClassField => {}
            RawOpCode::ClassEnd => buf.push(ClassEnd),
            RawOpCode::New => {
                let s = getStr(&data, i);
                skip += s.1;
                buf.push(New { name: s.0 })
            }
            RawOpCode::GetField => {}
            RawOpCode::SetField => {}
            RawOpCode::ArrayNew => {
                let t = getType(&data, i);
                skip += t.1;
                buf.push(ArrayNew(t.0))
            }
            RawOpCode::ArrayStore => {}
            RawOpCode::ArrayLoad => {}
            RawOpCode::ArrayLength => buf.push(ArrayLength),
            RawOpCode::Inc => {
                let t = getType(&data, i);
                skip += t.1;
                let s = getSize(&data, i + t.1);
                skip += s.1;
                buf.push(Inc {
                    index: s.0,
                    typ: t.0,
                })
            }
            RawOpCode::Dec => {
                let t = getType(&data, i);
                skip += t.1;
                let s = getSize(&data, i + t.1);
                skip += s.1;
                buf.push(Dec {
                    index: s.0,
                    typ: t.0,
                })
            }
        }
    }

    buf
}
