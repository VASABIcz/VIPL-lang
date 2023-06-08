use crate::bytecodeGen::SymbolicOpcode;
use crate::bytecodeGen::SymbolicOpcode::Op;
use crate::errors::LoadFileError;
use crate::parser::{TokenProvider};
use crate::vm;
use crate::vm::dataType::DataType;
use crate::vm::namespace::Namespace;
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::OpCode::{GetGlobal, LCall, SCall, SetGlobal};
use crate::vm::vm::{OpCode, VirtualMachine};
use std::arch::asm;
use std::error::Error;
use std::{env, fs};
use std::ops::Range;
use crate::ast::{Expression, Statement};
use crate::lexer::{LexingUnit, Token, tokenizeSource};
use crate::lexingUnits::TokenType;
use crate::viplParser::parseDataType;

// same as Vec but can be unsafely modified and accessed
pub struct FastVec<T> {
    pub ptr: *mut T,
    pub cap: usize,
    pub size: usize,
}

#[inline]
pub fn genFunName(name: &str, args: &[DataType]) -> String {
    format!("{}({})", name, argsToString(args))
}

#[inline]
pub fn genFunNameMeta(name: &str, args: &[VariableMetadata], argsLen: usize) -> String {
    format!("{}({})", name, argsToStringMeta(&args[0..argsLen]))
}

#[inline]
pub fn genFunNameMetaTypes(name: &str, args: &[DataType], argsLen: usize) -> String {
    format!("{}({})", name, typesToStringMeta(&args[0..argsLen]))
}

#[inline]
pub fn argsToStringMeta(args: &[VariableMetadata]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(&arg.typ.toString());
        if i != args.len() - 1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline]
pub fn typesToStringMeta(args: &[DataType]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(&arg.toString());
        if i != args.len() - 1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline]
pub fn argsToString(args: &[DataType]) -> String {
    let mut buf = String::new();

    for (i, arg) in args.iter().enumerate() {
        buf.push_str(&arg.toString());
        if i != args.len() - 1 {
            buf.push_str(", ")
        }
    }
    buf
}

#[inline]
pub fn parseDataTypeFromStr(s: &str, units: &mut [Box<dyn LexingUnit<TokenType>>]) -> Result<DataType, LoadFileError<TokenType>> {
    let p = tokenizeSource(s, units)?;
    Ok(parseDataType(&mut TokenProvider::new(p))?)
}

pub fn namespacePath(path: &str) -> Vec<String> {
    let mut con123 = fs::canonicalize(path).unwrap();
    let mut con = con123.iter();
    let mut cwd123 = env::current_dir().unwrap();
    let mut cwd = cwd123.iter();
    let mut hasResolved = false;
    let mut strBuf = vec![];

    loop {
        let c = con.next();
        let r = cwd.next();
        if c.is_none() {
            break;
        }
        if hasResolved || c != r {
            strBuf.push(c.unwrap().to_str().unwrap().to_string());
            hasResolved = true;
        }
    }
    let id = strBuf.len() - 1;
    strBuf
        .get_mut(id)
        .map(|it| *it = it.strip_suffix(".vipl").unwrap().to_string());
    strBuf
}

#[macro_export]
macro_rules! viplDbg {
    () => {
        eprintln!("[{}:{}]", file!(), line!())
    };
    ($val:expr $(,)?) => {
        if DEBUG {
               match $val {
            tmp => {
                eprintln!("[{}:{}] {} = {:#?}",
                    file!(), line!(), stringify!($val), &tmp);
                tmp
            }
        }
        }
        else {
            ""
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbg!($val)),+,)
    };
}

#[inline(always)]
pub fn saveRegisters() {
    unsafe {
        asm!(
            "push rbx",
            "push rsi",
            "push rdi",
            // "push rbp",
            // "push rsp",
            "push r8",
            "push r9",
            "push r10",
            "push r11",
            "push r12",
            "push r13",
            "push r14",
            "push r15",
            "sub rsp, 8"
        )
    }
}

// TODO
pub fn callNative(ptr: usize) -> Value {
    unsafe {
        asm!(
            "push rbx",
            "push rsi",
            "push rdi",
            // "push rbp",
            // "push rsp",
            "push r8",
            "push r9",
            "push r10",
            "push r11",
            "push r12",
            "push r13",
            "push r14",
            "push r15",
            "sub rsp, 8"
        );

        asm!(
            "add rsp, 8",
            "pop r15",
            "pop r14",
            "pop r13",
            "pop r12",
            "pop r11",
            "pop r10",
            "pop r9",
            "pop r8",
            // "pop rsp",
            // "pop rbp",
            "pop rdi",
            "pop rsi",
            "pop rbx",
        );
    }

    todo!()
}

#[inline(always)]
pub fn restoreRegisters() {
    unsafe {
        asm!(
            "add rsp, 8",
            "pop r15",
            "pop r14",
            "pop r13",
            "pop r12",
            "pop r11",
            "pop r10",
            "pop r9",
            "pop r8",
            // "pop rsp",
            // "pop rbp",
            "pop rdi",
            "pop rsi",
            "pop rbx",
        )
    }
}

#[inline(always)]
pub fn printRegisters() {
    let mut rax = 0usize;
    let mut rbx = 0usize;
    let mut rcx = 0usize;
    let mut rdx = 0usize;
    let mut rsi = 0usize;
    let mut rdi = 0usize;
    let mut rbp = 0usize;
    let mut rsp = 0usize;
    let mut r8 = 0usize;
    let mut r9 = 0usize;
    let mut r10 = 0usize;
    let mut r11 = 0usize;
    let mut r12 = 0usize;
    let mut r13 = 0usize;
    let mut r14 = 0usize;
    let mut r15 = 0usize;

    unsafe {
        asm!(
        "mov {rax}, rax",
        "mov {rbx}, rbx",
        "mov {rcx}, rcx",
        "mov {rdx}, rdx",
        "mov {rsi}, rsi",
        "mov {rdi}, rdi",
        "mov {rbp}, rbp",
        "mov {rsp}, rsp",
        "mov {r8}, r8",
        "mov {r9}, r9",
        "mov {r10}, r10",
        "mov {r11}, r11",
        "mov {r12}, r12",
        "mov {r13}, r13",
        "mov {r14}, r14",
        "mov {r15}, r15",
        rax = out(reg) rax,
        rbx = out(reg) rbx,
        rcx = out(reg) rcx,
        rdx = out(reg) rdx,
        rsi = out(reg) rsi,
        rdi = out(reg) rdi,
        rbp = out(reg) rbp,
        rsp = out(reg) rsp,
        r8 = out(reg) r8,
        r9 = out(reg) r9,
        r10 = out(reg) r10,
        r11 = out(reg) r11,
        r12 = out(reg) r12,
        r13 = out(reg) r13,
        r14 = out(reg) r14,
        r15 = out(reg) r15,
        )
    }
    println!("rax: {}", rax);
    println!("rbx: {}", rbx);
    println!("rcx: {}", rcx);
    println!("rdx: {}", rdx);
    println!("rsi: {}", rsi);
    println!("rdi: {}", rdi);
    println!("rbp: {}", rbp);
    println!("rsp: {}", rsp);
    println!("r8: {}", r8);
    println!("r9: {}", r9);
    println!("r10: {}", r10);
    println!("r11: {}", r11);
    println!("r12: {}", r12);
    println!("r13: {}", r13);
    println!("r14: {}", r14);
    println!("r15: {}", r15);
}

fn isPure(ops: &[OpCode], vm: &VirtualMachine, namespace: &Namespace) -> bool {
    for op in ops {
        match op {
            SCall { id } => {
                let f = namespace.getFunction(*id);

                return f.0.isPure;
            }
            LCall { namespace, id } => {
                let f = vm.getNamespace(*namespace as usize).getFunction(*id as usize);

                return f.0.isPure;
            }
            OpCode::DynamicCall(_, _) => return false,
            SetGlobal { .. } => return false,
            GetGlobal { .. } => return false,
            _ => {}
        }
    }
    true
}

pub fn readNeighbours(ptr: *const Value, amount: usize) {
    unsafe {
        for x in -(amount as isize)..amount as isize {
            let v = ptr.offset(x).read();
            println!("read {:?} at offset {}", v, x)
        }
    }
}

pub fn getSymbolicChunks(ops: &Vec<SymbolicOpcode>) -> Vec<Vec<OpCode>> {
    let mut buf = vec![];

    let mut curBuf = vec![];

    for op in ops {
        match op {
            Op(o) => curBuf.push(o.clone()),
            _ => {
                if !curBuf.is_empty() {
                    buf.push(curBuf);
                    curBuf = vec![];
                }
            }
        }
    }

    buf
}

pub fn transform<F: Fn(Vec<OpCode>) -> Vec<OpCode>>(
    ops: Vec<SymbolicOpcode>,
    f: F,
) -> Vec<SymbolicOpcode> {
    let mut buf = vec![];

    let mut curBuf = vec![];

    for op in ops {
        match op {
            Op(o) => curBuf.push(o.clone()),
            _ => {
                if !curBuf.is_empty() {
                    buf.extend(f(curBuf).into_iter().map(|it| Op(it)));
                    curBuf = vec![];
                }
                buf.push(op);
            }
        }
    }

    if !curBuf.is_empty() {
        buf.extend(f(curBuf).into_iter().map(|it| Op(it)));
    }

    buf
}

pub fn printOps(ops: &[OpCode]) {
    println!("===");
    for op in ops {
        println!("{:?}", op)
    }
    println!("===");
}

pub fn getRanges(tokens: &[Token<TokenType>], row: usize) -> Vec<Range<usize>> {
    let mut buf = vec![];

    for token in tokens {
        if token.location.row != row {
            continue
        }

        let len = token.str.len();
        let start = token.location.col;

        buf.push(start..start+len)
    }

    buf
}

pub fn findMax<T: PartialOrd>(data: &[T]) -> Option<&T> {
    let mut max = None;

    for item in data {
        if max.is_none() {
            max = Some(item);
            continue
        }

        if max.unwrap() < item {
            max = Some(item);
        }
    }


    max
}

pub fn findMaxBy<T, F: Fn(&T, &T) -> bool>(data: &[T], f: F) -> Option<&T> {
    let mut max = None;

    for item in data {
        if max.is_none() {
            max = Some(item);
            continue
        }

        if f(max.unwrap(), item) {
            max = Some(item);
        }
    }


    max
}

pub fn visualizeRange(ranges: &[Range<usize>], a: char, b: char, offset: usize) -> String {
    let n = findMaxBy(ranges, |prev, next| prev.end < next.end).unwrap().end;

    let mut buf = String::from(a).repeat(n-offset);


    for range in ranges {
        for i in range.clone() {
            buf.replace_range(i-offset..i-offset, &b.to_string());
        }
    }

    buf
}

pub fn errorBodys(src: &str, messages: &[(Vec<Range<usize>>, usize, Option<&str>)]) -> String {
    let mut buf = String::new();
    let mut lastLine: Option<usize> = None;

    for message in messages {
        let ranges = &message.0;

        let rowStr = getRow(src, message.1).trim_end();
        let trim = rowStr.trim_start();
        let offset = rowStr.len() - trim.len();

        let viz = visualizeRange(&ranges, ' ', '^', offset);

        match lastLine {
            None => {
                buf += "  | ";
                buf += trim;
                buf += "\n";
            }
            Some(v) if v != message.1 => {
                buf += "  | ";
                buf += trim;
                buf += "\n";
            }
            _ => {}
        };

        buf += "  | ";
        buf += viz.trim_end();
        if let Some(v) = message.2 {
            buf += " -> ";
            buf += v;
            buf += "\n";
        }

        lastLine = Some(message.1);
    }

    buf.trim_end().to_string()
}

pub fn errorBody(src: &str, messages: &[(&Expression, Option<&str>)]) -> String {
    errorBodys(src, &messages.iter().map(|it| (it.0.getRanges(it.0.getRow()), it.0.getRow(), it.1)).collect::<Vec<_>>().as_slice())
}

pub fn errorBody2(src: &str, messages: &[(&Statement, Option<&str>)]) -> String {
    errorBodys(src, &messages.iter().map(|it| (it.0.getRanges(it.0.getRow()), it.0.getRow(), it.1)).collect::<Vec<_>>().as_slice())
}

pub fn getRow(src: &str, row: usize) -> &str {
    src.split('\n').enumerate().find(|(i, it)| {
        *i == row
    }).unwrap().1
}