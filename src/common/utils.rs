use std::error::Error;
use std::{env, fs};
use crate::lexer::tokenizeSource;
use crate::parser::{parseDataType, TokenProvider};
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::dataType::DataType;

// same as Vec but can be unsafely modified and accessed
pub struct FastVec<T> {
    pub cap: usize,
    pub ptr: *mut T,
    pub size: usize
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
pub fn parseDataTypeFromStr(s: &str) -> Result<DataType, Box<dyn Error>> {
    let p = tokenizeSource(s)?;
    parseDataType(&mut TokenProvider::new(p))
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
            break
        }
        if hasResolved || c != r {
            strBuf.push(c.unwrap().to_str().unwrap().to_string());
            hasResolved = true;
        }
    }
    let id = strBuf.len()-1;
    strBuf.get_mut(id).map(|it| {
        *it = it.strip_suffix(".vipl").unwrap().to_string()
    });
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