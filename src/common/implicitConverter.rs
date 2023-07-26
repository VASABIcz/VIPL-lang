use std::fmt::Debug;
use std::sync::OnceLock;

use crate::bytecodeGen::SymbolicOpcode;
use crate::codeGenCtx::SimpleCtx;
use crate::errors::CodeGenError;
use crate::vm::dataType::{DataType, ObjectMeta};
use crate::vm::dataType::DataType::{Int, Object};
use crate::vm::vm::OpCode::GetField;

pub trait TypeConverter<T: Debug>: Debug {
    fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool;
    fn blindConvert(&self, src: &DataType) -> Option<DataType>;
    fn genBlindConvert(&self, src: DataType, ctx: SimpleCtx<T>) -> Result<(), CodeGenError> {
        Ok(())
    }
    fn genConvert(&self, src: DataType, tgt: DataType, ctx: SimpleCtx<T>) -> Result<(), CodeGenError> {
        Ok(())
    }
    fn accuracy(&self) -> usize;
}

#[derive(Clone, Debug)]
struct ValueConverter {
    ac: usize,
}

impl<T: Debug> TypeConverter<T> for ValueConverter {
    fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool {
        tgt.isValue()
    }

    fn blindConvert(&self, src: &DataType) -> Option<DataType> {
        Some(DataType::Value)
    }

    fn accuracy(&self) -> usize {
        self.ac
    }
}

#[derive(Clone, Debug)]
struct NullReferenceConverter {
    ac: usize,
}

impl<T: Debug> TypeConverter<T> for NullReferenceConverter {
    fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool {
        src.isNull() && tgt.isReferenceNullable()
    }

    fn blindConvert(&self, src: &DataType) -> Option<DataType> {
        None
    }

    fn accuracy(&self) -> usize {
        self.ac
    }
}

#[derive(Clone, Debug)]
struct RefNullifyConverter {
    ac: usize,
}

impl<T: Debug> TypeConverter<T> for RefNullifyConverter {
    fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool {
        src.isReferenceNonNullable() && tgt.isReferenceNullable() && tgt.isRefNamed(src.getRefName())
    }

    fn blindConvert(&self, src: &DataType) -> Option<DataType> {
        if src.isReferenceNonNullable() {
            let c = src.clone();
            return Some(c.toNullable());
        }
        None
    }

    fn accuracy(&self) -> usize {
        self.ac
    }
}

#[derive(Clone, Debug)]
struct ObjNullifyConverter {
    ac: usize,
}

impl<T: Debug> TypeConverter<T> for ObjNullifyConverter {
    fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool {
        src.isObjectNonNullable() && tgt.isObjectNullable()
    }

    fn blindConvert(&self, src: &DataType) -> Option<DataType> {
        if src.isObjectNonNullable() {
            return Some(Object(true));
        }
        None
    }

    fn accuracy(&self) -> usize {
        self.ac
    }
}

#[derive(Debug)]
struct BoxConverter {
    ac: usize,
}

impl TypeConverter<SymbolicOpcode> for BoxConverter {
    fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool {
        src.isInt() && tgt.isRefNamed("Int")
            || src.isBool() && tgt.isRefNamed("Bool")
            || src.isChar() && tgt.isRefNamed("Char")
            || src.isFloat() && tgt.isRefNamed("Float")
    }

    fn blindConvert(&self, src: &DataType) -> Option<DataType> {
        if src.isPrimitiveType() {
            return Some(src.clone().toBoxedType());
        }
        None
    }
    fn genBlindConvert(&self, src: DataType, mut ctx: SimpleCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
        boxValue(src, ctx)
    }
    fn genConvert(&self, src: DataType, tgt: DataType, mut ctx: SimpleCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
        boxValue(src, ctx)
    }

    fn accuracy(&self) -> usize {
        self.ac
    }
}

pub fn boxValue(src: DataType, mut ctx: SimpleCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
    if src.isInt() {
        let s = ctx.getStruct("Int")?.newInstruction();
        ctx.push(s);
    }
    if src.isInt() {
        let s = ctx.getStruct("Float")?.newInstruction();
        ctx.push(s);
    }
    if src.isInt() {
        let s = ctx.getStruct("Bool")?.newInstruction();
        ctx.push(s);
    }
    if src.isInt() {
        let s = ctx.getStruct("Char")?.newInstruction();
        ctx.push(s);
    } else {
        return Err(CodeGenError::ExpectedRawType);
    }
    ctx.push(GetField { fieldID: 0 });

    Ok(())
}

pub fn unboxValue(src: DataType, mut ctx: SimpleCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
    if !src.isPrimitiveType() {
        return Err(CodeGenError::ExpectedRawType);
    }

    ctx.push(GetField { fieldID: 0 });

    Ok(())
}

#[derive(Debug)]
struct UnBoxConverter {
    ac: usize,
}

impl TypeConverter<SymbolicOpcode> for UnBoxConverter {
    fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool {
        src.isRefNamed("Int") && tgt.isInt()
            || src.isRefNamed("Float") && tgt.isFloat()
            || src.isRefNamed("Bool") && tgt.isBool()
            || src.isRefNamed("Char") && tgt.isChar()
    }

    fn blindConvert(&self, src: &DataType) -> Option<DataType> {
        if src.isBoxed() {
            return Some(src.clone().toUnboxedType());
        }

        return None;
    }

    fn genBlindConvert(&self, src: DataType, ctx: SimpleCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
        unboxValue(src, ctx)
    }

    fn genConvert(&self, src: DataType, tgt: DataType, ctx: SimpleCtx<SymbolicOpcode>) -> Result<(), CodeGenError> {
        unboxValue(src, ctx)
    }

    fn accuracy(&self) -> usize {
        self.ac
    }
}

#[derive(Debug)]
struct ObjectConverter {
    ac: usize,
}

impl<T: Debug> TypeConverter<T> for ObjectConverter {
    fn canConvert(&self, src: &DataType, tgt: &DataType) -> bool {
        src.isReferenceNonNullable() && tgt.isObjectNonNullable()
    }

    fn blindConvert(&self, src: &DataType) -> Option<DataType> {
        if src.isReferenceNonNullable() {
            return Some(DataType::Object(false));
        }
        None
    }

    fn accuracy(&self) -> usize {
        self.ac
    }
}

#[derive(Debug)]
pub struct ImplicitConverter<T> {
    pub(crate) rules: Vec<Box<dyn TypeConverter<T>>>,
}

unsafe impl<T> Send for ImplicitConverter<T> {}

unsafe impl<T> Sync for ImplicitConverter<T> {}

impl<T: Debug> ImplicitConverter<T> {
    pub fn findChain<'a>(&'a self, depth: usize, maxDepth: usize, mut curRes: &mut Vec<&'a dyn TypeConverter<T>>, reses: &mut Vec<Vec<&'a dyn TypeConverter<T>>>, src: &DataType, tgt: &DataType) {
        println!("tries: {:?}", curRes);
        if depth >= maxDepth {
            return;
        }

        for rule in &self.rules {
            if rule.canConvert(&src, &tgt) {
                println!("solution {:?} {:?} {:?}", src, tgt, rule);
                let mut c = curRes.clone();
                c.push(rule.as_ref());
                reses.push(c);
            } else if let Some(v) = rule.blindConvert(src) && src != &v {
                curRes.push(rule.as_ref());
                self.findChain(depth + 1, maxDepth, curRes, reses, &v, tgt);
                curRes.pop();
            }
        }
    }

    pub fn findAbstractChain<'a, F: Fn(&DataType) -> bool>(&'a self, depth: usize, maxDepth: usize, mut curRes: &mut Vec<&'a dyn TypeConverter<T>>, reses: &mut Vec<(Vec<&'a dyn TypeConverter<T>>, DataType)>, src: &DataType, tgt: &F) {
        println!("tries: {:?}", curRes);
        if depth >= maxDepth {
            return;
        }

        for rule in &self.rules {
            if let Some(v) = rule.blindConvert(&src) && tgt(&v) {
                let mut c = curRes.clone();
                c.push(rule.as_ref());
                reses.push((c, v));
            } else if let Some(v) = rule.blindConvert(src) && src != &v {
                curRes.push(rule.as_ref());
                self.findAbstractChain(depth + 1, maxDepth, curRes, reses, &v, tgt);
                curRes.pop();
            }
        }
    }

    pub fn findConversionChain(&self, src: &DataType, tgt: &DataType) -> Option<Vec<&dyn TypeConverter<T>>> {
        if src == tgt {
            return Some(vec![]);
        }

        for rule in &self.rules {
            if rule.canConvert(&src, &tgt) {
                return Some(vec![rule.as_ref()]);
            }
        }

        let mut results = vec![];
        let mut curChain = vec![];

        self.findChain(0, 5, &mut curChain, &mut results, &src, &tgt);

        if results.is_empty() {
            return None;
        }

        let mut min = results.pop().unwrap();

        for result in results {
            if result.len() < min.len() {
                min = result
            }
        }

        return Some(min);
    }

    pub fn findAbstractConversion<F: Fn(&DataType) -> bool>(&self, src: DataType, tgt: F) -> Option<(Vec<&dyn TypeConverter<T>>, DataType)> {
        if tgt(&src) {
            return Some((vec![], src));
        }

        for rule in &self.rules {
            if let Some(v) = rule.blindConvert(&src) && tgt(&v) {
                return Some((vec![rule.as_ref()], v));
            }
        }

        let mut results = vec![];
        let mut curChain = vec![];

        self.findAbstractChain(0, 5, &mut curChain, &mut results, &src, &tgt);

        if results.is_empty() {
            return None;
        }

        let mut max = results.pop().unwrap();

        for result in results {
            if calculateAccuracy(&result.0) > calculateAccuracy(&max.0) {
                max = result
            }
        }

        return Some(max);
    }
}

fn calculateAccuracy<T: Debug>(converters: &[&dyn TypeConverter<T>]) -> usize {
    if converters.is_empty() {
        return usize::MAX;
    }

    let mut sum = 0;

    for converter in converters {
        sum += converter.accuracy();
    }

    return sum / (converters.len() * converters.len());
}

// FIXME cycle prevention - kinda hard
// TODO caching
// TODO invert the process?
// TODO cost/likely hood calculation
#[test]
fn testConversion() {
    let rules = implicitConversionRules();

    let c = ImplicitConverter { rules };

    let res = c.findConversionChain(&Int, &Object(true)).unwrap();
    println!("{:?} {}", &res, calculateAccuracy(&res));
    let res1 = c.findAbstractConversion(Int, |it| it.isReferenceNullable()).unwrap();
    println!("{:?} {}", &res1, calculateAccuracy(&res1.0));
}

#[test]
fn testVoidConversion() {
    let converter = getImplicitConverter();

    let x = converter.findConversionChain(&Int, &DataType::Void);

    assert!(x.is_none());
}

#[test]
fn testRefNullifyConversion() {
    let converter = getImplicitConverter();

    let x = converter.findConversionChain(&DataType::Reference(ObjectMeta{
        name: "Int".to_string(),
        generics: Box::new([]),
        nullable: false,
    }), &DataType::Reference(ObjectMeta{
        name: "Char".to_string(),
        generics: Box::new([]),
        nullable: true,
    }));

    assert!(x.is_none());
}

// FIXME this could be static just like lexing/parsing units
pub fn implicitConversionRules() -> Vec<Box<dyn TypeConverter<SymbolicOpcode>>> {
    let mut rules: Vec<Box<dyn TypeConverter<SymbolicOpcode>>> = vec![];

    let mut x = 0;


    // TODO find the best values
    // the priority is value conversion should be the last option
    rules.push(Box::new(ValueConverter { ac: x }));
    rules.push(Box::new(ObjectConverter {
        ac: {
            x += 600;
            x
        }
    }));
    rules.push(Box::new(ObjNullifyConverter {
        ac: {
            x += 1;
            x
        }
    }));
    rules.push(Box::new(NullReferenceConverter {
        ac: {
            x += 1;
            x
        }
    }));
    rules.push(Box::new(RefNullifyConverter {
        ac: {
            x += 1;
            x
        }
    }));
    rules.push(Box::new(BoxConverter {
        ac: {
            x += 1;
            x
        }
    }));
    rules.push(Box::new(UnBoxConverter {
        ac: {
            x += 1;
            x
        }
    }));

    rules
}

static IMPLICIT_CONVERTER: OnceLock<ImplicitConverter<SymbolicOpcode>> = OnceLock::new();

pub fn getImplicitConverter() -> &'static ImplicitConverter<SymbolicOpcode> {
    IMPLICIT_CONVERTER.get_or_init(|| ImplicitConverter { rules: implicitConversionRules() })
}

// int -> Int
// Int -> int
// bool -> Bool
// Bool -> bool
// char -> Char
// Char -> char
// float -> Float
// Float -> float
// * -> value
// Reference -> Object
// Reference -> Reference?
// null -> Reference?


// int -> Object?
// int -> Int -> Object -> Object?
// Int * Float

// int * float -> float
//  Int -> int
//  Float -> float
// bool -> x : x

// Int / Float
// int / int
// float / float
// float / int
// int / float