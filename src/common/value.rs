use std::fmt::{Debug, Formatter};
use crate::ast::Expression;
use crate::heap::Hay;
use crate::objects::{Array, Str, ViplObject};
use crate::vm::{DataType, VirtualMachine};
use crate::vm::DataType::*;

#[derive(Copy, Clone)]
pub union Value {
    pub Num: isize,
    pub Flo: f64,
    pub Bol: bool,
    pub Chr: char,
    pub Reference: Hay<ViplObject>,
    pub FunctionPointer: (u32, u32)
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.asNum())
    }
}

impl From<DataType> for Value {
    #[inline]
    fn from(val: DataType) -> Self {
        val.toDefaultValue()
    }
}

impl From<isize> for Value {
    #[inline]
    fn from(val: isize) -> Self {
        Self{Num: val}
    }
}

impl From<i32> for Value {
    #[inline]
    fn from(val: i32) -> Self {
        Self{Num: val as isize}
    }
}

impl From<char> for Value {
    #[inline]
    fn from(val: char) -> Self {
        Value{Chr: val}
    }
}

impl From<f64> for Value {
    #[inline]
    fn from(val: f64) -> Self {
        Value{Flo: val}
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(val: bool) -> Self {
        Value{Bol: val}
    }
}

impl From<usize> for Value {
    #[inline]
    fn from(val: usize) -> Self {
        Value{Reference: Hay::new(val as *mut ViplObject)}
    }
}

impl Value {
    #[inline(always)]
    pub fn asHay(&self) -> Hay<ViplObject> {
        unsafe { self.Reference }
    }

    #[inline(always)]
    pub fn asRef(&self) -> &ViplObject {
        unsafe { &self.Reference }
    }

    #[inline(always)]
    pub fn asMutRef(&mut self) -> &mut ViplObject {
        unsafe { &mut self.Reference }
    }

    #[inline(always)]
    pub fn asChar(&self) -> char {
        unsafe { self.Chr }
    }

    #[inline(always)]
    pub fn asFunction(&self) -> (u32, u32) {
        unsafe { self.FunctionPointer }
    }

    #[inline(always)]
    pub fn asNum(&self) -> isize {
        unsafe { self.Num }
    }

    #[inline(always)]
    pub fn asFlo(&self) -> f64 {
        unsafe { self.Flo }
    }

    #[inline(always)]
    pub fn asBool(&self) -> bool {
        unsafe { self.Bol }
    }
}

impl Value {
    #[inline]
    pub fn add(&mut self, value: Value, typ: &DataType, vm: &mut VirtualMachine) {
        match typ {
            DataType::Int => {
                *self.getRefNum() += value.getNum();
            }
            DataType::Float => {
                *self.getRefFlo() += value.getFlo();
            }
            DataType::Bool => {}
            Object(it) => {
                match it.name.as_str() {
                    "String" => {
                        let str1 = self.getString();
                        let str2 = value.getString();

                        let mut buf = String::with_capacity(str1.len() + str2.len());

                        buf.push_str(str1);
                        buf.push_str(str2);

                        unsafe { self.Reference = Value::makeString(buf, vm).asHay() }
                    }
                    _ => panic!()
                }
            }
            DataType::Char => panic!(),
            Void => unreachable!(),
            Function { .. } => unreachable!()
        }
    }

    #[inline]
    pub fn sub(&mut self, value: &Value, typ: &DataType) {
        match typ {
            DataType::Int => {
                *self.getRefNum() -= value.getNumRef();
            }
            DataType::Float => {
                *self.getRefFlo() -= value.getFlo();
            }
            DataType::Bool => {}
            DataType::Object { .. } => {}
            DataType::Char => panic!(),
            Void => unreachable!(),
            Function { .. } => unreachable!()
        }
    }

    #[inline]
    pub fn mul(&mut self, value: &Value, typ: &DataType) {
        match typ {
            DataType::Int => {
                *self.getRefNum() *= value.getNumRef();
            }
            DataType::Float => {
                *self.getRefFlo() *= value.getFlo();
            }
            DataType::Bool => {}
            DataType::Object { .. } => {}
            DataType::Char => panic!(),
            Void => panic!(),
            Function { .. } => unreachable!()
        }
    }

    #[inline]
    pub fn div(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() /= value.getNumRef();
            }
            Float => {
                *self.getRefFlo() /= value.getFlo();
            }
            Bool => {}
            Object { .. } => {}
            Char => panic!(),
            Void => unreachable!(),
            Function { .. } => unreachable!()
        }
    }

    #[inline]
    pub fn f2i(&mut self) {
        *self = (self.getFlo() as isize).into()
    }

    #[inline]
    pub fn i2f(&mut self) {
        *self = (self.getNumRef() as f64).into()
    }
}

impl Value {
    pub fn toExpression(&self, t: &DataType) -> Expression {
        match t {
            Int => {
                Expression::IntLiteral(self.asNum().to_string())
            }
            Float => {
                Expression::FloatLiteral(self.asFlo().to_string())
            }
            Bool => {
                Expression::BoolLiteral(self.asBool())
            }
            Char => {
                Expression::CharLiteral(self.asChar())
            }
            Object(_) => {
                panic!()
            }
            Void => unreachable!(),
            Function { .. } => unreachable!()
        }
    }
}

impl Value {
    #[inline]
    pub fn gt(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNumRef() > val.getNumRef(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
            Void => unreachable!(),
            Function { .. } => unreachable!()
        }
    }

    #[inline(always)]
    pub fn inc(&mut self, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() += 1;
            }
            Float => {
                *self.getRefFlo() += 1.;
            }
            _ => panic!(),
        }
    }

    #[inline]
    pub fn dec(&mut self, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefNum() -= 1;
            }
            Float => {
                *self.getRefFlo() -= 1.;
            }
            _ => panic!(),
        }
    }

    #[inline]
    pub fn less(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNumRef() < val.getNumRef(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
            Void => unreachable!(),
            Function { .. } => unreachable!()
        }
    }

    #[inline]
    pub fn refLess(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNumRef() > val.getNumRef(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            _ => panic!()
        };

        *self = l.into()
    }

    #[inline]
    pub fn refGt(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNumRef() < val.getNumRef(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            Object { .. } => panic!(),
            Char => panic!(),
            Void => unreachable!(),
            Function { .. } => unreachable!()
        };

        *self = l.into()
    }

    #[inline]
    pub fn eq(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNumRef() == val.getNumRef(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Char => self.getChar() == val.getChar(),
            Object { .. } => panic!(),
            Void => unreachable!(),
            Function { .. } => unreachable!()
        }
    }

    #[inline]
    pub fn refEq(&mut self, val: &Value, typ: &DataType) {
        let x = match typ {
            Int => self.getNumRef() == val.getNumRef(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Char => self.getChar() == val.getChar(),
            Object(a) => panic!("{:?}", a),
            Void => unreachable!(),
            Function { .. } => unreachable!()
        };
        *self = x.into()
    }

    #[inline]
    pub fn getChar(&self) -> char {
        self.asChar()
    }

    #[inline]
    pub fn toDataType(&self) -> DataType {
        panic!()
    }
}

impl Value {
    #[inline]
    pub fn or(&mut self, val: &Value) {
        let r = self.getRefBol();
        *r = *r || val.getBool();
    }

    #[inline]
    pub fn and(&mut self, val: &Value) {
        let r = self.getRefBol();
        *r = *r && val.getBool();
    }

    #[inline]
    pub fn not(&mut self) {
        let r = self.getRefBol();
        *r = !*r;
    }
}

impl Value {
    #[inline(always)]
    pub fn getNum(self) -> isize {
        self.asNum()
    }

    #[inline(always)]
    pub fn getNumRef(&self) -> isize {
        self.asNum()
    }

    #[inline(always)]
    pub fn getFlo(&self) -> f64 {
        self.asFlo()
    }

    #[inline(always)]
    pub fn getRefFlo(&mut self) -> &mut f64 {
        unsafe { &mut self.Flo }
    }

    #[inline(always)]
    pub fn getRefNum(&mut self) -> &mut isize {
        unsafe { &mut self.Num }
    }

    #[inline(always)]
    pub fn getRefBol(&mut self) -> &mut bool {
        unsafe { &mut self.Bol }
    }

    #[inline(always)]
    pub fn getBool(&self) -> bool {
        self.asBool()
    }

    #[inline(always)]
    pub fn getReference(&self) -> &ViplObject {
        self.asRef()
    }

    #[inline(always)]
    pub fn getMutReference(&mut self) -> &mut ViplObject {
        self.asMutRef()
    }
}

impl Value {
    #[inline(always)]
    pub fn getString(&self) -> &String {
        &self.asRef().getStr().string
    }

    #[inline(always)]
    pub fn getMutArray(&mut self) -> &mut Array {
        self.asMutRef().getMutArr()
    }

    // FIXME not sure why inline never :D
    #[inline(never)]
    pub fn makeString(str: String, vm: &mut VirtualMachine) -> Value {
        Value{Reference: vm.heap.allocate(Str::new(str).into())}
    }

    pub fn makeFunction(namespaceID: u32, functionID: u32) -> Value {
        Value{FunctionPointer: (namespaceID, functionID)}
    }

    #[inline]
    pub fn makeObject(_obj: Box<dyn crate::objects::Object>) -> Value {
        todo!();
        // Value{Reference: ManuallyDrop::new(Rice::new(ViplObject::Runtime(Box::leak(obj))))}
    }

    #[inline]
    pub fn makeArray(arr: Vec<Value>, typ: DataType, vm: &mut VirtualMachine) -> Value {
        Value{Reference: vm.heap.allocate(Array{internal: arr, typ}.into())}
    }

    #[inline]
    pub fn valueStr(&self) -> String {
        self.asNum().to_string()
    }
}