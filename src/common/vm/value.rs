use crate::ast::RawExpression;
use crate::vm;
use crate::vm::dataType::DataType;
use crate::vm::dataType::DataType::*;
use std::fmt::{Debug, Formatter};
use std::hint::unreachable_unchecked;

use crate::vm::heap::{Allocation, Hay, HayCollector};
use crate::vm::nativeObjects::{UntypedObject, ViplNativeObject, ViplObject, ViplObjectMeta};
use crate::vm::objects::{Array, Str};
use crate::vm::vm::VirtualMachine;

#[derive(Copy, Clone, Debug)]
pub struct Xd;

impl Allocation for Xd {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        todo!()
    }
}

#[derive(Copy, Clone)]
pub union Value {
    pub Num: isize,
    pub UNum: usize,
    pub Flo: f64,
    pub Bol: bool,
    pub Chr: char,
    pub Reference: Hay<Xd>,
    pub FunctionPointer: (u32, u32),
}

impl Value {
    #[inline(always)]
    pub fn null() -> Self {
        Value::from(0)
    }
}

impl Allocation for Value {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        if allocations
            .allocated
            .contains(&(self.asRefMeta() as *const UntypedObject as usize))
        {
            self.asRefMeta().collectAllocations(allocations)
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.asUnsigned())
    }
}

impl Into<usize> for Value {
    #[inline]
    fn into(self) -> usize {
        unsafe { self.UNum }
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
        Self { Num: val }
    }
}

impl From<i32> for Value {
    #[inline]
    fn from(val: i32) -> Self {
        Self { Num: val as isize }
    }
}

impl From<char> for Value {
    #[inline]
    fn from(val: char) -> Self {
        Value { Chr: val }
    }
}

impl From<f64> for Value {
    #[inline]
    fn from(val: f64) -> Self {
        Value { Flo: val }
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(val: bool) -> Self {
        Value { Bol: val }
    }
}

impl From<usize> for Value {
    #[inline]
    fn from(val: usize) -> Self {
        Value {
            Reference: Hay::new(val as *mut () as *mut Xd),
        }
    }
}

impl<T: Allocation + Debug> Into<Hay<Xd>> for Hay<ViplObject<T>> {
    fn into(self) -> Hay<Xd> {
        Hay::new(self.inner as *mut Xd)
    }
}

impl Value {
    #[inline(always)]
    pub fn asHay<T: Allocation + Debug>(&self) -> Hay<ViplObject<T>> {
        let casted = self.asHayUntyped().inner as *mut ViplObject<T>;

        Hay::new(casted)
    }

    #[inline(always)]
    pub fn asHayUntyped(&self) -> Hay<Xd> {
        unsafe { self.Reference }
    }

    #[inline(always)]
    pub fn asRef<T: Allocation + Debug>(&self) -> &ViplObject<T> {
        let casted = self.asHayUntyped().inner as *mut ViplObject<T>;

        unsafe { &*casted }
    }

    #[inline(always)]
    pub fn asRefMeta(&self) -> &UntypedObject {
        let casted = self.asHayUntyped().inner as *mut UntypedObject;

        unsafe { &*casted }
    }

    #[inline(always)]
    pub fn asMutRef<T: Allocation + Debug>(&mut self) -> &mut ViplObject<T> {
        let casted = unsafe { self.Reference.inner } as *mut ViplObject<T>;

        unsafe { &mut *casted }
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
    pub fn asUnsigned(&self) -> usize {
        unsafe { self.UNum }
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
            Object(it) => match it.name.as_str() {
                "String" => {
                    let str1 = self.getString();
                    let str2 = value.getString();

                    let mut buf = String::with_capacity(str1.len() + str2.len());

                    buf.push_str(str1);
                    buf.push_str(str2);

                    self.Reference = Value::makeString(buf, vm).asHayUntyped()
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
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
            _ => unreachable!(),
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
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn div(&mut self, value: &Value, typ: &DataType) {
        match typ {
            Int => {
                *self.getRefFlo() = *self.getRefNum() as f64 / value.getNumRef() as f64;
            }
            Float => {
                *self.getRefFlo() /= value.getFlo();
            }
            _ => unreachable!(),
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
    pub fn toExpression(&self, t: &DataType) -> RawExpression {
        match t {
            Int => RawExpression::IntLiteral(self.asNum().to_string()),
            Float => RawExpression::FloatLiteral(self.asFlo().to_string()),
            Bool => RawExpression::BoolLiteral(self.asBool()),
            Char => RawExpression::CharLiteral(self.asChar()),
            _ => unsafe { unreachable_unchecked() },
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
            _ => unreachable!(),
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
            _ => unreachable!(),
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
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn less(&self, val: &Value, typ: &DataType) -> bool {
        match typ {
            Int => self.getNumRef() < val.getNumRef(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn refLess(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNumRef() > val.getNumRef(),
            Float => self.getFlo() > val.getFlo(),
            Bool => self.getBool() & !val.getBool(),
            _ => unreachable!(),
        };

        *self = l.into()
    }

    #[inline]
    pub fn refGt(&mut self, val: &Value, typ: &DataType) {
        let l = match typ {
            Int => self.getNumRef() < val.getNumRef(),
            Float => self.getFlo() < val.getFlo(),
            Bool => !self.getBool() & val.getBool(),
            _ => unreachable!(),
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
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn refEq(&mut self, val: &Value, typ: &DataType) {
        let x = match typ {
            Int => self.getNumRef() == val.getNumRef(),
            Float => self.getFlo() == val.getFlo(),
            Bool => self.getBool() == val.getBool(),
            Char => self.getChar() == val.getChar(),
            Object(a) => self.getNumRef() == val.getNumRef(),
            _ => unreachable!(),
        };
        *self = x.into()
    }

    #[inline]
    pub fn getChar(&self) -> char {
        self.asChar()
    }

    #[inline]
    pub fn toDataType(&self) -> DataType {
        unreachable!()
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
        unsafe {
            &mut self.Num
        }
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
    pub fn getReference<T: Allocation + Debug>(&self) -> &ViplObject<T> {
        self.asRef()
    }

    #[inline(always)]
    pub fn getMutReference<T: Allocation + Debug>(&mut self) -> &mut ViplObject<T> {
        self.asMutRef()
    }
}

impl Value {
    #[inline(always)]
    pub fn getString(&self) -> &String {
        &self.asRef::<Str>().data.string
    }

    #[inline(always)]
    pub fn getMutArray(&mut self) -> &mut Array {
        &mut self.asMutRef::<Array>().data
    }

    // FIXME not sure why inline never :D
    #[inline(never)]
    pub fn makeString(str: String, vm: &mut VirtualMachine) -> Value {
        let obj = ViplObject::<Str>::str(Str::new(str));

        let hObj = vm.allocate(obj);

        Value {
            Reference: hObj.into(),
        }
    }

    pub fn makeFunction(namespaceID: u32, functionID: u32) -> Value {
        Value {
            FunctionPointer: (namespaceID, functionID),
        }
    }

    #[inline]
    pub fn makeArray(arr: Vec<Value>, vm: &mut VirtualMachine) -> Value {
        Value {
            Reference: vm
                .allocate(ViplObject::<Array>::arr(Array { internal: arr }))
                .into(),
        }
    }

    #[inline]
    pub fn valueStr(&self) -> String {
        self.asNum().to_string()
    }
}
