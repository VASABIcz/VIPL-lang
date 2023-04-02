use std::any::Any;
use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Debug;
use crate::heap::{Allocation, HayCollector};
use crate::value::Value;

use crate::vm::{DataType};

#[derive(Debug, Clone)]
pub enum ViplObject {
    Arr(Array),
    Str(Str),
    // Runtime(Box<dyn Object>),
}

impl Allocation for ViplObject {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        match self {
            ViplObject::Arr(v) => v.collectAllocations(allocations),
            ViplObject::Str(v) => v.collectAllocations(allocations)
        }
    }
}

unsafe impl Sync for ViplObject {

}

unsafe impl Send for ViplObject {

}

impl ViplObject {
    #[inline]
    pub fn getArr(&self) -> &Array {
        match self {
            ViplObject::Arr(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn toArr(self) -> Array {
        match self {
            ViplObject::Arr(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn getMutArr(&mut self) -> &mut Array {
        match self {
            ViplObject::Arr(v) => v,
            v => panic!("{:?}", v),
        }
    }

    #[inline(always)]
    pub fn getStr(&self) -> &Str {
        match self {
            ViplObject::Str(v) => v,
            v => panic!("{:?}", v),
        }
    }

    #[inline]
    pub fn toStr(self) -> Str {
        match self {
            ViplObject::Str(v) => v,
            v => panic!("{:?}", v),
        }
    }

    #[inline]
    pub fn getMutStr(&mut self) -> &mut Str {
        match self {
            ViplObject::Str(v) => v,
            _ => panic!(),
        }
    }

    #[inline]
    pub fn asObj(&self) -> &dyn Object {
        match self {
            ViplObject::Arr(a) => a,
            ViplObject::Str(a) => a,
            // ViplObject::Runtime(v) => &**v,
        }
    }
}

pub trait Object: Debug + Any + Allocation {
    fn getName(&self) -> String;
    fn getFields(&self) -> &[DataType];
    fn setField(&mut self, field: usize, value: Value);
    fn getField(&self, field: usize) -> Option<Value>;
}

impl dyn Object {
    #[inline]
    pub unsafe fn downcast_mut_unchecked<T: Object + 'static>(&mut self) -> &mut T {
        unsafe { &mut *(self as *mut dyn Object as *mut T) }
    }

    #[inline]
    pub unsafe fn downcast_ref_unchecked<T: Object + 'static>(&self) -> &T {
        unsafe { &*(self as *const dyn Object as *const T) }
    }

    #[inline]
    pub unsafe fn downcast_ref<T: Object + 'static>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe { Some(self.downcast_ref_unchecked()) }
        } else {
            None
        }
    }

    #[inline]
    pub fn downcast_mut<T: Object + 'static>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe { Some(self.downcast_mut_unchecked()) }
        } else {
            None
        }
    }

    #[inline]
    pub fn is<T: Object + 'static>(&self) -> bool {
        let t = TypeId::of::<T>();

        let concrete = self.type_id();

        t == concrete
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct ObjectDefinition {
    pub name: String,
    pub mapping: HashMap<String, (usize, DataType)>,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Str {
    pub string: String,
}

impl Str {
    pub fn new(str: String) -> Self {
        Self {
            string: str
        }
    }
}

impl From<Str> for ViplObject {
    #[inline]
    fn from(val: Str) -> Self {
        ViplObject::Str(val)
    }
}

impl From<Array> for ViplObject {
    #[inline]
    fn from(val: Array) -> Self {
        ViplObject::Arr(val)
    }
}

/*
impl Drop for Str {
    fn drop(&mut self) {
        println!("i am being freed :3")
    }
}

 */

impl Allocation for Str {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        println!("str ptr {}", self as *const Self as usize);
        allocations.visit(self as *const Self as usize)
    }
}

impl Object for Str {
    fn getName(&self) -> String {
        String::from("String")
    }

    fn getFields(&self) -> &[DataType] {
        &[]
    }

    fn setField(&mut self, _field: usize, _value: Value) {}

    fn getField(&self, _field: usize) -> Option<Value> {
        None
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct Array {
    pub internal: Vec<Value>,
    pub typ: DataType,
}

impl Array {
    pub fn new(typ: DataType) -> Self {
        Self {
            internal: vec![],
            typ,
        }
    }
}

impl Allocation for Array {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        allocations.visit(self as *const Self as usize);
        if let DataType::Object(_a) = &self.typ {
            for obj in &self.internal {
                obj.asRef();
            }
        }
    }
}

impl Object for Array {
    fn getName(&self) -> String {
        String::from("Array")
    }

    fn getFields(&self) -> &[DataType] {
        &[]
    }

    fn setField(&mut self, _field: usize, _value: Value) {}

    fn getField(&self, _field: usize) -> Option<Value> {
        None
    }
}