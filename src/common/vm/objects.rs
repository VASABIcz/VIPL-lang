use crate::vm::dataType::DataType;
use crate::vm::heap::{Allocation, Hay, HayCollector};
use crate::vm::nativeObjects::{
    ObjectType, UntypedObject, ViplNativeObject, ViplObject, ViplObjectMeta,
};
use crate::vm::value::{Value, Xd};
use std::any::Any;
use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Debug;

unsafe impl<T: Allocation + Debug> Sync for ViplObject<T> {}

unsafe impl<T: Allocation + Debug> Send for ViplObject<T> {}

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
        Self { string: str }
    }
}

impl<T: Allocation + Debug> From<Hay<T>> for Value {
    fn from(value: Hay<T>) -> Self {
        Self {
            Reference: Hay::new(value.inner as *mut Xd),
        }
    }
}

impl From<Str> for ViplObject<Str> {
    #[inline]
    fn from(val: Str) -> Self {
        ViplObject {
            meta: ViplObjectMeta {
                namespaceId: 0,
                structId: 0,
                objectType: ObjectType::Native(ViplNativeObject::default()),
            },
            data: val,
        }
    }
}

impl From<Array> for ViplObject<Array> {
    #[inline]
    fn from(val: Array) -> Self {
        ViplObject {
            meta: ViplObjectMeta {
                namespaceId: 0,
                structId: 1,
                objectType: ObjectType::Native(ViplNativeObject::default()),
            },
            data: val,
        }
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
      pub internal: Vec<Value>
}

impl Array {
    pub fn new() -> Self {
        Self {
            internal: vec![]
        }
    }

    pub fn insert(&mut self, value: Value, index: usize) {
        if index == self.internal.len() {
            self.internal.push(value)
        } else {
            self.internal[index] = value
        }
    }
}

impl Allocation for Array {
    fn collectAllocations(&self, allocations: &mut HayCollector) {
        allocations.visit(self as *const Self as usize);

        // FIXME
        println!("FIXME visit all children")

/*        if let DataType::Object(_a) = &self.typ {
            for obj in &self.internal {
                match obj.asRef::<Xd>().meta.objectType {
                    ObjectType::Simple(_) => todo!(),
                    ObjectType::Native(_) => todo!(),
                }
            }
        }*/
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
