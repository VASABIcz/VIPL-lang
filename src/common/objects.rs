use std::any::Any;
use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Debug;

use crate::vm::{DataType, Value};

pub trait Object: Debug + Any {
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
pub struct ObjectDefinition {
    pub name: String,
    pub mapping: HashMap<String, (usize, DataType)>,
}

#[derive(Debug)]
pub struct Str {
    pub string: String,
}

/*
impl Drop for Str {
    fn drop(&mut self) {
        println!("i am being freed :3")
    }
}

 */

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

#[derive(Debug)]
pub struct Array {
    pub internal: Vec<Value>,
    pub typ: DataType,
}

impl Object for Array {
    fn getName(&self) -> String {
        String::from("Array")
    }

    fn getFields(&self) -> &[DataType] {
        &[]
    }

    fn setField(&mut self, _field: usize, _value: Value) {}

    fn getField(&self, _field: usize) -> Option<Value> { None }
}

fn cd(r: Box<dyn Any>) {
    let res = r.downcast_ref::<Str>().unwrap();
    println!("{}", res.string);
}

#[test]
fn xd() {
    let x: Box<Str> = Box::new(Str { string: String::from("UwU") });
    let e: Box<dyn Any> = x;
    cd(e);
}