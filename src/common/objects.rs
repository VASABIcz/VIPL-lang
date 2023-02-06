use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Deref;

use crate::vm::{DataType, Value};

pub trait Object: Debug {
    fn getName(&self) -> String;
    fn getFields(&self) -> &[DataType];
    fn setField(&mut self, field: usize, value: Value);
    fn getField(&self, field: usize) -> Option<Value>;
}

#[derive(Clone, Debug)]
pub struct ObjectDefinition {
    pub name: String,
    pub mapping: HashMap<String, (usize, DataType)>,
}

#[derive(Debug)]
pub struct Str {
    pub(crate) string: String,
}

impl Object for Str {
    fn getName(&self) -> String {
        String::from("String")
    }

    fn getFields(&self) -> &[DataType] {
        return &[];
    }

    fn setField(&mut self, field: usize, value: Value) {}

    fn getField(&self, field: usize) -> Option<Value> {
        return None;
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

    fn setField(&mut self, field: usize, value: Value) {}

    fn getField(&self, field: usize) -> Option<Value> { None }
}

fn cd(mut r: Box<dyn Any>) {
    let res = r.downcast_ref::<Str>().unwrap();
    println!("{}", res.string);
}

#[test]
fn xd() {
    let mut x: Box<Str> = Box::new(Str { string: String::from("UwU") });
    let mut e: Box<dyn Any> = x;
    cd(e);
}