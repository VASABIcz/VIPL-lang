use std::fmt::Debug;
use std::ops::Deref;
use crate::vm::heap::{Allocation, Hay, HayCollector};
use crate::vm::namespace::Namespace;
use crate::vm::nativeObjects::{ObjectType, ViplObject};
use crate::vm::objects::Str;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

#[derive(Debug)]
#[repr(C)]
pub struct StackFrame {
    pub localVariables: *mut Value,
    pub programCounter: usize,
    pub namespaceId: usize,
    pub functionId: usize
}

impl StackFrame {
    pub fn getInt(&self, index: usize) -> isize {
        unsafe { self.localVariables.add(index).read().asNum() }
    }

    pub fn get(&self, index: usize) -> Value {
        unsafe { self.localVariables.add(index).read() }
    }

    pub fn getRef(&self, index: usize) -> &Value {
        unsafe { &*self.localVariables.add(index) }
    }

    pub fn getRefMut(&mut self, index: usize) -> &mut Value {
        unsafe { &mut *self.localVariables.add(index) }
    }

    pub fn getFloat(&self, index: usize) -> f64 {
        self.get(index).asFlo()
    }

    pub fn getReference<T: Debug + Allocation>(&mut self, index: usize) -> &mut ViplObject<T> {
        self.getRefMut(index).getMutReference()
    }

    pub fn getString(&mut self, index: usize) -> &String {
        self.getRef(index).getString()
    }
}

impl StackFrame {
    pub fn collect(&self, vm: &VirtualMachine, collector: &mut HayCollector) {
        todo!()
/*        for local in self.localVariables.iter() {
            if vm.heap.contains(local.asNum() as usize) {
                collector.visit(local.asNum() as usize);
                let m = local.asRefMeta();
                m.collectAllocations(collector);
            }
        }*/
    }
}

impl Drop for StackFrame {
    fn drop(&mut self) {}
}