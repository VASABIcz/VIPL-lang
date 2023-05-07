use std::fmt::Debug;
use crate::vm::heap::{Allocation, Hay, HayCollector};
use crate::vm::namespace::Namespace;
use crate::vm::nativeObjects::{ObjectType, ViplObject};
use crate::vm::objects::Str;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

#[derive(Debug)]
#[repr(C)]
pub struct StackFrame {
    pub localVariables: Box<[Value]>,
    pub programCounter: usize,
    pub namespaceId: usize,
    pub functionId: usize
}

impl StackFrame {
    pub fn getInt(&self, index: usize) -> isize {
        self.localVariables.get(index).unwrap().asNum()
    }

    pub fn getFloat(&self, index: usize) -> f64 {
        self.localVariables.get(index).unwrap().asFlo()
    }

    pub fn getReference<T: Debug + Allocation>(&mut self, index: usize) -> &mut ViplObject<T> {
        self.localVariables.get_mut(index).unwrap().getMutReference()
    }

    pub fn getString(&mut self, index: usize) -> &String {
        self.localVariables.get_mut(index).unwrap().getString()
    }
}

impl StackFrame {
    pub fn collect(&self, vm: &VirtualMachine, collector: &mut HayCollector) {
        for local in self.localVariables.iter() {
            if vm.heap.contains(local.asNum() as usize) {
                collector.visit(local.asNum() as usize);
                let m = local.asRefMeta();
                m.collectAllocations(collector);
            }
        }
    }
}

impl Drop for StackFrame {
    fn drop(&mut self) {}
}