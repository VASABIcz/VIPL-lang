use crate::vm::heap::{Allocation, HayCollector};
use crate::vm::nativeObjects::ViplObject;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;
use std::fmt::Debug;

#[derive(Debug)]
#[repr(C)]
pub struct StackFrame {
    pub localVariables: *mut Value,
    pub programCounter: usize, // will be set to program stack ptr
    // pub nativeStackEnd: Option<usize>, // current frame stack end, gets assigned when called frame FFI
    // pub nativeStackStart: usize, // current frame stack start
    pub namespaceId: usize,
    pub functionId: usize,
}

impl StackFrame {
    pub fn new(locals: *mut Value, nId: usize, fId: usize) -> Self {
        Self {
            localVariables: locals,
            programCounter: 0,
            namespaceId: nId,
            functionId: fId,
        }
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

    pub fn getReference<T: Debug + Allocation>(&mut self, index: usize) -> &mut ViplObject<T> {
        self.getRefMut(index).getMutReference()
    }

    pub fn getString(&self, index: usize) -> &String {
        self.getRef(index).getString()
    }

    pub fn getChar(&self, index: usize) -> char {
        self.get(index).asChar()
    }

    pub fn getInt(&self, index: usize) -> isize {
        self.get(index).asNum()
    }

    pub fn getUInt(&self, index: usize) -> usize {
        self.get(index).asUnsigned()
    }

    pub fn getFloat(&self, index: usize) -> f64 {
        self.get(index).asFlo()
    }

    pub fn getBool(&self, index: usize) -> bool {
        self.get(index).asBool()
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
