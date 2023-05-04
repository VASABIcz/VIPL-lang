use crate::vm::heap::{Hay, HayCollector};
use crate::vm::namespace::Namespace;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

#[derive(Debug)]
pub struct StackFrame<'a> {
    pub localVariables: &'a mut [Value],
    pub previous: Option<&'a StackFrame<'a>>,
    pub programCounter: usize,
    pub namespace: &'a Namespace,
    pub functionID: usize
}

impl StackFrame<'_> {
    pub fn collect(&self, vm: &VirtualMachine, collector: &mut HayCollector) {
        for local in self.localVariables.iter() {
            if vm.heap.contains(local.asNum() as usize) {
                collector.visit(local.asNum() as usize)
            }
        }
        if let Some(prev) = self.previous {
            prev.collect(vm, collector)
        }
    }
}

impl Drop for StackFrame<'_> {
    fn drop(&mut self) {}
}