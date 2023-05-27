use crate::vm::namespace::Namespace;
use crate::vm::vm::VirtualMachine;

#[derive(Debug)]
pub struct NamespaceLoader {
    pub lookupPaths: Vec<String>,
    pub lookupBuiltin: Vec<fn(&mut VirtualMachine)>,
}

impl NamespaceLoader {
    pub fn registerPath(&mut self, path: &str, depth: usize) {
        todo!()
    }

    pub fn registerBuiltin(&mut self, name: Vec<String>, init: fn(&mut VirtualMachine)) {
        todo!()
    }

    pub fn loadNamespace(&self, path: Vec<String>) -> Namespace {
        todo!()
    }

    pub fn new() -> Self {
        let s = Self {
            lookupPaths: vec![],
            lookupBuiltin: vec![],
        };

        s
    }
}
