use crate::vm::namespace::StructMeta;
use crate::vm::value::Value;
use crate::vm::vm::VirtualMachine;

pub trait NativeObject {
    fn getField(&mut self, field: usize, vm: &mut VirtualMachine) -> Option<Value>;

    fn setField(&mut self, field: usize, value: Value, vm: &mut VirtualMachine);

    fn destroy(&mut self, vm: &mut VirtualMachine);
}

struct RegixObject {
    regix: String,
    v: StructMeta
}


impl RegixObject {

}

/*
cant be constructed
are constructed by a function call

regix object
- captureCount

tcp connection object
-

file object


sock = tcp::connect("localhost:6000")
msg = [1, 2, 3, 4, 5, 6, 7, 8]
sock.tcp::write(msg)
sock.tcp::close()
 */