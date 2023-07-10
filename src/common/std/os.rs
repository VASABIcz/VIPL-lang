use std::ffi::{CStr, CString};
use std::process::exit;
use std::time::{Instant, SystemTime};
use crate::ast::RawExpression;
use crate::utils::microsSinceEpoch;
use crate::vm::dataType::DataType::{Bool, Char, Float, Int};
use crate::vm::dataType::{DataType, Generic};
use crate::vm::namespace::NamespaceState::Loaded;
use crate::vm::namespace::{GlobalMeta, Namespace};
use crate::vm::objects::{Array, Str};
use crate::vm::value::Value;
use crate::vm::variableMetadata::VariableMetadata;
use crate::vm::vm::VirtualMachine;

pub fn registerOs(vm: &mut VirtualMachine) {
    let mut namespace = Namespace::new("os", vm);

    namespace.makeNative("exec", &[DataType::str()], |a, b| {
        use libc;
        let c = b.getString(0);
        let idk = CString::new(c.clone()).unwrap();

        let retCode = unsafe { libc::system(idk.as_ptr()) };

        return retCode.into();
    }, Int, false);

    namespace.makeNativeNoRat("exit", &[Int], |vm, s| {
        exit(s.get(0).getNum() as i32)
    }, false);

    namespace.makeNative("time", &[], |vm, s| {
        ((microsSinceEpoch()/1000) as isize).into()
    }, Int, false);

    vm.registerNamespace(namespace);
}
