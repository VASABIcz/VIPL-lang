use crate::std::fs::registerFs;
use crate::std::json::registerJson;
use crate::std::os::registerOs;
use crate::std::out::registerOut;
use crate::std::regex::registerRegex;
use crate::std::strings::registerStrings;
use crate::std::test::registerTest;
use crate::std::utils::registerUtils;
use crate::std::vm::registerVm;
use crate::vm::vm::VirtualMachine;

pub mod fs;
pub mod json;
pub mod out;
pub mod regex;
pub mod vm;
pub mod os;
pub mod regix;
pub mod test;
pub mod utils;
pub mod strings;

pub fn bootStrapVM() -> VirtualMachine {
    let mut vm = VirtualMachine::new();

    registerOut(&mut vm);
    registerOs(&mut vm);
    registerVm(&mut vm);
    registerRegex(&mut vm);
    registerJson(&mut vm);
    registerTest(&mut vm);
    registerUtils(&mut vm);
    registerStrings(&mut vm);
    registerFs(&mut vm);

    vm
}