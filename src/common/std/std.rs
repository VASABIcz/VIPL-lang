use crate::std::json::registerJson;
use crate::std::os::registerOs;
use crate::std::out::registerOut;
use crate::std::regex::registerRegex;
use crate::std::strings::registerStrings;
use crate::std::test::registerTest;
use crate::std::utils::registerUtils;
use crate::std::vm::registerVm;
use crate::vm::vm::VirtualMachine;

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

    vm
}
