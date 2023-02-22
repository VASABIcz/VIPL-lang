use std::env::temp_dir;
use std::error::Error;
use std::fs;
use std::process::Command;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

static LIBRARY: &str =
    "#include <stdbool.h>
#include <stdint.h>

typedef struct StackFrame StackFrame;
typedef struct ViplObject ViplObject;
typedef struct VirtualMachine VirtualMachine;

typedef struct NativeWrapper {
  void (*pushInt)(struct VirtualMachine*, intptr_t);
  void (*pushFloat)(struct VirtualMachine*, float);
  void (*pushBool)(struct VirtualMachine*, bool);
  void (*pushChar)(struct VirtualMachine*, uint8_t);
  void (*pushRef)(struct VirtualMachine*, const struct ViplObject*);
  intptr_t (*popInt)(struct VirtualMachine*);
  float (*popFloat)(struct VirtualMachine*);
  bool (*popBool)(struct VirtualMachine*);
  uint8_t (*popChar)(struct VirtualMachine*);
  const struct ViplObject *(*popRef)(struct VirtualMachine*);
  intptr_t (*getLocalsInt)(struct StackFrame*, uintptr_t);
  float (*getLocalsFloat)(struct StackFrame*, uintptr_t);
  bool (*getLocalsBool)(struct StackFrame*, uintptr_t);
  uint8_t (*getLocalsChar)(struct StackFrame*, uintptr_t);
  const struct ViplObject *(*getLocalsRef)(struct StackFrame*, uintptr_t);
  intptr_t (*arrGetInt)(struct VirtualMachine*, struct ViplObject*, uintptr_t);
  float (*arrGetFloat)(struct VirtualMachine*, struct ViplObject*, uintptr_t);
  bool (*arrGetBool)(struct VirtualMachine*, struct ViplObject*, uintptr_t);
  uint32_t (*arrGetChar)(struct VirtualMachine*, struct ViplObject*, uintptr_t);
  const struct ViplObject *(*arrGetRef)(struct VirtualMachine*, struct ViplObject*, uintptr_t);
  void (*call)(struct VirtualMachine*, const char*);
  const struct ViplObject *(*stringNew)(struct VirtualMachine*, const char*);
  uint8_t (*stringGetChar)(struct VirtualMachine*, struct ViplObject*, uintptr_t);
  const struct ViplObject *(*strConcat)(struct VirtualMachine*, struct ViplObject*, struct ViplObject*);
} NativeWrapper;

typedef struct VirtualMachine {
    struct NativeWrapper nativeWrapper;
} VirtualMachine;
";

pub fn compile(sc: &str) -> Result<String, Box<dyn Error>> {
    // :)
    let start = Instant::now();
    let p = temp_dir();
    let mut buf = String::new();
    buf.push_str(LIBRARY);
    buf.push_str(sc);

    let ns = start.elapsed().as_nanos();

    let b = p.join(format!("vipl-srcFile-{}.c", ns));
    let cPath = b.to_str().ok_or("failed to conver .c path to str")?;
    let b1 = p.join(format!("vipl-srcFile-{}.so", ns));
    let soPath = b1.to_str().ok_or("failed to conver .so path to str")?;

    fs::write(&cPath, buf)?;

    let cmd = format!("gcc -Ofast -march=native -shared -o {} -fPIC {}", soPath, cPath);

    let e: Vec<&str> = cmd.split(" ").collect();

    let res = Command::new(e.first().unwrap())
        .args(&e[1..e.len()])
        .output()?;

    println!("{}", cmd);
    println!("ERR: {}", String::from_utf8_lossy(&res.stderr));
    println!("{}", String::from_utf8_lossy(&res.stdout));

    Ok(String::from(soPath))
}
