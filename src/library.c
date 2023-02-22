#include <stdbool.h>

#include <stdint.h>

typedef struct StackFrame StackFrame;
typedef struct ViplObject ViplObject;
typedef struct VirtualMachine VirtualMachine;

typedef struct NativeWrapper {
  void( * pushInt)(struct VirtualMachine * , intptr_t);
  void( * pushFloat)(struct VirtualMachine * , float);
  void( * pushBool)(struct VirtualMachine * , bool);
  void( * pushChar)(struct VirtualMachine * , uint8_t);
  void( * pushRef)(struct VirtualMachine * ,
    const struct ViplObject * );
  intptr_t( * popInt)(struct VirtualMachine * );
  float( * popFloat)(struct VirtualMachine * );
  bool( * popBool)(struct VirtualMachine * );
  uint8_t( * popChar)(struct VirtualMachine * );
  const struct ViplObject * ( * popRef)(struct VirtualMachine * );
  intptr_t( * getLocalsInt)(struct StackFrame * , uintptr_t);
  float( * getLocalsFloat)(struct StackFrame * , uintptr_t);
  bool( * getLocalsBool)(struct StackFrame * , uintptr_t);
  uint8_t( * getLocalsChar)(struct StackFrame * , uintptr_t);
  const struct ViplObject * ( * getLocalsRef)(struct StackFrame * , uintptr_t);
  intptr_t( * arrGetInt)(struct VirtualMachine * , struct ViplObject * , uintptr_t);
  float( * arrGetFloat)(struct VirtualMachine * , struct ViplObject * , uintptr_t);
  bool( * arrGetBool)(struct VirtualMachine * , struct ViplObject * , uintptr_t);
  uint32_t( * arrGetChar)(struct VirtualMachine * , struct ViplObject * , uintptr_t);
  const struct ViplObject * ( * arrGetRef)(struct VirtualMachine * , struct ViplObject * , uintptr_t);
  void( * call)(struct VirtualMachine * ,
    const char * );
  const struct ViplObject * ( * stringNew)(struct VirtualMachine * ,
    const char * );
  uint8_t( * stringGetChar)(struct VirtualMachine * , struct ViplObject * , uintptr_t);
  const struct ViplObject * ( * strConcat)(struct VirtualMachine * , struct ViplObject * , struct ViplObject * );
}
NativeWrapper;

typedef struct VirtualMachine {
  struct NativeWrapper nativeWrapper;
}
VirtualMachine;
void call(VirtualMachine * vm, StackFrame * frame) {
  ViplObject * path = vm -> nativeWrapper.getLocalsRef(frame, 0);
  ViplObject * file;
  long fileLen;
  long i;
  long counter;
  char c;
  if (!({
      vm -> nativeWrapper.pushRef(vm, path);vm -> nativeWrapper.pushRef(vm, vm -> nativeWrapper.stringNew(vm, ".rs"));vm -> nativeWrapper.call(vm, "endsWith(String, String)");vm -> nativeWrapper.popBool(vm);
    })) {
    vm -> nativeWrapper.pushInt(vm, 0);
    return;
  }
  vm -> nativeWrapper.pushRef(vm, path);
  vm -> nativeWrapper.call(vm, "print(String)");
  file = ({
    vm -> nativeWrapper.pushRef(vm, path);vm -> nativeWrapper.call(vm, "readFile(String)");vm -> nativeWrapper.popRef(vm);
  });
  fileLen = ({
    vm -> nativeWrapper.pushRef(vm, file);vm -> nativeWrapper.call(vm, "strLen(String)");vm -> nativeWrapper.popInt(vm);
  });
  i = 0;
  counter = 1;
  while (i < fileLen) {
    c = vm -> nativeWrapper.stringGetChar(vm, file, i);
    if (c == 'a') {
      counter += 1;
    }
    i += 1;
  }
  vm -> nativeWrapper.pushInt(vm, counter);
  return;
}