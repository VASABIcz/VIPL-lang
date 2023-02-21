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
  void( * call)(struct VirtualMachine * ,
    const char * );
  const struct ViplObject * ( * stringNew)(struct VirtualMachine * ,
    const char * );
  uint8_t( * stringGetChar)(struct VirtualMachine * , struct ViplObject * , uintptr_t);
}
NativeWrapper;

typedef struct VirtualMachine {
  struct NativeWrapper nativeWrapper;
}
VirtualMachine;
void call(VirtualMachine * vm, StackFrame * frame) {
  ViplObject * path = vm -> nativeWrapper.getLocalsRef(frame, 0);
  ViplObject * files;
  long i;
  long len;
  long counter;
  ViplObject * buf;
  long ft;
  files = ({
    vm -> nativeWrapper.pushRef(vm, path);vm -> nativeWrapper.call(vm, "ls(String)");vm -> nativeWrapper.popRef(vm);
  });
  i = 0;
  len = ({
    vm -> nativeWrapper.pushRef(vm, files);vm -> nativeWrapper.call(vm, "arrayLen(Array)");vm -> nativeWrapper.popInt(vm);
  });
  counter = 0;
  while (i < len) {
    buf = path + vm -> nativeWrapper.stringNew(vm, "/") + NULL;
    ft = ({
      vm -> nativeWrapper.pushRef(vm, buf);vm -> nativeWrapper.call(vm, "fileType(String)");vm -> nativeWrapper.popInt(vm);
    });
    if (ft == 2) {
      counter += ({
        vm -> nativeWrapper.pushRef(vm, buf);vm -> nativeWrapper.call(vm, "countDir(String)");vm -> nativeWrapper.popInt(vm);
      });
    } else {
      if (ft == 1) {
        counter += ({
          vm -> nativeWrapper.pushRef(vm, buf);vm -> nativeWrapper.call(vm, "countFile(String)");vm -> nativeWrapper.popInt(vm);
        });
      }
    }
    i += 1;
  }
  vm -> nativeWrapper.pushInt(vm, counter);
  return;
}