

#ifndef VIPL_H
#define VIPL_H

#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>

struct CachedOpCode;

struct Func;

template<typename K = void, typename V = void, typename Hasher = void>
struct HashMap;

struct MyStr;

struct ObjectDefinition;

struct ObjectMeta;

struct OpCode;

template<typename T = void>
struct Option;

template<typename T = void>
struct Rc;

struct StackFrame;

template<typename T = void>
struct Vec;

struct ViplObject;

struct Value {
  enum class Tag {
    Num,
    Flo,
    Bol,
    Chr,
    Reference,
  };

  struct Num_Body {
    intptr_t _0;
  };

  struct Flo_Body {
    float _0;
  };

  struct Bol_Body {
    bool _0;
  };

  struct Chr_Body {
    uint32_t _0;
  };

  struct Reference_Body {
    Option<Rc<ViplObject>> instance;
  };

  Tag tag;
  union {
    Num_Body num;
    Flo_Body flo;
    Bol_Body bol;
    Chr_Body chr;
    Reference_Body reference;
  };
};

struct NativeWrapper {
  void (*pushInt)(VirtualMachine*, intptr_t);
  void (*pushFloat)(VirtualMachine*, float);
  void (*pushBool)(VirtualMachine*, bool);
  void (*pushChar)(VirtualMachine*, uint8_t);
  void (*pushRef)(VirtualMachine*, const ViplObject*);
  intptr_t (*popInt)(VirtualMachine*);
  float (*popFloat)(VirtualMachine*);
  bool (*popBool)(VirtualMachine*);
  uint8_t (*popChar)(VirtualMachine*);
  const ViplObject *(*popRef)(VirtualMachine*);
  void (*call)(VirtualMachine*, const char*);
};

struct VirtualMachine {
  HashMap<MyStr, Func> functions;
  HashMap<MyStr, ObjectDefinition> classes;
  Vec<Value> stack;
  Vec<OpCode> opCodes;
  Vec<Option<CachedOpCode>> opCodeCache;
  NativeWrapper nativeWrapper;
};

struct DataType {
  enum class Tag {
    Int,
    Float,
    Bool,
    Char,
    Object,
  };

  struct Object_Body {
    ObjectMeta _0;
  };

  Tag tag;
  union {
    Object_Body object;
  };
};

extern "C" {

VirtualMachine *createVm();

void pushStack(VirtualMachine *vm, Value *value);

void registerNative(VirtualMachine *vm,
                    const uint8_t *name,
                    uintptr_t nameLen,
                    const DataType *args,
                    uintptr_t argsLen,
                    void (*callback)(VirtualMachine*, StackFrame*));

Option<Value> popStack(VirtualMachine *vm);

void evaluate(VirtualMachine *vm, const uint8_t *d, uintptr_t len);

void test(VirtualMachine *vm);

void dropVm(VirtualMachine *ptr);

void pushInt(VirtualMachine *vm, intptr_t v);

void pushFloat(VirtualMachine *vm, float v);

void pushChar(VirtualMachine *vm, uint8_t v);

void pushBool(VirtualMachine *vm, bool v);

void pushRef(VirtualMachine *vm, const ViplObject *v);

intptr_t popInt(VirtualMachine *vm);

float popFloat(VirtualMachine *vm);

uint8_t popChar(VirtualMachine *vm);

bool popBool(VirtualMachine *vm);

const ViplObject *popRef(VirtualMachine *vm);

void call(VirtualMachine *vm, const char *s);

} // extern "C"


##endif