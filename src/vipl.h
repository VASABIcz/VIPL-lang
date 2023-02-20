

#ifndef VIPL_H
#define VIPL_H

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct HashMap_MyStr__Func HashMap_MyStr__Func;

typedef struct HashMap_MyStr__ObjectDefinition HashMap_MyStr__ObjectDefinition;

typedef struct ObjectMeta ObjectMeta;

typedef struct Option_Rc_ViplObject Option_Rc_ViplObject;

typedef struct Option_Value Option_Value;

typedef struct StackFrame StackFrame;

typedef struct Vec_OpCode Vec_OpCode;

typedef struct Vec_Option_CachedOpCode Vec_Option_CachedOpCode;

typedef struct Vec_Value Vec_Value;

typedef struct ViplObject ViplObject;

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
  void (*call)(struct VirtualMachine*, const char*);
  const struct ViplObject *(*stringNew)(struct VirtualMachine*, const char*);
  uint8_t (*stringGetChar)(struct VirtualMachine*, struct ViplObject*, uintptr_t);
} NativeWrapper;

typedef struct VirtualMachine {
  struct NativeWrapper nativeWrapper;
  struct HashMap_MyStr__Func functions;
  struct HashMap_MyStr__ObjectDefinition classes;
  struct Vec_Value stack;
  struct Vec_OpCode opCodes;
  struct Vec_Option_CachedOpCode opCodeCache;
} VirtualMachine;

typedef enum Value_Tag {
  Num,
  Flo,
  Bol,
  Chr,
  Reference,
} Value_Tag;

typedef struct Reference_Body {
  struct Option_Rc_ViplObject instance;
} Reference_Body;

typedef struct Value {
  Value_Tag tag;
  union {
    struct {
      intptr_t num;
    };
    struct {
      float flo;
    };
    struct {
      bool bol;
    };
    struct {
      uint32_t chr;
    };
    Reference_Body reference;
  };
} Value;

typedef enum DataType_Tag {
  Int,
  Float,
  Bool,
  Char,
  Object,
} DataType_Tag;

typedef struct DataType {
  DataType_Tag tag;
  union {
    struct {
      struct ObjectMeta object;
    };
  };
} DataType;

struct VirtualMachine *createVm(void);

void pushStack(struct VirtualMachine *vm, struct Value *value);

void registerNative(struct VirtualMachine *vm,
                    const uint8_t *name,
                    uintptr_t nameLen,
                    const struct DataType *args,
                    uintptr_t argsLen,
                    void (*callback)(struct VirtualMachine*, struct StackFrame*));

struct Option_Value popStack(struct VirtualMachine *vm);

void evaluate(struct VirtualMachine *vm, const uint8_t *d, uintptr_t len);

void test(struct VirtualMachine *vm);

void dropVm(struct VirtualMachine *ptr);

void pushInt(struct VirtualMachine *vm, intptr_t v);

void pushFloat(struct VirtualMachine *vm, float v);

void pushChar(struct VirtualMachine *vm, uint8_t v);

void pushBool(struct VirtualMachine *vm, bool v);

void pushRef(struct VirtualMachine *vm, const struct ViplObject *v);

intptr_t popInt(struct VirtualMachine *vm);

float popFloat(struct VirtualMachine *vm);

uint8_t popChar(struct VirtualMachine *vm);

bool popBool(struct VirtualMachine *vm);

const struct ViplObject *popRef(struct VirtualMachine *vm);

intptr_t getLocalsInt(struct StackFrame *vm, uintptr_t index);

float getLocalsFloat(struct StackFrame *vm, uintptr_t index);

uint8_t getLocalsChar(struct StackFrame *vm, uintptr_t index);

bool getLocalsBool(struct StackFrame *vm, uintptr_t index);

const struct ViplObject *getLocalsRef(struct StackFrame *vm, uintptr_t index);

void call(struct VirtualMachine *vm, const char *s);

const struct ViplObject *stringNew(struct VirtualMachine *vm, const char *s);

uint8_t stringGetChar(struct VirtualMachine *vm, struct ViplObject *obj, uintptr_t index);achine *vm, const char *s);

} // extern "C"


##endif