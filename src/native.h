#include <stdbool.h>
#include <stdint.h>

#ifndef VIPLNATIVE_LIBRARY_H
#define VIPLNATIVE_LIBRARY_H

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
    void (*call)(struct VirtualMachine*, const char*);
    const struct ViplObject *(*stringNew)(struct VirtualMachine*, const char*);
    uint8_t (*stringGetChar)(struct VirtualMachine*, struct ViplObject*, uintptr_t);
} NativeWrapper;


typedef struct VirtualMachine {
    struct NativeWrapper nativeWrapper;
} VirtualMachine;

void call(VirtualMachine* vm, StackFrame* frame);

#endif //VIPLNATIVE_LIBRARY_H
