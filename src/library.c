void call(VirtualMachine* vm, StackFrame* frame){
    long max = vm->nativeWrapper.getLocalsInt(frame, 0);
    long x;
    x=0;
    while(x < max){
        x+=1;
    }
    vm->nativeWrapper.pushInt(vm, x);
    return;
}