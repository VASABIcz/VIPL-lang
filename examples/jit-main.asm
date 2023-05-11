BITS 64
DEFAULT REL

section .text
; init code
mov r15, rdi
mov r14, rsi
mov rbx, [r14+0]

; start opcode PushIntZero
mov r13, 0
; end opcode PushIntZero

; start opcode SetLocalZero
mov [rbx+0], r13
; end opcode SetLocalZero

; start opcode GetLocalZero
JMP1:
mov r13, [rbx+0]
; end opcode GetLocalZero

; start opcode PushInt(10000000)
mov r12, 10000000
; end opcode PushInt(10000000)

; start opcode Greater(Int)
xor rax, rax
cmp r13, r12
setl al
mov r13, rax
; end opcode Greater(Int)

; start opcode Jmp { offset: 8, jmpType: False }
cmp r13, 0
jz JMP0
; end opcode Jmp { offset: 8, jmpType: False }

; start opcode PushInt(20)
mov r13, 20
; end opcode PushInt(20)

; start opcode SCall { id: 0 }
; call 4:0 -> true
push r13
mov rdi, r15
mov rsi, 0
mov rdx, 4
mov rcx, rsp
push rbx
sub rsp, 8
call [r15+64]
add rsp, 8
pop rbx
add rsp, 8
mov r12, rax
; end opcode SCall { id: 0 }

; start opcode Pop
; end opcode Pop

; start opcode GetLocalZero
mov r12, [rbx+0]
; end opcode GetLocalZero

; start opcode PushIntOne
mov r11, 1
; end opcode PushIntOne

; start opcode AddInt
add r12, r11
; end opcode AddInt

; start opcode SetLocalZero
mov [rbx+0], r12
; end opcode SetLocalZero

; start opcode Jmp { offset: -12, jmpType: Jmp }
jmp JMP1
; end opcode Jmp { offset: -12, jmpType: Jmp }

; start opcode Return
JMP0:
ret
; end opcode Return


section .rodata
