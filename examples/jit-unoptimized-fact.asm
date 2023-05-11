BITS 64
DEFAULT REL

section .text
; init code
mov r15, rdi
mov r14, rsi
mov rbx, [r14+0]

; start opcode GetLocalZero
; getlocal 0
mov r10, [rbx+0]
push r10
; end opcode GetLocalZero

; start opcode PushIntZero
push 0
; end opcode PushIntZero

; start opcode LessInt
pop r12
pop r13
xor rax, rax
cmp r13, r12
setg al
mov r12, rax
push r12
; end opcode LessInt

; start opcode Jmp { offset: 7, jmpType: False }
pop r12
cmp r12, 0
jz JMP0
; end opcode Jmp { offset: 7, jmpType: False }

; start opcode GetLocalZero
; getlocal 0
mov r10, [rbx+0]
push r10
; end opcode GetLocalZero

; start opcode GetLocalZero
; getlocal 0
mov r10, [rbx+0]
push r10
; end opcode GetLocalZero

; start opcode PushIntOne
push 1
; end opcode PushIntOne

; start opcode SubInt
pop r13
pop r12
sub r12, r13
push r12
; end opcode SubInt

; start opcode SCall { id: 0 }
; call 4:0 -> true
mov rdi, r15
mov rsi, 0
mov rdx, 4
mov rcx, rsp
push rbx
call [r15+64]
pop rbx
add rsp, 8
push rax
; end opcode SCall { id: 0 }

; start opcode MulInt
pop r12
pop r13
imul r12, r13
push r12
; end opcode MulInt

; start opcode Return
pop rax
ret
; end opcode Return

; start opcode PushIntOne
JMP0:
push 1
; end opcode PushIntOne

; start opcode Return
pop rax
ret
; end opcode Return

; start opcode Return
pop rax
ret
; end opcode Return


section .rodata