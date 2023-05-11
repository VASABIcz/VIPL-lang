BITS 64
DEFAULT REL

section .text
; init code
mov r15, rdi
mov r14, rsi
mov rbx, [r14+0]

; start opcode GetLocalZero
mov r13, [rbx+0]
; end opcode GetLocalZero

; start opcode PushIntZero
mov r12, 0
; end opcode PushIntZero

; start opcode LessInt
xor rax, rax
cmp r13, r12
setg al
mov r13, rax
; end opcode LessInt

; start opcode Jmp { offset: 7, jmpType: False }
cmp r13, 0
jz JMP0
; end opcode Jmp { offset: 7, jmpType: False }

; start opcode GetLocalZero
mov r13, [rbx+0]
; end opcode GetLocalZero

; start opcode GetLocalZero
mov r12, [rbx+0]
; end opcode GetLocalZero

; start opcode PushIntOne
mov r11, 1
; end opcode PushIntOne

; start opcode SubInt
sub r12, r11
; end opcode SubInt

; start opcode SCall { id: 0 }
; call 4:0 -> true
push r12
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
mov r11, rax
; end opcode SCall { id: 0 }

; start opcode MulInt
imul r13, r11
; end opcode MulInt

; start opcode Return
mov rax, r13
ret
; end opcode Return

; start opcode PushIntOne
JMP0:
mov r11, 1
; end opcode PushIntOne

; start opcode Return
mov rax, r11
ret
; end opcode Return

; start opcode Return

section .rodata
