BITS 64
DEFAULT REL

section .text
mov r15, rdi
mov r14, rsi
mov rbx, [r14+0]

; pushLocal 0
mov r10, [rbx+0]
push r10

push 0
pop r12
pop r13
cmp r12, r13
jne JMP0

; pushLocal 0
mov r10, [rbx+0]
push r10


; pushLocal 0
mov r10, [rbx+0]
push r10

push 1
pop r12
pop r13
sub r12, r13
push r12

; asmCall fact(int)
mov rdi, r15
mov rsi, str0
mov rdx, rsp
mov r10, [r15+176]
call r10
add rsp, rax

pop r12
pop r13
imul r12, r13
push r12
JMP0:
ret
push 1
ret
ret
push 0
JMP2:

; setLocal 0
pop r10
mov [rbx+0], r10


; pushLocal 0
mov r10, [rbx+0]
push r10

push 10000000
pop r12
pop r13
cmp r12, r13
jne JMP1
push 20

; asmCall fact(int)
mov rdi, r15
mov rsi, str1
mov rdx, rsp
mov r10, [r15+176]
call r10
add rsp, rax

pop r12

; pushLocal 0
mov r10, [rbx+0]
push r10

push 1
pop r12
pop r13
add r12, r13
push r12

; setLocal 0
pop r10
mov [rbx+0], r10

JMP1:
jmp JMP2

section .rodata
str0: db "fact(int)", 0
str1: db "fact(int)", 0