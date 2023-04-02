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
mul r12, r13
push r12
JMP0:
ret
push 1
ret
ret

; pushStr while test
mov rdi, r15
mov rsi, r14
mov rdx, str1
mov r10, [r15+80]
call r10
push rax


; asmCall print(String)
mov rdi, r15
mov rsi, str2
mov rdx, rsp
mov r10, [r15+176]
call r10
add rsp, rax

push 0
JMP2:

; setLocal 0
pop r10
mov [rbx+0], r10


; pushLocal 0
mov r10, [rbx+0]
push r10

push 10
pop r12
pop r13
cmp r12, r13
jne JMP1

; pushLocal 0
mov r10, [rbx+0]
push r10


; asmCall print(int)
mov rdi, r15
mov rsi, str3
mov rdx, rsp
mov r10, [r15+176]
call r10
add rsp, rax


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

; pushStr recursion test
mov rdi, r15
mov rsi, r14
mov rdx, str4
mov r10, [r15+80]
call r10
push rax


; asmCall print(String)
mov rdi, r15
mov rsi, str5
mov rdx, rsp
mov r10, [r15+176]
call r10
add rsp, rax

push 11

; asmCall fact(int)
mov rdi, r15
mov rsi, str6
mov rdx, rsp
mov r10, [r15+176]
call r10
add rsp, rax


; asmCall print(int)
mov rdi, r15
mov rsi, str7
mov rdx, rsp
mov r10, [r15+176]
call r10
add rsp, rax


section .rodata
str0: db "fact(int)", 0
str1: db "while test", 0
str2: db "print(String)", 0
str3: db "print(int)", 0
str4: db "recursion test", 0
str5: db "print(String)", 0
str6: db "fact(int)", 0
str7: db "print(int)", 0