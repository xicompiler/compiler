.intel_syntax noprefix
.data
g0: .quad 5, 104, 101, 108, 108, 111
.globl _Imain_paai
.text
_Imain_paai:
	enter 200, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	lea r8, qword ptr g0[rip]
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r8, 8
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -40]
	mov r10, qword ptr [rbp + -48]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, 1
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r8, 8
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -96]
	sub r8, r9
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -104]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, qword ptr [rbp + -104]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -112], r8
	mov r8, 1
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, qword ptr [rbp + -120]
	xor r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, qword ptr [rbp + -128]
	test r8, r9
	jnz l0
	l1:
	mov r8, 8
	mov qword ptr [rbp + -136], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -144]
	mov r9, qword ptr [rbp + -136]
	imul r8, r9
	mov qword ptr [rbp + -144], r8
	mov r9, qword ptr [rbp + -72]
	mov r10, qword ptr [rbp + -144]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -152], r8
	mov r9, qword ptr [rbp + -152]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -152], r8
	mov r9, qword ptr [rbp + -152]
	mov r8, r9
	mov qword ptr [rbp + -160], r8
	mov r9, qword ptr [rbp + -160]
	mov r8, r9
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -168]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -176]
	mov r8, r9
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -192], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -200]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
	l0:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l1
