.intel_syntax noprefix
.data
.globl _Igcd_iii, _Iisprime_bi, _Ilargestprime_ii, _Imain_paai
.text
_Igcd_iii:
	enter 80, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	l2:
	mov r8, 0
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, qword ptr [rbp + -56]
	cmp r8, r9
	je l0
	l1:
	mov r8, qword ptr [rbp + -32]
	mov r9, qword ptr [rbp + -48]
	cmp r8, r9
	jge l4
	l5:
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, qword ptr [rbp + -32]
	sub r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	l3:
	jmp l2
	l4:
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, qword ptr [rbp + -48]
	sub r8, r9
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -32], r8
	jmp l3
	l0:
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov rax, r8
	leave
	ret
_Iisprime_bi:
	enter 144, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r8, 2
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	l8:
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, qword ptr [rbp + -40]
	imul r8, r9
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, qword ptr [rbp + -24]
	cmp r8, r9
	jg l6
	l7:
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov rdi, r8
	mov r8, qword ptr [rbp + -56]
	mov rsi, r8
	and rsp, -16
	call _Igcd_iii
	mov r8, rax
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r8, 1
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, qword ptr [rbp + -96]
	cmp r8, r9
	je l9
	l10:
	mov r8, 0
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -104]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov rax, r8
	leave
	ret
	l9:
	mov r8, 1
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -40]
	mov r10, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	jmp l8
	l6:
	mov r8, 1
	mov qword ptr [rbp + -136], r8
	mov r9, qword ptr [rbp + -136]
	mov r8, r9
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -144]
	mov rax, r8
	leave
	ret
_Ilargestprime_ii:
	enter 128, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r8, 1
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r8, 1
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	l13:
	mov r8, qword ptr [rbp + -40]
	mov r9, qword ptr [rbp + -24]
	cmp r8, r9
	jge l11
	l12:
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov rdi, r8
	and rsp, -16
	call _Iisprime_bi
	mov r8, rax
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r8, 1
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -96]
	xor r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -104]
	test r8, r9
	jnz l14
	l15:
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	l14:
	mov r8, 1
	mov qword ptr [rbp + -112], r8
	mov r9, qword ptr [rbp + -40]
	mov r10, qword ptr [rbp + -112]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -120]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	jmp l13
	l11:
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov rax, r8
	leave
	ret
_Imain_paai:
	enter 96, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r8, 30
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -40]
	mov rdi, r8
	and rsp, -16
	call _Ilargestprime_ii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -72]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -96]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
