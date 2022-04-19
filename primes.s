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
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -32], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	_l2:
	mov r8, 0
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, qword ptr [rbp + -56]
	cmp r8, r9
	je _l0
	_l1:
	mov r8, qword ptr [rbp + -32]
	mov r9, qword ptr [rbp + -48]
	cmp r8, r9
	jge _l4
	_l5:
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -64]
	mov r9, qword ptr [rbp + -32]
	sub r8, r9
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	_l3:
	jmp _l2
	_l4:
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, qword ptr [rbp + -72]
	mov r9, qword ptr [rbp + -48]
	sub r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -32], r9
	jmp _l3
	_l0:
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -80]
	mov rax, r8
	leave
	ret
_Iisprime_bi:
	enter 136, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, 2
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	_l8:
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	mov r8, qword ptr [rbp + -48]
	mov r9, qword ptr [rbp + -40]
	imul r8, r9
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, qword ptr [rbp + -24]
	cmp r8, r9
	jg _l6
	_l7:
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -64]
	mov rsi, r8
	mov r8, qword ptr [rbp + -56]
	mov rdi, r8
	and rsp, -16
	call _Igcd_iii
	mov r8, rax
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, 1
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, qword ptr [rbp + -96]
	cmp r8, r9
	je _l9
	_l10:
	mov r8, 0
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, qword ptr [rbp + -112]
	mov rax, r8
	leave
	ret
	_l9:
	mov r8, qword ptr [rbp + -40]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -120]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	jmp _l8
	_l6:
	mov r8, 1
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, r8
	mov qword ptr [rbp + -136], r9
	mov r8, qword ptr [rbp + -136]
	mov rax, r8
	leave
	ret
_Ilargestprime_ii:
	enter 120, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, 1
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, 1
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	_l13:
	mov r8, qword ptr [rbp + -40]
	mov r9, qword ptr [rbp + -24]
	cmp r8, r9
	jge _l11
	_l12:
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -64]
	mov rdi, r8
	and rsp, -16
	call _Iisprime_bi
	mov r8, rax
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, 1
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -104], r9
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -96]
	xor r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -104]
	test r8, r9
	jnz _l14
	_l15:
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	_l14:
	mov r8, qword ptr [rbp + -40]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	jmp _l13
	_l11:
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -120], r9
	mov r8, qword ptr [rbp + -120]
	mov rax, r8
	leave
	ret
_Imain_paai:
	enter 96, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, 30
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, qword ptr [rbp + -40]
	mov rdi, r8
	and rsp, -16
	call _Ilargestprime_ii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, qword ptr [rbp + -72]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, qword ptr [rbp + -96]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
