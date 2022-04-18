.intel_syntax noprefix
.data
g0: .quad 5, 104, 101, 108, 108, 111
.globl _Imain_paai
.text
_Imain_paai:
	mov _ARG1, rdi
	mov t7, _ARG1
	mov args, t7
	lea t8, qword ptr g0[rip]
	mov t2, t8
	mov t9, 8
	lea t10, qword ptr [t2 + t9]
	mov t3, t10
	mov t1, t3
	mov t11, 1
	mov t0, t11
	mov t12, 8
	mov t13, t1
	sub t13, t12
	mov t13, qword ptr [t13]
	cmp t0, t13
	setb t14
	mov t15, 1
	mov t16, t14
	xor t16, t15
	test t16, t16
	jnz l0
	l1:
	mov t17, 8
	mov t18, t0
	imul t18, t17
	lea t19, qword ptr [t1 + t18]
	mov t19, qword ptr [t19]
	mov x, t19
	mov t6, x
	mov rdi, t6
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov t20, _RV1
	mov t5, t20
	mov t4, t5
	mov rdi, t4
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
	l0:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l1
