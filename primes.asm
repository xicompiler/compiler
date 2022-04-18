.intel_syntax noprefix
.data
.globl _Igcd_iii, _Iisprime_bi, _Ilargestprime_ii, _Imain_paai
.text
_Igcd_iii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov t14, _ARG1
	mov a, t14
	mov t15, _ARG2
	mov b, t15
	l2:
	mov t16, 0
	cmp a, t16
	je l0
	l1:
	cmp a, b
	jge l4
	l5:
	mov t17, b
	sub t17, a
	mov b, t17
	l3:
	jmp l2
	l4:
	mov t18, a
	sub t18, b
	mov a, t18
	jmp l3
	l0:
	mov t0, b
	mov rax, t0
	leave
	ret
_Iisprime_bi:
	mov _ARG1, rdi
	mov t19, _ARG1
	mov n, t19
	mov t20, 2
	mov i, t20
	l8:
	mov t21, i
	imul t21, i
	cmp t21, n
	jg l6
	l7:
	mov t2, i
	mov t3, n
	mov rdi, t3
	mov rsi, t2
	and rsp, -16
	call _Igcd_iii
	mov _RV1, rax
	mov t22, _RV1
	mov t1, t22
	mov t23, 1
	cmp t1, t23
	je l9
	l10:
	mov t24, 0
	mov t4, t24
	mov rax, t4
	leave
	ret
	l9:
	mov t25, 1
	lea t26, qword ptr [i + t25]
	mov i, t26
	jmp l8
	l6:
	mov t27, 1
	mov t5, t27
	mov rax, t5
	leave
	ret
_Ilargestprime_ii:
	mov _ARG1, rdi
	mov t28, _ARG1
	mov max, t28
	mov t29, 1
	mov a, t29
	mov t30, 1
	mov largest, t30
	l13:
	cmp a, max
	jge l11
	l12:
	mov t7, a
	mov rdi, t7
	and rsp, -16
	call _Iisprime_bi
	mov _RV1, rax
	mov t31, _RV1
	mov t6, t31
	mov t32, 1
	mov t33, t6
	xor t33, t32
	test t33, t33
	jnz l14
	l15:
	mov largest, a
	l14:
	mov t34, 1
	lea t35, qword ptr [a + t34]
	mov a, t35
	jmp l13
	l11:
	mov t8, largest
	mov rax, t8
	leave
	ret
_Imain_paai:
	mov _ARG1, rdi
	mov t36, _ARG1
	mov args, t36
	mov t37, 30
	mov t13, t37
	mov rdi, t13
	and rsp, -16
	call _Ilargestprime_ii
	mov _RV1, rax
	mov t38, _RV1
	mov t12, t38
	mov t11, t12
	mov rdi, t11
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov t39, _RV1
	mov t10, t39
	mov t9, t10
	mov rdi, t9
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
