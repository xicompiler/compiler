.intel_syntax noprefix
.data
.globl _Igcd_iii, _Iisprime_bi, _Ilargestprime_ii, _Imain_paai
.text
_Igcd_iii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t15, _ARG1
	mov a, _t15
	mov _t16, _ARG2
	mov b, _t16
	_l2:
	mov _t17, 0
	cmp a, _t17
	je _l0
	_l1:
	cmp a, b
	jge _l4
	_l5:
	mov _t18, b
	sub _t18, a
	mov b, _t18
	_l3:
	jmp _l2
	_l4:
	mov _t19, a
	sub _t19, b
	mov a, _t19
	jmp _l3
	_l0:
	mov _t0, b
	mov rax, _t0
	leave
	ret
_Iisprime_bi:
	mov _ARG1, rdi
	mov _t21, _ARG1
	mov n, _t21
	mov _t22, 2
	mov i, _t22
	_l8:
	mov _t23, i
	imul _t23, i
	cmp _t23, n
	jg _l6
	_l7:
	mov _t2, i
	mov _t3, n
	mov rsi, _t3
	mov rdi, _t2
	and rsp, -16
	call _Igcd_iii
	mov _RV1, rax
	mov _t24, _RV1
	mov _t1, _t24
	mov _t25, 1
	cmp _t1, _t25
	je _l9
	_l10:
	mov _t26, 0
	mov _t4, _t26
	mov rax, _t4
	leave
	ret
	_l9:
	lea _t27, qword ptr [i + 1]
	mov i, _t27
	jmp _l8
	_l6:
	mov _t28, 1
	mov _t5, _t28
	mov rax, _t5
	leave
	ret
_Ilargestprime_ii:
	mov _ARG1, rdi
	mov _t30, _ARG1
	mov max, _t30
	mov _t31, 1
	mov a, _t31
	mov _t32, 1
	mov largest, _t32
	_l13:
	cmp a, max
	jge _l11
	_l12:
	mov _t7, a
	mov rdi, _t7
	and rsp, -16
	call _Iisprime_bi
	mov _RV1, rax
	mov _t33, _RV1
	mov _t6, _t33
	mov _t34, 1
	mov _t35, _t6
	xor _t35, _t34
	test _t35, _t35
	jnz _l14
	_l15:
	mov largest, a
	_l14:
	lea _t36, qword ptr [a + 1]
	mov a, _t36
	jmp _l13
	_l11:
	mov _t8, largest
	mov rax, _t8
	leave
	ret
_Imain_paai:
	mov _ARG1, rdi
	mov _t38, _ARG1
	mov args, _t38
	mov _t39, 30
	mov _t13, _t39
	mov rdi, _t13
	and rsp, -16
	call _Ilargestprime_ii
	mov _RV1, rax
	mov _t40, _RV1
	mov _t12, _t40
	mov _t11, _t12
	mov rdi, _t11
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t41, _RV1
	mov _t10, _t41
	mov _t9, _t10
	mov rdi, _t9
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
