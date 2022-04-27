.intel_syntax noprefix
.data
.globl _Imain_paai, _Ifoo_t3iiiiiiiii
.text
_Imain_paai:
	mov _ARG1, rdi
	mov _t13, _ARG1
	mov args, _t13
	mov _t14, 7
	mov _t0, _t14
	mov _t15, 34
	mov _t1, _t15
	mov _t16, 13
	mov _t2, _t16
	mov _t17, 14
	mov _t3, _t17
	mov _t18, 16
	mov _t4, _t18
	mov _t19, 19
	mov _t5, _t19
	sub rsp, 8
	mov rdi, rsp
	push _t5
	mov r9, _t4
	mov r8, _t3
	mov rcx, _t2
	mov rdx, _t1
	mov rsi, _t0
	and rsp, -16
	call _Ifoo_t3iiiiiiiii
	add rsp, 8
	mov _RV1, rax
	mov _RV2, rdx
	pop _RV3
	mov _t20, _RV1
	mov n, _t20
	mov _t21, _RV2
	mov m, _t21
	mov _t22, _RV3
	mov o, _t22
	leave
	ret
_Ifoo_t3iiiiiiiii:
	mov _t23, rdi
	mov _ARG1, rsi
	mov _ARG2, rdx
	mov _ARG3, rcx
	mov _ARG4, r8
	mov _ARG5, r9
	pop _ARG6
	mov _t24, _ARG1
	mov a, _t24
	mov _t25, _ARG2
	mov b, _t25
	mov _t26, _ARG3
	mov c, _t26
	mov _t27, _ARG4
	mov d, _t27
	mov _t28, _ARG5
	mov e, _t28
	mov _t29, _ARG6
	mov f, _t29
	mov _t8, f
	mov rdi, _t8
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t30, _RV1
	mov _t7, _t30
	mov _t6, _t7
	mov rdi, _t6
	and rsp, -16
	call _Iprintln_pai
	mov _t31, 22
	mov _t9, _t31
	mov _t32, 24
	mov _t10, _t32
	mov _t33, 3
	mov _t11, _t33
	mov rax, _t9
	mov rdx, _t10
	mov qword ptr [_t23 + 0], _t11
	leave
	ret
