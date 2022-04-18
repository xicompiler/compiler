.intel_syntax noprefix
.data
_g2: .quad 9, 114, 101, 116, 117, 114, 110, 105, 110, 103
_g1: .quad 4, 109, 97, 105, 110
_g0: .quad 3, 98, 97, 114
.globl _Ibar_pii, _Imain_paai, _Ifoo_i, _Iacker_iii, _Iack_iii
.text
_Ibar_pii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t33, _ARG1
	mov x, _t33
	mov _t34, _ARG2
	mov y, _t34
	lea _t35, qword ptr _g0[rip]
	mov _t0, _t35
	mov _t36, 8
	lea _t37, qword ptr [_t0 + _t36]
	mov _t1, _t37
	mov _t6, _t1
	mov rdi, _t6
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
_Imain_paai:
	mov _ARG1, rdi
	mov _t39, _ARG1
	mov args, _t39
	lea _t40, qword ptr _g1[rip]
	mov _t2, _t40
	mov _t41, 8
	lea _t42, qword ptr [_t2 + _t41]
	mov _t3, _t42
	mov _t7, _t3
	mov rdi, _t7
	and rsp, -16
	call _Iprintln_pai
	and rsp, -16
	call _Ifoo_i
	mov _RV1, rax
	mov _t43, _RV1
	mov _t8, _t43
	mov n, _t8
	mov _t44, 100000
	mov _t10, _t44
	mov _t45, 0
	mov _t11, _t45
	mov rsi, _t11
	mov rdi, _t10
	and rsp, -16
	call _Iack_iii
	mov _RV1, rax
	mov _t46, _RV1
	mov _t9, _t46
	mov r, _t9
	mov _t14, r
	mov rdi, _t14
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t47, _RV1
	mov _t13, _t47
	mov _t12, _t13
	mov rdi, _t12
	and rsp, -16
	call _Iprintln_pai
	mov _t48, 1
	mov _t15, _t48
	mov _t49, 2
	mov _t16, _t49
	mov rsi, _t16
	mov rdi, _t15
	and rsp, -16
	call _Ibar_pii
	leave
	ret
_Ifoo_i:
	mov _t51, 1
	mov _t17, _t51
	mov rax, _t17
	leave
	ret
_Iacker_iii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t53, _ARG1
	mov m, _t53
	mov _t54, _ARG2
	mov n, _t54
	lea _t55, qword ptr [m + n]
	mov _t18, _t55
	mov rax, _t18
	leave
	ret
_Iack_iii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t57, _ARG1
	mov m, _t57
	mov _t58, _ARG2
	mov n, _t58
	mov _t59, 0
	cmp m, _t59
	jne _l1
	_l2:
	lea _t60, qword ptr _g2[rip]
	mov _t4, _t60
	mov _t61, 8
	lea _t62, qword ptr [_t4 + _t61]
	mov _t5, _t62
	mov _t19, _t5
	mov rdi, _t19
	and rsp, -16
	call _Iprintln_pai
	mov _t63, 1
	lea _t64, qword ptr [n + _t63]
	mov _t20, _t64
	mov rax, _t20
	leave
	ret
	_l1:
	mov _t65, 0
	cmp n, _t65
	jne _l4
	_l5:
	mov _t66, 1
	mov _t67, m
	sub _t67, _t66
	mov _t23, _t67
	mov _t68, 1
	mov _t24, _t68
	mov rsi, _t24
	mov rdi, _t23
	and rsp, -16
	call _Iack_iii
	mov _RV1, rax
	mov _t69, _RV1
	mov _t22, _t69
	mov _t21, _t22
	mov rax, _t21
	leave
	ret
	_l4:
	mov _t70, 1
	mov _t71, m
	sub _t71, _t70
	mov _t27, _t71
	mov _t30, m
	mov _t72, 1
	mov _t73, n
	sub _t73, _t72
	mov _t31, _t73
	mov rsi, _t31
	mov rdi, _t30
	and rsp, -16
	call _Iack_iii
	mov _RV1, rax
	mov _t74, _RV1
	mov _t29, _t74
	mov _t28, _t29
	mov rsi, _t28
	mov rdi, _t27
	and rsp, -16
	call _Iack_iii
	mov _RV1, rax
	mov _t75, _RV1
	mov _t26, _t75
	mov _t25, _t26
	mov rax, _t25
	leave
	ret
