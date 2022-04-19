.intel_syntax noprefix
.data
.globl _Ibar_pii, _Imain_paai, _Ifoo_i, _Iacker_iii, _Iack_iii
.text
_Ibar_pii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t36, _ARG1
	mov x, _t36
	mov _t37, _ARG2
	mov y, _t37
	mov _t38, 32
	mov _t5, _t38
	mov rdi, _t5
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t39, _RV1
	mov _t4, _t39
	mov _t0, _t4
	mov _t40, 3
	mov qword ptr [_t0], _t40
	lea _t41, qword ptr [_t0 + 8]
	mov _t42, 98
	mov qword ptr [_t41], _t42
	lea _t43, qword ptr [_t0 + 16]
	mov _t44, 97
	mov qword ptr [_t43], _t44
	lea _t45, qword ptr [_t0 + 24]
	mov _t46, 114
	mov qword ptr [_t45], _t46
	lea _t47, qword ptr [_t0 + 8]
	mov _t3, _t47
	mov rdi, _t3
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
_Imain_paai:
	mov _ARG1, rdi
	mov _t49, _ARG1
	mov args, _t49
	mov _t50, 40
	mov _t8, _t50
	mov rdi, _t8
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t51, _RV1
	mov _t7, _t51
	mov _t1, _t7
	mov _t52, 4
	mov qword ptr [_t1], _t52
	lea _t53, qword ptr [_t1 + 8]
	mov _t54, 109
	mov qword ptr [_t53], _t54
	lea _t55, qword ptr [_t1 + 16]
	mov _t56, 97
	mov qword ptr [_t55], _t56
	lea _t57, qword ptr [_t1 + 24]
	mov _t58, 105
	mov qword ptr [_t57], _t58
	lea _t59, qword ptr [_t1 + 32]
	mov _t60, 110
	mov qword ptr [_t59], _t60
	lea _t61, qword ptr [_t1 + 8]
	mov _t6, _t61
	mov rdi, _t6
	and rsp, -16
	call _Iprintln_pai
	and rsp, -16
	call _Ifoo_i
	mov _RV1, rax
	mov _t62, _RV1
	mov _t9, _t62
	mov n, _t9
	mov _t63, 100000
	mov _t11, _t63
	mov _t64, 0
	mov _t12, _t64
	mov rsi, _t12
	mov rdi, _t11
	and rsp, -16
	call _Iack_iii
	mov _RV1, rax
	mov _t65, _RV1
	mov _t10, _t65
	mov r, _t10
	mov _t15, r
	mov rdi, _t15
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t66, _RV1
	mov _t14, _t66
	mov _t13, _t14
	mov rdi, _t13
	and rsp, -16
	call _Iprintln_pai
	mov _t67, 1
	mov _t16, _t67
	mov _t68, 2
	mov _t17, _t68
	mov rsi, _t17
	mov rdi, _t16
	and rsp, -16
	call _Ibar_pii
	leave
	ret
_Ifoo_i:
	mov _t70, 1
	mov _t18, _t70
	mov rax, _t18
	leave
	ret
_Iacker_iii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t72, _ARG1
	mov m, _t72
	mov _t73, _ARG2
	mov n, _t73
	lea _t74, qword ptr [m + n]
	mov _t19, _t74
	mov rax, _t19
	leave
	ret
_Iack_iii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t76, _ARG1
	mov m, _t76
	mov _t77, _ARG2
	mov n, _t77
	mov _t78, 0
	cmp m, _t78
	jne _l1
	_l2:
	mov _t79, 80
	mov _t22, _t79
	mov rdi, _t22
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t80, _RV1
	mov _t21, _t80
	mov _t2, _t21
	mov _t81, 9
	mov qword ptr [_t2], _t81
	lea _t82, qword ptr [_t2 + 8]
	mov _t83, 114
	mov qword ptr [_t82], _t83
	lea _t84, qword ptr [_t2 + 16]
	mov _t85, 101
	mov qword ptr [_t84], _t85
	lea _t86, qword ptr [_t2 + 24]
	mov _t87, 116
	mov qword ptr [_t86], _t87
	lea _t88, qword ptr [_t2 + 32]
	mov _t89, 117
	mov qword ptr [_t88], _t89
	lea _t90, qword ptr [_t2 + 40]
	mov _t91, 114
	mov qword ptr [_t90], _t91
	lea _t92, qword ptr [_t2 + 48]
	mov _t93, 110
	mov qword ptr [_t92], _t93
	lea _t94, qword ptr [_t2 + 56]
	mov _t95, 105
	mov qword ptr [_t94], _t95
	lea _t96, qword ptr [_t2 + 64]
	mov _t97, 110
	mov qword ptr [_t96], _t97
	lea _t98, qword ptr [_t2 + 72]
	mov _t99, 103
	mov qword ptr [_t98], _t99
	lea _t100, qword ptr [_t2 + 8]
	mov _t20, _t100
	mov rdi, _t20
	and rsp, -16
	call _Iprintln_pai
	lea _t101, qword ptr [n + 1]
	mov _t23, _t101
	mov rax, _t23
	leave
	ret
	_l1:
	mov _t102, 0
	cmp n, _t102
	jne _l4
	_l5:
	mov _t103, 1
	mov _t104, m
	sub _t104, m
	mov _t26, _t104
	mov _t105, 1
	mov _t27, _t105
	mov rsi, _t27
	mov rdi, _t26
	and rsp, -16
	call _Iack_iii
	mov _RV1, rax
	mov _t106, _RV1
	mov _t25, _t106
	mov _t24, _t25
	mov rax, _t24
	leave
	ret
	_l4:
	mov _t107, 1
	mov _t108, m
	sub _t108, m
	mov _t30, _t108
	mov _t33, m
	mov _t109, 1
	mov _t110, n
	sub _t110, n
	mov _t34, _t110
	mov rsi, _t34
	mov rdi, _t33
	and rsp, -16
	call _Iack_iii
	mov _RV1, rax
	mov _t111, _RV1
	mov _t32, _t111
	mov _t31, _t32
	mov rsi, _t31
	mov rdi, _t30
	and rsp, -16
	call _Iack_iii
	mov _RV1, rax
	mov _t112, _RV1
	mov _t29, _t112
	mov _t28, _t29
	mov rax, _t28
	leave
	ret
