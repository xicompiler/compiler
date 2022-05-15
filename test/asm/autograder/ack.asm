.intel_syntax noprefix
.data
.globl _Iusage_p, _Imain_paai, _IAck_iii
.text
_Iusage_p:
	and rsp, -16
	mov _t6, 240
	mov rdi, _t6
	call _xi_alloc
	mov _RV1, rax
	mov _t38, _RV1
	mov _t5, _t38
	mov _t0, _t5
	mov qword ptr [_t0], 29
	mov qword ptr [_t0 + 8], 80
	mov qword ptr [_t0 + 16], 108
	mov qword ptr [_t0 + 24], 101
	mov qword ptr [_t0 + 32], 97
	mov qword ptr [_t0 + 40], 115
	mov qword ptr [_t0 + 48], 101
	mov qword ptr [_t0 + 56], 32
	mov qword ptr [_t0 + 64], 115
	mov qword ptr [_t0 + 72], 112
	mov qword ptr [_t0 + 80], 101
	mov qword ptr [_t0 + 88], 99
	mov qword ptr [_t0 + 96], 105
	mov qword ptr [_t0 + 104], 102
	mov qword ptr [_t0 + 112], 121
	mov qword ptr [_t0 + 120], 32
	mov qword ptr [_t0 + 128], 116
	mov qword ptr [_t0 + 136], 104
	mov qword ptr [_t0 + 144], 101
	mov qword ptr [_t0 + 152], 32
	mov qword ptr [_t0 + 160], 105
	mov qword ptr [_t0 + 168], 110
	mov qword ptr [_t0 + 176], 112
	mov qword ptr [_t0 + 184], 117
	mov qword ptr [_t0 + 192], 116
	mov qword ptr [_t0 + 200], 32
	mov qword ptr [_t0 + 208], 115
	mov qword ptr [_t0 + 216], 105
	mov qword ptr [_t0 + 224], 122
	mov qword ptr [_t0 + 232], 101
	lea _t39, qword ptr [_t0 + 8]
	mov _t4, _t39
	mov rdi, _t4
	call _Iprintln_pai
	leave
	ret
_Imain_paai:
	and rsp, -16
	mov _ARG1, rdi
	mov _t41, _ARG1
	mov args, _t41
	mov n, 11
	mov _t8, 2
	mov _t9, n
	mov rsi, _t9
	mov rdi, _t8
	call _IAck_iii
	mov _RV1, rax
	mov _t42, _RV1
	mov _t7, _t42
	mov r, _t7
	mov _t12, 56
	mov rdi, _t12
	call _xi_alloc
	mov _RV1, rax
	mov _t43, _RV1
	mov _t11, _t43
	mov _t1, _t11
	mov qword ptr [_t1], 6
	mov qword ptr [_t1 + 8], 65
	mov qword ptr [_t1 + 16], 99
	mov qword ptr [_t1 + 24], 107
	mov qword ptr [_t1 + 32], 40
	mov qword ptr [_t1 + 40], 50
	mov qword ptr [_t1 + 48], 44
	lea _t44, qword ptr [_t1 + 8]
	mov _t10, _t44
	mov rdi, _t10
	call _Iprint_pai
	mov _t15, n
	mov rdi, _t15
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t45, _RV1
	mov _t14, _t45
	mov _t13, _t14
	mov rdi, _t13
	call _Iprint_pai
	mov _t18, 32
	mov rdi, _t18
	call _xi_alloc
	mov _RV1, rax
	mov _t46, _RV1
	mov _t17, _t46
	mov _t2, _t17
	mov qword ptr [_t2], 3
	mov qword ptr [_t2 + 8], 41
	mov qword ptr [_t2 + 16], 58
	mov qword ptr [_t2 + 24], 32
	lea _t47, qword ptr [_t2 + 8]
	mov _t16, _t47
	mov rdi, _t16
	call _Iprint_pai
	mov _t21, r
	mov rdi, _t21
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t48, _RV1
	mov _t20, _t48
	mov _t19, _t20
	mov rdi, _t19
	call _Iprint_pai
	mov _t24, 8
	mov rdi, _t24
	call _xi_alloc
	mov _RV1, rax
	mov _t49, _RV1
	mov _t23, _t49
	mov _t3, _t23
	mov qword ptr [_t3], 0
	lea _t50, qword ptr [_t3 + 8]
	mov _t22, _t50
	mov rdi, _t22
	call _Iprintln_pai
	leave
	ret
_IAck_iii:
	and rsp, -16
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t52, _ARG1
	mov m, _t52
	mov _t53, _ARG2
	mov n, _t53
	mov _t54, 0
	cmp m, _t54
	jne _l1
	_l2:
	lea _t55, qword ptr [n + 1]
	mov _t25, _t55
	mov rax, _t25
	leave
	ret
	_l1:
	mov _t56, 0
	cmp n, _t56
	jne _l4
	_l5:
	mov _t57, 1
	mov _t58, m
	sub _t58, _t57
	mov _t28, _t58
	mov _t29, 1
	mov rsi, _t29
	mov rdi, _t28
	call _IAck_iii
	mov _RV1, rax
	mov _t59, _RV1
	mov _t27, _t59
	mov _t26, _t27
	mov rax, _t26
	leave
	ret
	_l4:
	mov _t60, 1
	mov _t61, m
	sub _t61, _t60
	mov _t32, _t61
	mov _t35, m
	mov _t62, 1
	mov _t63, n
	sub _t63, _t62
	mov _t36, _t63
	mov rsi, _t36
	mov rdi, _t35
	call _IAck_iii
	mov _RV1, rax
	mov _t64, _RV1
	mov _t34, _t64
	mov _t33, _t34
	mov rsi, _t33
	mov rdi, _t32
	call _IAck_iii
	mov _RV1, rax
	mov _t65, _RV1
	mov _t31, _t65
	mov _t30, _t31
	mov rax, _t30
	leave
	ret
