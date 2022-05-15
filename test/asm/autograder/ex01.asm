.intel_syntax noprefix
.data
.globl _Ifoo_p, _Imain_paai
.text
_Ifoo_p:
	and rsp, -16
	leave
	ret
_Imain_paai:
	and rsp, -16
	mov _ARG1, rdi
	mov _t6, _ARG1
	mov args, _t6
	mov _t3, 48
	mov rdi, _t3
	call _xi_alloc
	mov _RV1, rax
	mov _t7, _RV1
	mov _t2, _t7
	mov _t0, _t2
	mov qword ptr [_t0], 5
	mov qword ptr [_t0 + 8], 104
	mov qword ptr [_t0 + 16], 101
	mov qword ptr [_t0 + 24], 108
	mov qword ptr [_t0 + 32], 108
	mov qword ptr [_t0 + 40], 111
	lea _t8, qword ptr [_t0 + 8]
	mov _t1, _t8
	mov rdi, _t1
	call _Iprintln_pai
	leave
	ret
