.intel_syntax noprefix
.data
i: .quad 5
.globl _Imain_paai
.text
_Imain_paai:
	mov _ARG1, rdi
	mov t3, _ARG1
	mov args, t3
	lea t4, qword ptr i[rip]
	mov t4, qword ptr [t4]
	mov t2, t4
	mov rdi, t2
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov t5, _RV1
	mov t1, t5
	mov t0, t1
	mov rdi, t0
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
