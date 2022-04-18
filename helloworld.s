.intel_syntax noprefix
.data
i: .quad 5
.globl _Imain_paai
.text
_Imain_paai:
	enter 64, 0
	mov r8, rdi
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov r8, qword ptr [rbp + -24]
	lea r8, qword ptr i[rip]
	mov qword ptr [rbp + -24], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, qword ptr [r9]
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov r8, qword ptr [rbp + -32]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov r8, qword ptr [rbp + -64]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
