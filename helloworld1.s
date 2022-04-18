.intel_syntax noprefix
.data
g0: .quad 12, 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33
g: .quad 3
.globl _Imain_paai
.text
_Imain_paai:
	enter 88, 0
	mov r8, qword ptr [rbp + -8]
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -16]
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r8, qword ptr [rbp + -24]
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r8, qword ptr [rbp + -32]
	mov r8, qword ptr g[rip]
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -40]
	mov r8, 5
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, qword ptr [rbp + -40]
	mov qword ptr [r8], r9
	/*
	mov r8, qword ptr [rbp + -48]
	mov r8, qword ptr g0[rip]
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -64]
	mov r8, 8
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, qword ptr [rbp + -56]
	mov r10, qword ptr [rbp + -64]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -80]
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r8, qword ptr [rbp + -88]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	*/
	leave
	ret
