.intel_syntax noprefix
.data
g: .quad 3
.globl _Imain_paai
.text
_Imain_paai:
	enter 112, 0
	mov qword ptr [rbp + -8], r8
	mov qword ptr [rbp + -16], r9
	mov qword ptr [rbp + -24], r10
	and rsp, -16
	mov r8, rdi
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	lea r8, qword ptr g[rip]
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov qword ptr [r8], 5
	mov r8, 104
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov rdi, r8
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -16]
	mov r10, qword ptr [rbp + -24]
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8], 12
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 8], 72
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 16], 101
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 24], 108
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 32], 108
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 40], 111
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 48], 32
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 56], 87
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 64], 111
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 72], 114
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 80], 108
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 88], 100
	mov r8, qword ptr [rbp + -96]
	mov qword ptr [r8 + 96], 33
	mov r9, qword ptr [rbp + -96]
	lea r8, qword ptr [r9 + 8]
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -104]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov rdi, r8
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -16]
	mov r10, qword ptr [rbp + -24]
	call _Iprintln_pai
	leave
	ret
