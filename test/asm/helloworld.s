.intel_syntax noprefix
.data
g: .quad 3
.globl _Imain_paai
.text
_Imain_paai:
	enter 336, 0
	mov byte ptr [rbp + -8], r8b
	mov qword ptr [rbp + -16], r8
	mov qword ptr [rbp + -24], r9
	mov qword ptr [rbp + -32], r10
	mov r8, qword ptr [rbp + -40]
	mov r8, rdi
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -64]
	lea r8, qword ptr g[rip]
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -72]
	mov r8, 5
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, qword ptr [rbp + -72]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -80]
	mov r8, 104
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r8, qword ptr [rbp + -88]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, qword ptr [rbp + -96]
	mov r8, rax
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -104]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -120]
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -128]
	mov r8, 12
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -120]
	mov r9, qword ptr [rbp + -128]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 8]
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -144]
	mov r8, 72
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -144]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 16]
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -160]
	mov r8, 101
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -160]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 24]
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -176]
	mov r8, 108
	mov qword ptr [rbp + -176], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -176]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 32]
	mov qword ptr [rbp + -184], r8
	mov r8, qword ptr [rbp + -192]
	mov r8, 108
	mov qword ptr [rbp + -192], r8
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -192]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -200]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 40]
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -208]
	mov r8, 111
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -200]
	mov r9, qword ptr [rbp + -208]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -216]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 48]
	mov qword ptr [rbp + -216], r8
	mov r8, qword ptr [rbp + -224]
	mov r8, 32
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, qword ptr [rbp + -224]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 56]
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -240]
	mov r8, 87
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -240]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -248]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 64]
	mov qword ptr [rbp + -248], r8
	mov r8, qword ptr [rbp + -256]
	mov r8, 111
	mov qword ptr [rbp + -256], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, qword ptr [rbp + -256]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -264]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 72]
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -272]
	mov r8, 114
	mov qword ptr [rbp + -272], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, qword ptr [rbp + -272]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -280]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 80]
	mov qword ptr [rbp + -280], r8
	mov r8, qword ptr [rbp + -288]
	mov r8, 108
	mov qword ptr [rbp + -288], r8
	mov r8, qword ptr [rbp + -280]
	mov r9, qword ptr [rbp + -288]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -296]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 88]
	mov qword ptr [rbp + -296], r8
	mov r8, qword ptr [rbp + -304]
	mov r8, 100
	mov qword ptr [rbp + -304], r8
	mov r8, qword ptr [rbp + -296]
	mov r9, qword ptr [rbp + -304]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -312]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 96]
	mov qword ptr [rbp + -312], r8
	mov r8, qword ptr [rbp + -320]
	mov r8, 33
	mov qword ptr [rbp + -320], r8
	mov r8, qword ptr [rbp + -312]
	mov r9, qword ptr [rbp + -320]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -328]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + 8]
	mov qword ptr [rbp + -328], r8
	mov r8, qword ptr [rbp + -336]
	mov r9, qword ptr [rbp + -328]
	mov r8, r9
	mov qword ptr [rbp + -336], r8
	mov r8, qword ptr [rbp + -336]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
