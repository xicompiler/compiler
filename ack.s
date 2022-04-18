.intel_syntax noprefix
.data
_g2: .quad 9, 114, 101, 116, 117, 114, 110, 105, 110, 103
_g1: .quad 4, 109, 97, 105, 110
_g0: .quad 3, 98, 97, 114
.globl _Ibar_pii, _Imain_paai, _Ifoo_i, _Iacker_iii, _Iack_iii
.text
_Ibar_pii:
	enter 96, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	lea r8, qword ptr _g0[rip]
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r8, 8
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -64]
	mov r10, qword ptr [rbp + -72]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -96]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
_Imain_paai:
	enter 224, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	lea r8, qword ptr _g1[rip]
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r8, 8
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -40]
	mov r10, qword ptr [rbp + -48]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -72]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	and rsp, -16
	call _Ifoo_i
	mov r8, rax
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, 100000
	mov qword ptr [rbp + -112], r8
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -120], r8
	mov r8, 0
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, r9
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -136]
	mov rsi, r8
	mov r8, qword ptr [rbp + -120]
	mov rdi, r8
	and rsp, -16
	call _Iack_iii
	mov r8, rax
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -144], r8
	mov r9, qword ptr [rbp + -144]
	mov r8, r9
	mov qword ptr [rbp + -152], r8
	mov r9, qword ptr [rbp + -152]
	mov r8, r9
	mov qword ptr [rbp + -160], r8
	mov r9, qword ptr [rbp + -160]
	mov r8, r9
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -168]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -176]
	mov r8, r9
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -192], r8
	mov r8, qword ptr [rbp + -192]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	mov r8, 1
	mov qword ptr [rbp + -200], r8
	mov r9, qword ptr [rbp + -200]
	mov r8, r9
	mov qword ptr [rbp + -208], r8
	mov r8, 2
	mov qword ptr [rbp + -216], r8
	mov r9, qword ptr [rbp + -216]
	mov r8, r9
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -224]
	mov rsi, r8
	mov r8, qword ptr [rbp + -208]
	mov rdi, r8
	and rsp, -16
	call _Ibar_pii
	leave
	ret
_Ifoo_i:
	enter 16, 0
	mov r8, 1
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r8, qword ptr [rbp + -16]
	mov rax, r8
	leave
	ret
_Iacker_iii:
	enter 64, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -32]
	mov r10, qword ptr [rbp + -48]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov rax, r8
	leave
	ret
_Iack_iii:
	enter 312, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	mov r8, 0
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, qword ptr [rbp + -56]
	cmp r8, r9
	jne _l1
	_l2:
	lea r8, qword ptr _g2[rip]
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, 8
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -72]
	mov r10, qword ptr [rbp + -80]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	mov r8, 1
	mov qword ptr [rbp + -112], r8
	mov r9, qword ptr [rbp + -48]
	mov r10, qword ptr [rbp + -112]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -120]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov rax, r8
	leave
	ret
	_l1:
	mov r8, 0
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, qword ptr [rbp + -136]
	cmp r8, r9
	jne _l4
	_l5:
	mov r8, 1
	mov qword ptr [rbp + -144], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -144]
	sub r8, r9
	mov qword ptr [rbp + -152], r8
	mov r9, qword ptr [rbp + -152]
	mov r8, r9
	mov qword ptr [rbp + -160], r8
	mov r8, 1
	mov qword ptr [rbp + -168], r8
	mov r9, qword ptr [rbp + -168]
	mov r8, r9
	mov qword ptr [rbp + -176], r8
	mov r8, qword ptr [rbp + -176]
	mov rsi, r8
	mov r8, qword ptr [rbp + -160]
	mov rdi, r8
	and rsp, -16
	call _Iack_iii
	mov r8, rax
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -192], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -200], r8
	mov r9, qword ptr [rbp + -200]
	mov r8, r9
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov rax, r8
	leave
	ret
	_l4:
	mov r8, 1
	mov qword ptr [rbp + -216], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -224]
	mov r9, qword ptr [rbp + -216]
	sub r8, r9
	mov qword ptr [rbp + -224], r8
	mov r9, qword ptr [rbp + -224]
	mov r8, r9
	mov qword ptr [rbp + -232], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -240], r8
	mov r8, 1
	mov qword ptr [rbp + -248], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -256], r8
	mov r8, qword ptr [rbp + -256]
	mov r9, qword ptr [rbp + -248]
	sub r8, r9
	mov qword ptr [rbp + -256], r8
	mov r9, qword ptr [rbp + -256]
	mov r8, r9
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov rsi, r8
	mov r8, qword ptr [rbp + -240]
	mov rdi, r8
	and rsp, -16
	call _Iack_iii
	mov r8, rax
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -272], r8
	mov r9, qword ptr [rbp + -272]
	mov r8, r9
	mov qword ptr [rbp + -280], r8
	mov r9, qword ptr [rbp + -280]
	mov r8, r9
	mov qword ptr [rbp + -288], r8
	mov r8, qword ptr [rbp + -288]
	mov rsi, r8
	mov r8, qword ptr [rbp + -232]
	mov rdi, r8
	and rsp, -16
	call _Iack_iii
	mov r8, rax
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -296], r8
	mov r9, qword ptr [rbp + -296]
	mov r8, r9
	mov qword ptr [rbp + -304], r8
	mov r9, qword ptr [rbp + -304]
	mov r8, r9
	mov qword ptr [rbp + -312], r8
	mov r8, qword ptr [rbp + -312]
	mov rax, r8
	leave
	ret
