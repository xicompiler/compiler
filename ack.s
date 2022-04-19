.intel_syntax noprefix
.data
.globl _Ibar_pii, _Imain_paai, _Ifoo_i, _Iacker_iii, _Iack_iii
.text
_Ibar_pii:
	enter 168, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -32], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	mov r8, 32
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -64]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, 3
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -96]
	mov r9, qword ptr [rbp + -104]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -96]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -112], r8
	mov r8, 98
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -120]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -96]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -128], r8
	mov r8, 97
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, qword ptr [rbp + -136]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -96]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -144], r8
	mov r8, 114
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -144]
	mov r9, qword ptr [rbp + -152]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -96]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -168], r9
	mov r8, qword ptr [rbp + -168]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
_Imain_paai:
	enter 304, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, 40
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, qword ptr [rbp + -40]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, 4
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, qword ptr [rbp + -80]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -88], r8
	mov r8, 109
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, qword ptr [rbp + -96]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -104], r8
	mov r8, 97
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -112]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -120], r8
	mov r8, 105
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -120]
	mov r9, qword ptr [rbp + -128]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -136], r8
	mov r8, 110
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -144]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, r8
	mov qword ptr [rbp + -160], r9
	mov r8, qword ptr [rbp + -160]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	and rsp, -16
	call _Ifoo_i
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -168], r9
	mov r8, qword ptr [rbp + -168]
	mov r9, r8
	mov qword ptr [rbp + -176], r9
	mov r8, qword ptr [rbp + -176]
	mov r9, r8
	mov qword ptr [rbp + -184], r9
	mov r8, 100000
	mov qword ptr [rbp + -192], r8
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -200], r9
	mov r8, 0
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, r8
	mov qword ptr [rbp + -216], r9
	mov r8, qword ptr [rbp + -216]
	mov rsi, r8
	mov r8, qword ptr [rbp + -200]
	mov rdi, r8
	and rsp, -16
	call _Iack_iii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -224], r9
	mov r8, qword ptr [rbp + -224]
	mov r9, r8
	mov qword ptr [rbp + -232], r9
	mov r8, qword ptr [rbp + -232]
	mov r9, r8
	mov qword ptr [rbp + -240], r9
	mov r8, qword ptr [rbp + -240]
	mov r9, r8
	mov qword ptr [rbp + -248], r9
	mov r8, qword ptr [rbp + -248]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	mov r8, qword ptr [rbp + -256]
	mov r9, r8
	mov qword ptr [rbp + -264], r9
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -272], r9
	mov r8, qword ptr [rbp + -272]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	mov r8, 1
	mov qword ptr [rbp + -280], r8
	mov r8, qword ptr [rbp + -280]
	mov r9, r8
	mov qword ptr [rbp + -288], r9
	mov r8, 2
	mov qword ptr [rbp + -296], r8
	mov r8, qword ptr [rbp + -296]
	mov r9, r8
	mov qword ptr [rbp + -304], r9
	mov r8, qword ptr [rbp + -304]
	mov rsi, r8
	mov r8, qword ptr [rbp + -288]
	mov rdi, r8
	and rsp, -16
	call _Ibar_pii
	leave
	ret
_Ifoo_i:
	enter 16, 0
	mov r8, 1
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
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
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -32], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	mov r8, qword ptr [rbp + -32]
	mov r9, qword ptr [rbp + -48]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -64]
	mov rax, r8
	leave
	ret
_Iack_iii:
	enter 464, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -32], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	mov r8, 0
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, qword ptr [rbp + -56]
	cmp r8, r9
	jne _l1
	_l2:
	mov r8, 80
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, qword ptr [rbp + -72]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, qword ptr [rbp + -96]
	mov r9, r8
	mov qword ptr [rbp + -104], r9
	mov r8, 9
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -112]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -120], r8
	mov r8, 114
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -120]
	mov r9, qword ptr [rbp + -128]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -136], r8
	mov r8, 101
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -144]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -152], r8
	mov r8, 116
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -160]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -168], r8
	mov r8, 117
	mov qword ptr [rbp + -176], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -176]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -184], r8
	mov r8, 114
	mov qword ptr [rbp + -192], r8
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -192]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -200], r8
	mov r8, 110
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -200]
	mov r9, qword ptr [rbp + -208]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -216], r8
	mov r8, 105
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, qword ptr [rbp + -224]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -232], r8
	mov r8, 110
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -240]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 72]
	mov qword ptr [rbp + -248], r8
	mov r8, 103
	mov qword ptr [rbp + -256], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, qword ptr [rbp + -256]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -104]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -272], r9
	mov r8, qword ptr [rbp + -272]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	mov r8, qword ptr [rbp + -48]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -280], r8
	mov r8, qword ptr [rbp + -280]
	mov r9, r8
	mov qword ptr [rbp + -288], r9
	mov r8, qword ptr [rbp + -288]
	mov rax, r8
	leave
	ret
	_l1:
	mov r8, 0
	mov qword ptr [rbp + -296], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, qword ptr [rbp + -296]
	cmp r8, r9
	jne _l4
	_l5:
	mov r8, 1
	mov qword ptr [rbp + -304], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -312], r9
	mov r8, qword ptr [rbp + -312]
	mov r9, qword ptr [rbp + -32]
	sub r8, r9
	mov qword ptr [rbp + -312], r8
	mov r8, qword ptr [rbp + -312]
	mov r9, r8
	mov qword ptr [rbp + -320], r9
	mov r8, 1
	mov qword ptr [rbp + -328], r8
	mov r8, qword ptr [rbp + -328]
	mov r9, r8
	mov qword ptr [rbp + -336], r9
	mov r8, qword ptr [rbp + -336]
	mov rsi, r8
	mov r8, qword ptr [rbp + -320]
	mov rdi, r8
	and rsp, -16
	call _Iack_iii
	mov r8, rax
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -344], r9
	mov r8, qword ptr [rbp + -344]
	mov r9, r8
	mov qword ptr [rbp + -352], r9
	mov r8, qword ptr [rbp + -352]
	mov r9, r8
	mov qword ptr [rbp + -360], r9
	mov r8, qword ptr [rbp + -360]
	mov rax, r8
	leave
	ret
	_l4:
	mov r8, 1
	mov qword ptr [rbp + -368], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -376], r9
	mov r8, qword ptr [rbp + -376]
	mov r9, qword ptr [rbp + -32]
	sub r8, r9
	mov qword ptr [rbp + -376], r8
	mov r8, qword ptr [rbp + -376]
	mov r9, r8
	mov qword ptr [rbp + -384], r9
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -392], r9
	mov r8, 1
	mov qword ptr [rbp + -400], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -408], r9
	mov r8, qword ptr [rbp + -408]
	mov r9, qword ptr [rbp + -48]
	sub r8, r9
	mov qword ptr [rbp + -408], r8
	mov r8, qword ptr [rbp + -408]
	mov r9, r8
	mov qword ptr [rbp + -416], r9
	mov r8, qword ptr [rbp + -416]
	mov rsi, r8
	mov r8, qword ptr [rbp + -392]
	mov rdi, r8
	and rsp, -16
	call _Iack_iii
	mov r8, rax
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -424], r9
	mov r8, qword ptr [rbp + -424]
	mov r9, r8
	mov qword ptr [rbp + -432], r9
	mov r8, qword ptr [rbp + -432]
	mov r9, r8
	mov qword ptr [rbp + -440], r9
	mov r8, qword ptr [rbp + -440]
	mov rsi, r8
	mov r8, qword ptr [rbp + -384]
	mov rdi, r8
	and rsp, -16
	call _Iack_iii
	mov r8, rax
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -448], r9
	mov r8, qword ptr [rbp + -448]
	mov r9, r8
	mov qword ptr [rbp + -456], r9
	mov r8, qword ptr [rbp + -456]
	mov r9, r8
	mov qword ptr [rbp + -464], r9
	mov r8, qword ptr [rbp + -464]
	mov rax, r8
	leave
	ret
