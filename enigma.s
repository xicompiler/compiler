.intel_syntax noprefix
.data
.globl _ItoLower_ii, _ImakeInverse_paiai, _ImkMatrix_aaii, _ImakeRotor_t3aaiaaiiai, _IrotorEncryptForward_iaaiaaiii, _IrotorEncryptBack_iaaiaaiii, _ImakeReflector_aiai, _IreflectorEncrypt_iaii, _Imain_paai
.text
_ItoLower_ii:
	enter 80, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, 65
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -24]
	mov r9, qword ptr [rbp + -32]
	cmp r8, r9
	jl _l0
	_l2:
	mov r8, 90
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -24]
	mov r9, qword ptr [rbp + -40]
	cmp r8, r9
	jg _l0
	_l1:
	mov r8, 65
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	mov r8, qword ptr [rbp + -56]
	mov r9, qword ptr [rbp + -48]
	sub r8, r9
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	lea r8, qword ptr [r8 + 97]
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, qword ptr [rbp + -72]
	mov rax, r8
	leave
	ret
	_l0:
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -80]
	mov rax, r8
	leave
	ret
_ImakeInverse_paiai:
	enter 224, 0
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
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	_l5:
	mov r8, 26
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, qword ptr [rbp + -72]
	cmp r8, r9
	jge _l3
	_l4:
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, 8
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -104]
	sub r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -96]
	mov r9, qword ptr [rbp + -120]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -128], r8
	mov r8, 1
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, r8
	mov qword ptr [rbp + -144], r9
	mov r8, qword ptr [rbp + -144]
	mov r9, qword ptr [rbp + -136]
	xor r8, r9
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -144]
	mov r9, qword ptr [rbp + -144]
	test r8, r9
	jnz _l8
	_l9:
	mov r8, qword ptr [rbp + -88]
	mov r9, qword ptr [rbp + -96]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, r8
	mov qword ptr [rbp + -160], r9
	mov r8, 8
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -176], r9
	mov r8, qword ptr [rbp + -176]
	mov r9, qword ptr [rbp + -168]
	sub r8, r9
	mov qword ptr [rbp + -176], r8
	mov r8, qword ptr [rbp + -176]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -184], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, qword ptr [rbp + -184]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -192], r8
	mov r8, 1
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -208], r9
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -200]
	xor r8, r9
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -208]
	test r8, r9
	jnz _l6
	_l7:
	mov r8, qword ptr [rbp + -80]
	mov r9, qword ptr [rbp + -160]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -216], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, qword ptr [rbp + -64]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -64]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -224]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	jmp _l5
	_l6:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l7
	_l8:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l9
	_l3:
	leave
	ret
_ImkMatrix_aaii:
	enter 272, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -32], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, 8
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, qword ptr [rbp + -32]
	lea r8, qword ptr [r8 + r9 * 8]
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
	mov r8, qword ptr [rbp + -96]
	mov r9, qword ptr [rbp + -32]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -96]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, qword ptr [rbp + -112]
	mov r9, r8
	mov qword ptr [rbp + -120], r9
	mov r8, 0
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, r8
	mov qword ptr [rbp + -136], r9
	_l15:
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -32]
	cmp r8, r9
	jge _l13
	_l14:
	mov r8, 8
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -144]
	mov r9, qword ptr [rbp + -40]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, r8
	mov qword ptr [rbp + -160], r9
	mov r8, qword ptr [rbp + -160]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -168], r9
	mov r8, qword ptr [rbp + -168]
	mov r9, r8
	mov qword ptr [rbp + -176], r9
	mov r8, qword ptr [rbp + -176]
	mov r9, r8
	mov qword ptr [rbp + -184], r9
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -40]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -184]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -192], r8
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -200], r9
	mov r8, qword ptr [rbp + -200]
	mov r9, r8
	mov qword ptr [rbp + -208], r9
	mov r8, 0
	mov qword ptr [rbp + -216], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, r8
	mov qword ptr [rbp + -224], r9
	_l12:
	mov r8, qword ptr [rbp + -224]
	mov r9, qword ptr [rbp + -40]
	cmp r8, r9
	jge _l10
	_l11:
	mov r8, qword ptr [rbp + -200]
	mov r9, qword ptr [rbp + -224]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -240]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -224]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -248], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, r8
	mov qword ptr [rbp + -224], r9
	jmp _l12
	_l10:
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -256], r8
	mov r8, qword ptr [rbp + -256]
	mov r9, qword ptr [rbp + -208]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -136], r9
	jmp _l15
	_l13:
	mov r8, qword ptr [rbp + -120]
	mov r9, r8
	mov qword ptr [rbp + -272], r9
	mov r8, qword ptr [rbp + -272]
	mov rax, r8
	leave
	ret
_ImakeRotor_t3aaiaaiiai:
	enter 1576, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -32], r9
	mov r8, 26
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	mov r8, qword ptr [rbp + -48]
	mov rdi, r8
	and rsp, -16
	call _ImkMatrix_aaii
	mov r8, rax
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, 26
	mov qword ptr [rbp + -88], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, qword ptr [rbp + -96]
	mov rdi, r8
	and rsp, -16
	call _ImkMatrix_aaii
	mov r8, rax
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -104], r9
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, qword ptr [rbp + -112]
	mov r9, r8
	mov qword ptr [rbp + -120], r9
	mov r8, 216
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, r8
	mov qword ptr [rbp + -136], r9
	mov r8, qword ptr [rbp + -136]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -144], r9
	mov r8, qword ptr [rbp + -144]
	mov r9, r8
	mov qword ptr [rbp + -152], r9
	mov r8, qword ptr [rbp + -152]
	mov r9, r8
	mov qword ptr [rbp + -160], r9
	mov r8, 26
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, qword ptr [rbp + -168]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -160]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -176], r8
	mov r8, qword ptr [rbp + -176]
	mov r9, r8
	mov qword ptr [rbp + -184], r9
	mov r8, qword ptr [rbp + -184]
	mov r9, r8
	mov qword ptr [rbp + -192], r9
	mov r8, 0
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -200]
	mov r9, r8
	mov qword ptr [rbp + -208], r9
	_l18:
	mov r8, 26
	mov qword ptr [rbp + -216], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -216]
	cmp r8, r9
	jge _l16
	_l17:
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -208]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -224]
	mov r9, qword ptr [rbp + -232]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -208]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -240]
	mov r9, r8
	mov qword ptr [rbp + -208], r9
	jmp _l18
	_l16:
	mov r8, 0
	mov qword ptr [rbp + -248], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	_l21:
	mov r8, 26
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -256]
	mov r9, qword ptr [rbp + -264]
	cmp r8, r9
	jge _l19
	_l20:
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -272], r9
	mov r8, qword ptr [rbp + -256]
	mov r9, r8
	mov qword ptr [rbp + -280], r9
	mov r8, 8
	mov qword ptr [rbp + -288], r8
	mov r8, qword ptr [rbp + -272]
	mov r9, r8
	mov qword ptr [rbp + -296], r9
	mov r8, qword ptr [rbp + -296]
	mov r9, qword ptr [rbp + -288]
	sub r8, r9
	mov qword ptr [rbp + -296], r8
	mov r8, qword ptr [rbp + -296]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -304], r8
	mov r8, qword ptr [rbp + -280]
	mov r9, qword ptr [rbp + -304]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -312], r8
	mov r8, 1
	mov qword ptr [rbp + -320], r8
	mov r8, qword ptr [rbp + -312]
	mov r9, r8
	mov qword ptr [rbp + -328], r9
	mov r8, qword ptr [rbp + -328]
	mov r9, qword ptr [rbp + -320]
	xor r8, r9
	mov qword ptr [rbp + -328], r8
	mov r8, qword ptr [rbp + -328]
	mov r9, qword ptr [rbp + -328]
	test r8, r9
	jnz _l22
	_l23:
	mov r8, qword ptr [rbp + -272]
	mov r9, qword ptr [rbp + -280]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -336], r8
	mov r8, qword ptr [rbp + -336]
	mov r9, r8
	mov qword ptr [rbp + -344], r9
	mov r8, qword ptr [rbp + -344]
	mov rdi, r8
	and rsp, -16
	call _ItoLower_ii
	mov r8, rax
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -352], r9
	mov r8, qword ptr [rbp + -352]
	mov r9, r8
	mov qword ptr [rbp + -360], r9
	mov r8, 97
	mov qword ptr [rbp + -368], r8
	mov r8, qword ptr [rbp + -360]
	mov r9, r8
	mov qword ptr [rbp + -376], r9
	mov r8, qword ptr [rbp + -376]
	mov r9, qword ptr [rbp + -368]
	sub r8, r9
	mov qword ptr [rbp + -376], r8
	mov r8, qword ptr [rbp + -376]
	mov r9, r8
	mov qword ptr [rbp + -384], r9
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -392], r9
	mov r8, qword ptr [rbp + -256]
	mov r9, r8
	mov qword ptr [rbp + -400], r9
	mov r8, 8
	mov qword ptr [rbp + -408], r8
	mov r8, qword ptr [rbp + -392]
	mov r9, r8
	mov qword ptr [rbp + -416], r9
	mov r8, qword ptr [rbp + -416]
	mov r9, qword ptr [rbp + -408]
	sub r8, r9
	mov qword ptr [rbp + -416], r8
	mov r8, qword ptr [rbp + -416]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -424], r8
	mov r8, qword ptr [rbp + -400]
	mov r9, qword ptr [rbp + -424]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -432], r8
	mov r8, 1
	mov qword ptr [rbp + -440], r8
	mov r8, qword ptr [rbp + -432]
	mov r9, r8
	mov qword ptr [rbp + -448], r9
	mov r8, qword ptr [rbp + -448]
	mov r9, qword ptr [rbp + -440]
	xor r8, r9
	mov qword ptr [rbp + -448], r8
	mov r8, qword ptr [rbp + -448]
	mov r9, qword ptr [rbp + -448]
	test r8, r9
	jnz _l24
	_l25:
	mov r8, qword ptr [rbp + -392]
	mov r9, qword ptr [rbp + -400]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -456], r8
	mov r8, qword ptr [rbp + -456]
	mov r9, qword ptr [rbp + -384]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -256]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -464], r8
	mov r8, qword ptr [rbp + -464]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	jmp _l21
	_l24:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l25
	_l22:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l23
	_l19:
	mov r8, 216
	mov qword ptr [rbp + -472], r8
	mov r8, qword ptr [rbp + -472]
	mov r9, r8
	mov qword ptr [rbp + -480], r9
	mov r8, qword ptr [rbp + -480]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -488], r9
	mov r8, qword ptr [rbp + -488]
	mov r9, r8
	mov qword ptr [rbp + -496], r9
	mov r8, qword ptr [rbp + -496]
	mov r9, r8
	mov qword ptr [rbp + -504], r9
	mov r8, 26
	mov qword ptr [rbp + -512], r8
	mov r8, qword ptr [rbp + -504]
	mov r9, qword ptr [rbp + -512]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -504]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -520], r8
	mov r8, qword ptr [rbp + -520]
	mov r9, r8
	mov qword ptr [rbp + -528], r9
	mov r8, qword ptr [rbp + -528]
	mov r9, r8
	mov qword ptr [rbp + -536], r9
	mov r8, 0
	mov qword ptr [rbp + -544], r8
	mov r8, qword ptr [rbp + -544]
	mov r9, r8
	mov qword ptr [rbp + -552], r9
	_l28:
	mov r8, 26
	mov qword ptr [rbp + -560], r8
	mov r8, qword ptr [rbp + -552]
	mov r9, qword ptr [rbp + -560]
	cmp r8, r9
	jge _l26
	_l27:
	mov r8, qword ptr [rbp + -528]
	mov r9, qword ptr [rbp + -552]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -568], r8
	mov r8, qword ptr [rbp + -568]
	mov r9, qword ptr [rbp + -576]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -552]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -584], r8
	mov r8, qword ptr [rbp + -584]
	mov r9, r8
	mov qword ptr [rbp + -552], r9
	jmp _l28
	_l26:
	mov r8, 0
	mov qword ptr [rbp + -592], r8
	mov r8, qword ptr [rbp + -592]
	mov r9, r8
	mov qword ptr [rbp + -600], r9
	_l31:
	mov r8, 26
	mov qword ptr [rbp + -608], r8
	mov r8, qword ptr [rbp + -600]
	mov r9, qword ptr [rbp + -608]
	cmp r8, r9
	jge _l29
	_l30:
	mov r8, 0
	mov qword ptr [rbp + -616], r8
	mov r8, qword ptr [rbp + -616]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	_l34:
	mov r8, 26
	mov qword ptr [rbp + -624], r8
	mov r8, qword ptr [rbp + -256]
	mov r9, qword ptr [rbp + -624]
	cmp r8, r9
	jge _l32
	_l33:
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -632], r9
	mov r8, qword ptr [rbp + -600]
	mov r9, r8
	mov qword ptr [rbp + -640], r9
	mov r8, 8
	mov qword ptr [rbp + -648], r8
	mov r8, qword ptr [rbp + -632]
	mov r9, r8
	mov qword ptr [rbp + -656], r9
	mov r8, qword ptr [rbp + -656]
	mov r9, qword ptr [rbp + -648]
	sub r8, r9
	mov qword ptr [rbp + -656], r8
	mov r8, qword ptr [rbp + -656]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -664], r8
	mov r8, qword ptr [rbp + -640]
	mov r9, qword ptr [rbp + -664]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -672], r8
	mov r8, 1
	mov qword ptr [rbp + -680], r8
	mov r8, qword ptr [rbp + -672]
	mov r9, r8
	mov qword ptr [rbp + -688], r9
	mov r8, qword ptr [rbp + -688]
	mov r9, qword ptr [rbp + -680]
	xor r8, r9
	mov qword ptr [rbp + -688], r8
	mov r8, qword ptr [rbp + -688]
	mov r9, qword ptr [rbp + -688]
	test r8, r9
	jnz _l39
	_l40:
	mov r8, qword ptr [rbp + -632]
	mov r9, qword ptr [rbp + -640]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -696], r8
	mov r8, qword ptr [rbp + -696]
	mov r9, r8
	mov qword ptr [rbp + -704], r9
	mov r8, qword ptr [rbp + -256]
	mov r9, r8
	mov qword ptr [rbp + -712], r9
	mov r8, 8
	mov qword ptr [rbp + -720], r8
	mov r8, qword ptr [rbp + -704]
	mov r9, r8
	mov qword ptr [rbp + -728], r9
	mov r8, qword ptr [rbp + -728]
	mov r9, qword ptr [rbp + -720]
	sub r8, r9
	mov qword ptr [rbp + -728], r8
	mov r8, qword ptr [rbp + -728]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -736], r8
	mov r8, qword ptr [rbp + -712]
	mov r9, qword ptr [rbp + -736]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -744], r8
	mov r8, 1
	mov qword ptr [rbp + -752], r8
	mov r8, qword ptr [rbp + -744]
	mov r9, r8
	mov qword ptr [rbp + -760], r9
	mov r8, qword ptr [rbp + -760]
	mov r9, qword ptr [rbp + -752]
	xor r8, r9
	mov qword ptr [rbp + -760], r8
	mov r8, qword ptr [rbp + -760]
	mov r9, qword ptr [rbp + -760]
	test r8, r9
	jnz _l37
	_l38:
	mov r8, qword ptr [rbp + -704]
	mov r9, qword ptr [rbp + -712]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -768], r8
	mov r8, qword ptr [rbp + -768]
	mov r9, r8
	mov qword ptr [rbp + -776], r9
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -784], r9
	mov r8, qword ptr [rbp + -256]
	mov r9, r8
	mov qword ptr [rbp + -792], r9
	mov r8, 8
	mov qword ptr [rbp + -800], r8
	mov r8, qword ptr [rbp + -784]
	mov r9, r8
	mov qword ptr [rbp + -808], r9
	mov r8, qword ptr [rbp + -808]
	mov r9, qword ptr [rbp + -800]
	sub r8, r9
	mov qword ptr [rbp + -808], r8
	mov r8, qword ptr [rbp + -808]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -816], r8
	mov r8, qword ptr [rbp + -792]
	mov r9, qword ptr [rbp + -816]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -824], r8
	mov r8, 1
	mov qword ptr [rbp + -832], r8
	mov r8, qword ptr [rbp + -824]
	mov r9, r8
	mov qword ptr [rbp + -840], r9
	mov r8, qword ptr [rbp + -840]
	mov r9, qword ptr [rbp + -832]
	xor r8, r9
	mov qword ptr [rbp + -840], r8
	mov r8, qword ptr [rbp + -840]
	mov r9, qword ptr [rbp + -840]
	test r8, r9
	jnz _l35
	_l36:
	mov r8, qword ptr [rbp + -784]
	mov r9, qword ptr [rbp + -792]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -848], r8
	mov r8, qword ptr [rbp + -776]
	mov r9, qword ptr [rbp + -848]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -256]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -856], r8
	mov r8, qword ptr [rbp + -856]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	jmp _l34
	_l35:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l36
	_l37:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l38
	_l39:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l40
	_l32:
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -864], r9
	mov r8, qword ptr [rbp + -536]
	mov r9, r8
	mov qword ptr [rbp + -872], r9
	mov r8, qword ptr [rbp + -872]
	mov rsi, r8
	mov r8, qword ptr [rbp + -864]
	mov rdi, r8
	and rsp, -16
	call _ImakeInverse_paiai
	mov r8, 0
	mov qword ptr [rbp + -880], r8
	mov r8, qword ptr [rbp + -880]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	_l43:
	mov r8, 26
	mov qword ptr [rbp + -888], r8
	mov r8, qword ptr [rbp + -256]
	mov r9, qword ptr [rbp + -888]
	cmp r8, r9
	jge _l41
	_l42:
	mov r8, qword ptr [rbp + -120]
	mov r9, r8
	mov qword ptr [rbp + -896], r9
	mov r8, qword ptr [rbp + -600]
	mov r9, r8
	mov qword ptr [rbp + -904], r9
	mov r8, 8
	mov qword ptr [rbp + -912], r8
	mov r8, qword ptr [rbp + -896]
	mov r9, r8
	mov qword ptr [rbp + -920], r9
	mov r8, qword ptr [rbp + -920]
	mov r9, qword ptr [rbp + -912]
	sub r8, r9
	mov qword ptr [rbp + -920], r8
	mov r8, qword ptr [rbp + -920]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -928], r8
	mov r8, qword ptr [rbp + -904]
	mov r9, qword ptr [rbp + -928]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -936], r8
	mov r8, 1
	mov qword ptr [rbp + -944], r8
	mov r8, qword ptr [rbp + -936]
	mov r9, r8
	mov qword ptr [rbp + -952], r9
	mov r8, qword ptr [rbp + -952]
	mov r9, qword ptr [rbp + -944]
	xor r8, r9
	mov qword ptr [rbp + -952], r8
	mov r8, qword ptr [rbp + -952]
	mov r9, qword ptr [rbp + -952]
	test r8, r9
	jnz _l48
	_l49:
	mov r8, qword ptr [rbp + -896]
	mov r9, qword ptr [rbp + -904]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -960], r8
	mov r8, qword ptr [rbp + -960]
	mov r9, r8
	mov qword ptr [rbp + -968], r9
	mov r8, qword ptr [rbp + -256]
	mov r9, r8
	mov qword ptr [rbp + -976], r9
	mov r8, 8
	mov qword ptr [rbp + -984], r8
	mov r8, qword ptr [rbp + -968]
	mov r9, r8
	mov qword ptr [rbp + -992], r9
	mov r8, qword ptr [rbp + -992]
	mov r9, qword ptr [rbp + -984]
	sub r8, r9
	mov qword ptr [rbp + -992], r8
	mov r8, qword ptr [rbp + -992]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1000], r8
	mov r8, qword ptr [rbp + -976]
	mov r9, qword ptr [rbp + -1000]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1008], r8
	mov r8, 1
	mov qword ptr [rbp + -1016], r8
	mov r8, qword ptr [rbp + -1008]
	mov r9, r8
	mov qword ptr [rbp + -1024], r9
	mov r8, qword ptr [rbp + -1024]
	mov r9, qword ptr [rbp + -1016]
	xor r8, r9
	mov qword ptr [rbp + -1024], r8
	mov r8, qword ptr [rbp + -1024]
	mov r9, qword ptr [rbp + -1024]
	test r8, r9
	jnz _l46
	_l47:
	mov r8, qword ptr [rbp + -968]
	mov r9, qword ptr [rbp + -976]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -1032], r8
	mov r8, qword ptr [rbp + -1032]
	mov r9, r8
	mov qword ptr [rbp + -1040], r9
	mov r8, qword ptr [rbp + -536]
	mov r9, r8
	mov qword ptr [rbp + -1048], r9
	mov r8, qword ptr [rbp + -256]
	mov r9, r8
	mov qword ptr [rbp + -1056], r9
	mov r8, 8
	mov qword ptr [rbp + -1064], r8
	mov r8, qword ptr [rbp + -1048]
	mov r9, r8
	mov qword ptr [rbp + -1072], r9
	mov r8, qword ptr [rbp + -1072]
	mov r9, qword ptr [rbp + -1064]
	sub r8, r9
	mov qword ptr [rbp + -1072], r8
	mov r8, qword ptr [rbp + -1072]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1080], r8
	mov r8, qword ptr [rbp + -1056]
	mov r9, qword ptr [rbp + -1080]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1088], r8
	mov r8, 1
	mov qword ptr [rbp + -1096], r8
	mov r8, qword ptr [rbp + -1088]
	mov r9, r8
	mov qword ptr [rbp + -1104], r9
	mov r8, qword ptr [rbp + -1104]
	mov r9, qword ptr [rbp + -1096]
	xor r8, r9
	mov qword ptr [rbp + -1104], r8
	mov r8, qword ptr [rbp + -1104]
	mov r9, qword ptr [rbp + -1104]
	test r8, r9
	jnz _l44
	_l45:
	mov r8, qword ptr [rbp + -1048]
	mov r9, qword ptr [rbp + -1056]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -1112], r8
	mov r8, qword ptr [rbp + -1040]
	mov r9, qword ptr [rbp + -1112]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -256]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -1120], r8
	mov r8, qword ptr [rbp + -1120]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	jmp _l43
	_l44:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l45
	_l46:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l47
	_l48:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l49
	_l41:
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -1128], r9
	mov r8, 0
	mov qword ptr [rbp + -1136], r8
	mov r8, qword ptr [rbp + -1136]
	mov r9, r8
	mov qword ptr [rbp + -1144], r9
	mov r8, 8
	mov qword ptr [rbp + -1152], r8
	mov r8, qword ptr [rbp + -1128]
	mov r9, r8
	mov qword ptr [rbp + -1160], r9
	mov r8, qword ptr [rbp + -1160]
	mov r9, qword ptr [rbp + -1152]
	sub r8, r9
	mov qword ptr [rbp + -1160], r8
	mov r8, qword ptr [rbp + -1160]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1168], r8
	mov r8, qword ptr [rbp + -1144]
	mov r9, qword ptr [rbp + -1168]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1176], r8
	mov r8, 1
	mov qword ptr [rbp + -1184], r8
	mov r8, qword ptr [rbp + -1176]
	mov r9, r8
	mov qword ptr [rbp + -1192], r9
	mov r8, qword ptr [rbp + -1192]
	mov r9, qword ptr [rbp + -1184]
	xor r8, r9
	mov qword ptr [rbp + -1192], r8
	mov r8, qword ptr [rbp + -1192]
	mov r9, qword ptr [rbp + -1192]
	test r8, r9
	jnz _l50
	_l51:
	mov r8, 1
	mov qword ptr [rbp + -1200], r8
	mov r8, qword ptr [rbp + -1128]
	mov r9, qword ptr [rbp + -1144]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -1208], r8
	mov r8, qword ptr [rbp + -1208]
	mov r9, qword ptr [rbp + -1200]
	sub r8, r9
	mov qword ptr [rbp + -1208], r8
	mov r8, qword ptr [rbp + -1208]
	lea r8, qword ptr [r8 + 26]
	mov qword ptr [rbp + -1216], r8
	mov r8, 26
	mov qword ptr [rbp + -1224], r8
	mov r8, qword ptr [rbp + -1216]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1224]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1216], r8
	mov r8, qword ptr [rbp + -1216]
	mov r9, r8
	mov qword ptr [rbp + -1232], r9
	mov r8, 1
	mov qword ptr [rbp + -1240], r8
	mov r8, qword ptr [rbp + -1240]
	mov r9, r8
	mov qword ptr [rbp + -1248], r9
	_l54:
	mov r8, 26
	mov qword ptr [rbp + -1256], r8
	mov r8, qword ptr [rbp + -1248]
	mov r9, qword ptr [rbp + -1256]
	cmp r8, r9
	jge _l52
	_l53:
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -1264], r9
	mov r8, 1
	mov qword ptr [rbp + -1272], r8
	mov r8, qword ptr [rbp + -1248]
	mov r9, r8
	mov qword ptr [rbp + -1280], r9
	mov r8, qword ptr [rbp + -1280]
	mov r9, qword ptr [rbp + -1272]
	sub r8, r9
	mov qword ptr [rbp + -1280], r8
	mov r8, qword ptr [rbp + -1280]
	mov r9, r8
	mov qword ptr [rbp + -1288], r9
	mov r8, 8
	mov qword ptr [rbp + -1296], r8
	mov r8, qword ptr [rbp + -1264]
	mov r9, r8
	mov qword ptr [rbp + -1304], r9
	mov r8, qword ptr [rbp + -1304]
	mov r9, qword ptr [rbp + -1296]
	sub r8, r9
	mov qword ptr [rbp + -1304], r8
	mov r8, qword ptr [rbp + -1304]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1312], r8
	mov r8, qword ptr [rbp + -1288]
	mov r9, qword ptr [rbp + -1312]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1320], r8
	mov r8, 1
	mov qword ptr [rbp + -1328], r8
	mov r8, qword ptr [rbp + -1320]
	mov r9, r8
	mov qword ptr [rbp + -1336], r9
	mov r8, qword ptr [rbp + -1336]
	mov r9, qword ptr [rbp + -1328]
	xor r8, r9
	mov qword ptr [rbp + -1336], r8
	mov r8, qword ptr [rbp + -1336]
	mov r9, qword ptr [rbp + -1336]
	test r8, r9
	jnz _l57
	_l58:
	mov r8, qword ptr [rbp + -1264]
	mov r9, qword ptr [rbp + -1288]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -1344], r8
	mov r8, qword ptr [rbp + -1344]
	mov r9, r8
	mov qword ptr [rbp + -1352], r9
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -1360], r9
	mov r8, qword ptr [rbp + -1248]
	mov r9, r8
	mov qword ptr [rbp + -1368], r9
	mov r8, 8
	mov qword ptr [rbp + -1376], r8
	mov r8, qword ptr [rbp + -1360]
	mov r9, r8
	mov qword ptr [rbp + -1384], r9
	mov r8, qword ptr [rbp + -1384]
	mov r9, qword ptr [rbp + -1376]
	sub r8, r9
	mov qword ptr [rbp + -1384], r8
	mov r8, qword ptr [rbp + -1384]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1392], r8
	mov r8, qword ptr [rbp + -1368]
	mov r9, qword ptr [rbp + -1392]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1400], r8
	mov r8, 1
	mov qword ptr [rbp + -1408], r8
	mov r8, qword ptr [rbp + -1400]
	mov r9, r8
	mov qword ptr [rbp + -1416], r9
	mov r8, qword ptr [rbp + -1416]
	mov r9, qword ptr [rbp + -1408]
	xor r8, r9
	mov qword ptr [rbp + -1416], r8
	mov r8, qword ptr [rbp + -1416]
	mov r9, qword ptr [rbp + -1416]
	test r8, r9
	jnz _l55
	_l56:
	mov r8, 1
	mov qword ptr [rbp + -1424], r8
	mov r8, qword ptr [rbp + -1360]
	mov r9, qword ptr [rbp + -1368]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -1432], r8
	mov r8, qword ptr [rbp + -1432]
	mov r9, qword ptr [rbp + -1424]
	sub r8, r9
	mov qword ptr [rbp + -1432], r8
	mov r8, qword ptr [rbp + -1432]
	lea r8, qword ptr [r8 + 26]
	mov qword ptr [rbp + -1440], r8
	mov r8, 26
	mov qword ptr [rbp + -1448], r8
	mov r8, qword ptr [rbp + -1440]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1448]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1440], r8
	mov r8, qword ptr [rbp + -1352]
	mov r9, qword ptr [rbp + -1440]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1248]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -1456], r8
	mov r8, qword ptr [rbp + -1456]
	mov r9, r8
	mov qword ptr [rbp + -1248], r9
	jmp _l54
	_l55:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l56
	_l57:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l58
	_l52:
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -1464], r9
	mov r8, 25
	mov qword ptr [rbp + -1472], r8
	mov r8, qword ptr [rbp + -1472]
	mov r9, r8
	mov qword ptr [rbp + -1480], r9
	mov r8, 8
	mov qword ptr [rbp + -1488], r8
	mov r8, qword ptr [rbp + -1464]
	mov r9, r8
	mov qword ptr [rbp + -1496], r9
	mov r8, qword ptr [rbp + -1496]
	mov r9, qword ptr [rbp + -1488]
	sub r8, r9
	mov qword ptr [rbp + -1496], r8
	mov r8, qword ptr [rbp + -1496]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1504], r8
	mov r8, qword ptr [rbp + -1480]
	mov r9, qword ptr [rbp + -1504]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1512], r8
	mov r8, 1
	mov qword ptr [rbp + -1520], r8
	mov r8, qword ptr [rbp + -1512]
	mov r9, r8
	mov qword ptr [rbp + -1528], r9
	mov r8, qword ptr [rbp + -1528]
	mov r9, qword ptr [rbp + -1520]
	xor r8, r9
	mov qword ptr [rbp + -1528], r8
	mov r8, qword ptr [rbp + -1528]
	mov r9, qword ptr [rbp + -1528]
	test r8, r9
	jnz _l59
	_l60:
	mov r8, qword ptr [rbp + -1464]
	mov r9, qword ptr [rbp + -1480]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -1536], r8
	mov r8, qword ptr [rbp + -1536]
	mov r9, qword ptr [rbp + -1232]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -600]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -1544], r8
	mov r8, qword ptr [rbp + -1544]
	mov r9, r8
	mov qword ptr [rbp + -600], r9
	jmp _l31
	_l59:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l60
	_l50:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l51
	_l29:
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -1552], r9
	mov r8, qword ptr [rbp + -120]
	mov r9, r8
	mov qword ptr [rbp + -1560], r9
	mov r8, 0
	mov qword ptr [rbp + -1568], r8
	mov r8, qword ptr [rbp + -1568]
	mov r9, r8
	mov qword ptr [rbp + -1576], r9
	mov r8, qword ptr [rbp + -1552]
	mov rax, r8
	mov r8, qword ptr [rbp + -1560]
	mov rdx, r8
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -1576]
	mov qword ptr [r8 + 0], r9
	leave
	ret
_IrotorEncryptForward_iaaiaaiii:
	enter 248, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r8, rdx
	mov qword ptr [rbp + -24], r8
	mov r8, rcx
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -104], r9
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, 8
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -128], r9
	mov r8, qword ptr [rbp + -128]
	mov r9, qword ptr [rbp + -120]
	sub r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -136]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -144], r8
	mov r8, 1
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -144]
	mov r9, r8
	mov qword ptr [rbp + -160], r9
	mov r8, qword ptr [rbp + -160]
	mov r9, qword ptr [rbp + -152]
	xor r8, r9
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, qword ptr [rbp + -160]
	test r8, r9
	jnz _l61
	_l62:
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -112]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, r8
	mov qword ptr [rbp + -176], r9
	mov r8, qword ptr [rbp + -96]
	mov r9, r8
	mov qword ptr [rbp + -184], r9
	mov r8, 8
	mov qword ptr [rbp + -192], r8
	mov r8, qword ptr [rbp + -176]
	mov r9, r8
	mov qword ptr [rbp + -200], r9
	mov r8, qword ptr [rbp + -200]
	mov r9, qword ptr [rbp + -192]
	sub r8, r9
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -200]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -208]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -216], r8
	mov r8, 1
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, r8
	mov qword ptr [rbp + -232], r9
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -224]
	xor r8, r9
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -232]
	test r8, r9
	jnz _l63
	_l64:
	mov r8, qword ptr [rbp + -176]
	mov r9, qword ptr [rbp + -184]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -240]
	mov r9, r8
	mov qword ptr [rbp + -248], r9
	mov r8, qword ptr [rbp + -248]
	mov rax, r8
	leave
	ret
	_l63:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l64
	_l61:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l62
_IrotorEncryptBack_iaaiaaiii:
	enter 248, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r8, rdx
	mov qword ptr [rbp + -24], r8
	mov r8, rcx
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -48], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -104], r9
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, 8
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -128], r9
	mov r8, qword ptr [rbp + -128]
	mov r9, qword ptr [rbp + -120]
	sub r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -136]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -144], r8
	mov r8, 1
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -144]
	mov r9, r8
	mov qword ptr [rbp + -160], r9
	mov r8, qword ptr [rbp + -160]
	mov r9, qword ptr [rbp + -152]
	xor r8, r9
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, qword ptr [rbp + -160]
	test r8, r9
	jnz _l65
	_l66:
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -112]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, r8
	mov qword ptr [rbp + -176], r9
	mov r8, qword ptr [rbp + -96]
	mov r9, r8
	mov qword ptr [rbp + -184], r9
	mov r8, 8
	mov qword ptr [rbp + -192], r8
	mov r8, qword ptr [rbp + -176]
	mov r9, r8
	mov qword ptr [rbp + -200], r9
	mov r8, qword ptr [rbp + -200]
	mov r9, qword ptr [rbp + -192]
	sub r8, r9
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -200]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -208]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -216], r8
	mov r8, 1
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, r8
	mov qword ptr [rbp + -232], r9
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -224]
	xor r8, r9
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -232]
	test r8, r9
	jnz _l67
	_l68:
	mov r8, qword ptr [rbp + -176]
	mov r9, qword ptr [rbp + -184]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -240]
	mov r9, r8
	mov qword ptr [rbp + -248], r9
	mov r8, qword ptr [rbp + -248]
	mov rax, r8
	leave
	ret
	_l67:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l68
	_l65:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l66
_ImakeReflector_aiai:
	enter 384, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, 216
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
	mov r8, 26
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, qword ptr [rbp + -80]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -88], r8
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, qword ptr [rbp + -96]
	mov r9, r8
	mov qword ptr [rbp + -104], r9
	mov r8, 0
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, r8
	mov qword ptr [rbp + -120], r9
	_l71:
	mov r8, 26
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -120]
	mov r9, qword ptr [rbp + -128]
	cmp r8, r9
	jge _l69
	_l70:
	mov r8, qword ptr [rbp + -96]
	mov r9, qword ptr [rbp + -120]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -144]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -120]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, r8
	mov qword ptr [rbp + -120], r9
	jmp _l71
	_l69:
	mov r8, 0
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -168], r9
	_l74:
	mov r8, 26
	mov qword ptr [rbp + -176], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -176]
	cmp r8, r9
	jge _l72
	_l73:
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -184], r9
	mov r8, qword ptr [rbp + -168]
	mov r9, r8
	mov qword ptr [rbp + -192], r9
	mov r8, 8
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -184]
	mov r9, r8
	mov qword ptr [rbp + -208], r9
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -200]
	sub r8, r9
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -216], r8
	mov r8, qword ptr [rbp + -192]
	mov r9, qword ptr [rbp + -216]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -224], r8
	mov r8, 1
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -224]
	mov r9, r8
	mov qword ptr [rbp + -240], r9
	mov r8, qword ptr [rbp + -240]
	mov r9, qword ptr [rbp + -232]
	xor r8, r9
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -240]
	mov r9, qword ptr [rbp + -240]
	test r8, r9
	jnz _l75
	_l76:
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -192]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -248], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	mov r8, qword ptr [rbp + -256]
	mov rdi, r8
	and rsp, -16
	call _ItoLower_ii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -264], r9
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -272], r9
	mov r8, 97
	mov qword ptr [rbp + -280], r8
	mov r8, qword ptr [rbp + -272]
	mov r9, r8
	mov qword ptr [rbp + -288], r9
	mov r8, qword ptr [rbp + -288]
	mov r9, qword ptr [rbp + -280]
	sub r8, r9
	mov qword ptr [rbp + -288], r8
	mov r8, qword ptr [rbp + -288]
	mov r9, r8
	mov qword ptr [rbp + -296], r9
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -304], r9
	mov r8, qword ptr [rbp + -168]
	mov r9, r8
	mov qword ptr [rbp + -312], r9
	mov r8, 8
	mov qword ptr [rbp + -320], r8
	mov r8, qword ptr [rbp + -304]
	mov r9, r8
	mov qword ptr [rbp + -328], r9
	mov r8, qword ptr [rbp + -328]
	mov r9, qword ptr [rbp + -320]
	sub r8, r9
	mov qword ptr [rbp + -328], r8
	mov r8, qword ptr [rbp + -328]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -336], r8
	mov r8, qword ptr [rbp + -312]
	mov r9, qword ptr [rbp + -336]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -344], r8
	mov r8, 1
	mov qword ptr [rbp + -352], r8
	mov r8, qword ptr [rbp + -344]
	mov r9, r8
	mov qword ptr [rbp + -360], r9
	mov r8, qword ptr [rbp + -360]
	mov r9, qword ptr [rbp + -352]
	xor r8, r9
	mov qword ptr [rbp + -360], r8
	mov r8, qword ptr [rbp + -360]
	mov r9, qword ptr [rbp + -360]
	test r8, r9
	jnz _l77
	_l78:
	mov r8, qword ptr [rbp + -304]
	mov r9, qword ptr [rbp + -312]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -368], r8
	mov r8, qword ptr [rbp + -368]
	mov r9, qword ptr [rbp + -296]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -168]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -376], r8
	mov r8, qword ptr [rbp + -376]
	mov r9, r8
	mov qword ptr [rbp + -168], r9
	jmp _l74
	_l77:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l78
	_l75:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l76
	_l72:
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -384], r9
	mov r8, qword ptr [rbp + -384]
	mov rax, r8
	leave
	ret
_IreflectorEncrypt_iaii:
	enter 128, 0
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
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -64], r9
	mov r8, 8
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -80]
	mov r9, qword ptr [rbp + -72]
	sub r8, r9
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -88], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, qword ptr [rbp + -88]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -96], r8
	mov r8, 1
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -96]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -104]
	xor r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -112]
	test r8, r9
	jnz _l79
	_l80:
	mov r8, qword ptr [rbp + -56]
	mov r9, qword ptr [rbp + -64]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -120]
	mov r9, r8
	mov qword ptr [rbp + -128], r9
	mov r8, qword ptr [rbp + -128]
	mov rax, r8
	leave
	ret
	_l79:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l80
_Imain_paai:
	enter 5192, 0
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
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, 72
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, qword ptr [rbp + -112]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -120], r9
	mov r8, qword ptr [rbp + -120]
	mov r9, r8
	mov qword ptr [rbp + -128], r9
	mov r8, qword ptr [rbp + -128]
	mov r9, r8
	mov qword ptr [rbp + -136], r9
	mov r8, 8
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -144]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -152], r8
	mov r8, 12
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -160]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -168], r8
	mov r8, 27
	mov qword ptr [rbp + -176], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -176]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -184], r8
	mov r8, 6
	mov qword ptr [rbp + -192], r8
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -192]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -200], r8
	mov r8, 57
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -200]
	mov r9, qword ptr [rbp + -208]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -216], r8
	mov r8, 25
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, qword ptr [rbp + -224]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -232], r8
	mov r8, 51
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -240]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -248], r8
	mov r8, 52
	mov qword ptr [rbp + -256], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, qword ptr [rbp + -256]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -264], r8
	mov r8, -1
	mov qword ptr [rbp + -272], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, qword ptr [rbp + -272]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -136]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -280], r8
	mov r8, qword ptr [rbp + -96]
	mov r9, qword ptr [rbp + -280]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -288], r8
	mov r8, qword ptr [rbp + -288]
	mov r9, r8
	mov qword ptr [rbp + -296], r9
	mov r8, 72
	mov qword ptr [rbp + -304], r8
	mov r8, qword ptr [rbp + -304]
	mov r9, r8
	mov qword ptr [rbp + -312], r9
	mov r8, qword ptr [rbp + -312]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -320], r9
	mov r8, qword ptr [rbp + -320]
	mov r9, r8
	mov qword ptr [rbp + -328], r9
	mov r8, qword ptr [rbp + -328]
	mov r9, r8
	mov qword ptr [rbp + -336], r9
	mov r8, 8
	mov qword ptr [rbp + -344], r8
	mov r8, qword ptr [rbp + -336]
	mov r9, qword ptr [rbp + -344]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -352], r8
	mov r8, 12
	mov qword ptr [rbp + -360], r8
	mov r8, qword ptr [rbp + -352]
	mov r9, qword ptr [rbp + -360]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -368], r8
	mov r8, 27
	mov qword ptr [rbp + -376], r8
	mov r8, qword ptr [rbp + -368]
	mov r9, qword ptr [rbp + -376]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -384], r8
	mov r8, 6
	mov qword ptr [rbp + -392], r8
	mov r8, qword ptr [rbp + -384]
	mov r9, qword ptr [rbp + -392]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -400], r8
	mov r8, 55
	mov qword ptr [rbp + -408], r8
	mov r8, qword ptr [rbp + -400]
	mov r9, qword ptr [rbp + -408]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -416], r8
	mov r8, 25
	mov qword ptr [rbp + -424], r8
	mov r8, qword ptr [rbp + -416]
	mov r9, qword ptr [rbp + -424]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -432], r8
	mov r8, 51
	mov qword ptr [rbp + -440], r8
	mov r8, qword ptr [rbp + -432]
	mov r9, qword ptr [rbp + -440]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -448], r8
	mov r8, 52
	mov qword ptr [rbp + -456], r8
	mov r8, qword ptr [rbp + -448]
	mov r9, qword ptr [rbp + -456]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -464], r8
	mov r8, -1
	mov qword ptr [rbp + -472], r8
	mov r8, qword ptr [rbp + -464]
	mov r9, qword ptr [rbp + -472]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -480], r8
	mov r8, qword ptr [rbp + -296]
	mov r9, qword ptr [rbp + -480]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -488], r8
	mov r8, qword ptr [rbp + -488]
	mov r9, r8
	mov qword ptr [rbp + -496], r9
	mov r8, 40
	mov qword ptr [rbp + -504], r8
	mov r8, qword ptr [rbp + -504]
	mov r9, r8
	mov qword ptr [rbp + -512], r9
	mov r8, qword ptr [rbp + -512]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -520], r9
	mov r8, qword ptr [rbp + -520]
	mov r9, r8
	mov qword ptr [rbp + -528], r9
	mov r8, qword ptr [rbp + -528]
	mov r9, r8
	mov qword ptr [rbp + -536], r9
	mov r8, 4
	mov qword ptr [rbp + -544], r8
	mov r8, qword ptr [rbp + -536]
	mov r9, qword ptr [rbp + -544]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -536]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -552], r8
	mov r8, 12
	mov qword ptr [rbp + -560], r8
	mov r8, qword ptr [rbp + -552]
	mov r9, qword ptr [rbp + -560]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -536]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -568], r8
	mov r8, 46
	mov qword ptr [rbp + -576], r8
	mov r8, qword ptr [rbp + -568]
	mov r9, qword ptr [rbp + -576]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -536]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -584], r8
	mov r8, 47
	mov qword ptr [rbp + -592], r8
	mov r8, qword ptr [rbp + -584]
	mov r9, qword ptr [rbp + -592]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -536]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -600], r8
	mov r8, -1
	mov qword ptr [rbp + -608], r8
	mov r8, qword ptr [rbp + -600]
	mov r9, qword ptr [rbp + -608]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -536]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -616], r8
	mov r8, qword ptr [rbp + -496]
	mov r9, qword ptr [rbp + -616]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -624], r8
	mov r8, qword ptr [rbp + -624]
	mov r9, r8
	mov qword ptr [rbp + -632], r9
	mov r8, 64
	mov qword ptr [rbp + -640], r8
	mov r8, qword ptr [rbp + -640]
	mov r9, r8
	mov qword ptr [rbp + -648], r9
	mov r8, qword ptr [rbp + -648]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -656], r9
	mov r8, qword ptr [rbp + -656]
	mov r9, r8
	mov qword ptr [rbp + -664], r9
	mov r8, qword ptr [rbp + -664]
	mov r9, r8
	mov qword ptr [rbp + -672], r9
	mov r8, 7
	mov qword ptr [rbp + -680], r8
	mov r8, qword ptr [rbp + -672]
	mov r9, qword ptr [rbp + -680]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -672]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -688], r8
	mov r8, 12
	mov qword ptr [rbp + -696], r8
	mov r8, qword ptr [rbp + -688]
	mov r9, qword ptr [rbp + -696]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -672]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -704], r8
	mov r8, 27
	mov qword ptr [rbp + -712], r8
	mov r8, qword ptr [rbp + -704]
	mov r9, qword ptr [rbp + -712]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -672]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -720], r8
	mov r8, 6
	mov qword ptr [rbp + -728], r8
	mov r8, qword ptr [rbp + -720]
	mov r9, qword ptr [rbp + -728]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -672]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -736], r8
	mov r8, 16
	mov qword ptr [rbp + -744], r8
	mov r8, qword ptr [rbp + -736]
	mov r9, qword ptr [rbp + -744]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -672]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -752], r8
	mov r8, 11
	mov qword ptr [rbp + -760], r8
	mov r8, qword ptr [rbp + -752]
	mov r9, qword ptr [rbp + -760]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -672]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -768], r8
	mov r8, 52
	mov qword ptr [rbp + -776], r8
	mov r8, qword ptr [rbp + -768]
	mov r9, qword ptr [rbp + -776]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -672]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -784], r8
	mov r8, -1
	mov qword ptr [rbp + -792], r8
	mov r8, qword ptr [rbp + -784]
	mov r9, qword ptr [rbp + -792]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -672]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -800], r8
	mov r8, qword ptr [rbp + -632]
	mov r9, qword ptr [rbp + -800]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -72]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -808], r8
	mov r8, qword ptr [rbp + -808]
	mov r9, r8
	mov qword ptr [rbp + -816], r9
	mov r8, 216
	mov qword ptr [rbp + -824], r8
	mov r8, qword ptr [rbp + -824]
	mov r9, r8
	mov qword ptr [rbp + -832], r9
	mov r8, qword ptr [rbp + -832]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -840], r9
	mov r8, qword ptr [rbp + -840]
	mov r9, r8
	mov qword ptr [rbp + -848], r9
	mov r8, qword ptr [rbp + -848]
	mov r9, r8
	mov qword ptr [rbp + -856], r9
	mov r8, 26
	mov qword ptr [rbp + -864], r8
	mov r8, qword ptr [rbp + -856]
	mov r9, qword ptr [rbp + -864]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -872], r8
	mov r8, 69
	mov qword ptr [rbp + -880], r8
	mov r8, qword ptr [rbp + -872]
	mov r9, qword ptr [rbp + -880]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -888], r8
	mov r8, 75
	mov qword ptr [rbp + -896], r8
	mov r8, qword ptr [rbp + -888]
	mov r9, qword ptr [rbp + -896]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -904], r8
	mov r8, 77
	mov qword ptr [rbp + -912], r8
	mov r8, qword ptr [rbp + -904]
	mov r9, qword ptr [rbp + -912]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -920], r8
	mov r8, 70
	mov qword ptr [rbp + -928], r8
	mov r8, qword ptr [rbp + -920]
	mov r9, qword ptr [rbp + -928]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -936], r8
	mov r8, 76
	mov qword ptr [rbp + -944], r8
	mov r8, qword ptr [rbp + -936]
	mov r9, qword ptr [rbp + -944]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -952], r8
	mov r8, 71
	mov qword ptr [rbp + -960], r8
	mov r8, qword ptr [rbp + -952]
	mov r9, qword ptr [rbp + -960]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -968], r8
	mov r8, 68
	mov qword ptr [rbp + -976], r8
	mov r8, qword ptr [rbp + -968]
	mov r9, qword ptr [rbp + -976]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -984], r8
	mov r8, 81
	mov qword ptr [rbp + -992], r8
	mov r8, qword ptr [rbp + -984]
	mov r9, qword ptr [rbp + -992]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 72]
	mov qword ptr [rbp + -1000], r8
	mov r8, 86
	mov qword ptr [rbp + -1008], r8
	mov r8, qword ptr [rbp + -1000]
	mov r9, qword ptr [rbp + -1008]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 80]
	mov qword ptr [rbp + -1016], r8
	mov r8, 90
	mov qword ptr [rbp + -1024], r8
	mov r8, qword ptr [rbp + -1016]
	mov r9, qword ptr [rbp + -1024]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 88]
	mov qword ptr [rbp + -1032], r8
	mov r8, 78
	mov qword ptr [rbp + -1040], r8
	mov r8, qword ptr [rbp + -1032]
	mov r9, qword ptr [rbp + -1040]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 96]
	mov qword ptr [rbp + -1048], r8
	mov r8, 84
	mov qword ptr [rbp + -1056], r8
	mov r8, qword ptr [rbp + -1048]
	mov r9, qword ptr [rbp + -1056]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 104]
	mov qword ptr [rbp + -1064], r8
	mov r8, 79
	mov qword ptr [rbp + -1072], r8
	mov r8, qword ptr [rbp + -1064]
	mov r9, qword ptr [rbp + -1072]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 112]
	mov qword ptr [rbp + -1080], r8
	mov r8, 87
	mov qword ptr [rbp + -1088], r8
	mov r8, qword ptr [rbp + -1080]
	mov r9, qword ptr [rbp + -1088]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 120]
	mov qword ptr [rbp + -1096], r8
	mov r8, 89
	mov qword ptr [rbp + -1104], r8
	mov r8, qword ptr [rbp + -1096]
	mov r9, qword ptr [rbp + -1104]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 128]
	mov qword ptr [rbp + -1112], r8
	mov r8, 72
	mov qword ptr [rbp + -1120], r8
	mov r8, qword ptr [rbp + -1112]
	mov r9, qword ptr [rbp + -1120]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 136]
	mov qword ptr [rbp + -1128], r8
	mov r8, 88
	mov qword ptr [rbp + -1136], r8
	mov r8, qword ptr [rbp + -1128]
	mov r9, qword ptr [rbp + -1136]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 144]
	mov qword ptr [rbp + -1144], r8
	mov r8, 85
	mov qword ptr [rbp + -1152], r8
	mov r8, qword ptr [rbp + -1144]
	mov r9, qword ptr [rbp + -1152]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 152]
	mov qword ptr [rbp + -1160], r8
	mov r8, 83
	mov qword ptr [rbp + -1168], r8
	mov r8, qword ptr [rbp + -1160]
	mov r9, qword ptr [rbp + -1168]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 160]
	mov qword ptr [rbp + -1176], r8
	mov r8, 80
	mov qword ptr [rbp + -1184], r8
	mov r8, qword ptr [rbp + -1176]
	mov r9, qword ptr [rbp + -1184]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 168]
	mov qword ptr [rbp + -1192], r8
	mov r8, 65
	mov qword ptr [rbp + -1200], r8
	mov r8, qword ptr [rbp + -1192]
	mov r9, qword ptr [rbp + -1200]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 176]
	mov qword ptr [rbp + -1208], r8
	mov r8, 73
	mov qword ptr [rbp + -1216], r8
	mov r8, qword ptr [rbp + -1208]
	mov r9, qword ptr [rbp + -1216]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 184]
	mov qword ptr [rbp + -1224], r8
	mov r8, 66
	mov qword ptr [rbp + -1232], r8
	mov r8, qword ptr [rbp + -1224]
	mov r9, qword ptr [rbp + -1232]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 192]
	mov qword ptr [rbp + -1240], r8
	mov r8, 82
	mov qword ptr [rbp + -1248], r8
	mov r8, qword ptr [rbp + -1240]
	mov r9, qword ptr [rbp + -1248]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 200]
	mov qword ptr [rbp + -1256], r8
	mov r8, 67
	mov qword ptr [rbp + -1264], r8
	mov r8, qword ptr [rbp + -1256]
	mov r9, qword ptr [rbp + -1264]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 208]
	mov qword ptr [rbp + -1272], r8
	mov r8, 74
	mov qword ptr [rbp + -1280], r8
	mov r8, qword ptr [rbp + -1272]
	mov r9, qword ptr [rbp + -1280]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -856]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -1288], r8
	mov r8, qword ptr [rbp + -1288]
	mov r9, r8
	mov qword ptr [rbp + -1296], r9
	sub rsp, 8
	mov rdi, rsp
	mov r8, qword ptr [rbp + -1296]
	mov rsi, r8
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, rdx
	mov qword ptr [rbp + -1304], r8
	pop r8
	mov qword ptr [rbp + -1312], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -1320], r9
	mov r8, qword ptr [rbp + -1320]
	mov r9, r8
	mov qword ptr [rbp + -1328], r9
	mov r8, qword ptr [rbp + -1304]
	mov r9, r8
	mov qword ptr [rbp + -1336], r9
	mov r8, qword ptr [rbp + -1336]
	mov r9, r8
	mov qword ptr [rbp + -1344], r9
	mov r8, qword ptr [rbp + -1312]
	mov r9, r8
	mov qword ptr [rbp + -1352], r9
	mov r8, qword ptr [rbp + -1352]
	mov r9, r8
	mov qword ptr [rbp + -1360], r9
	mov r8, 216
	mov qword ptr [rbp + -1368], r8
	mov r8, qword ptr [rbp + -1368]
	mov r9, r8
	mov qword ptr [rbp + -1376], r9
	mov r8, qword ptr [rbp + -1376]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -1384], r9
	mov r8, qword ptr [rbp + -1384]
	mov r9, r8
	mov qword ptr [rbp + -1392], r9
	mov r8, qword ptr [rbp + -1392]
	mov r9, r8
	mov qword ptr [rbp + -1400], r9
	mov r8, 26
	mov qword ptr [rbp + -1408], r8
	mov r8, qword ptr [rbp + -1400]
	mov r9, qword ptr [rbp + -1408]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -1416], r8
	mov r8, 65
	mov qword ptr [rbp + -1424], r8
	mov r8, qword ptr [rbp + -1416]
	mov r9, qword ptr [rbp + -1424]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -1432], r8
	mov r8, 74
	mov qword ptr [rbp + -1440], r8
	mov r8, qword ptr [rbp + -1432]
	mov r9, qword ptr [rbp + -1440]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -1448], r8
	mov r8, 68
	mov qword ptr [rbp + -1456], r8
	mov r8, qword ptr [rbp + -1448]
	mov r9, qword ptr [rbp + -1456]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -1464], r8
	mov r8, 75
	mov qword ptr [rbp + -1472], r8
	mov r8, qword ptr [rbp + -1464]
	mov r9, qword ptr [rbp + -1472]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -1480], r8
	mov r8, 83
	mov qword ptr [rbp + -1488], r8
	mov r8, qword ptr [rbp + -1480]
	mov r9, qword ptr [rbp + -1488]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -1496], r8
	mov r8, 73
	mov qword ptr [rbp + -1504], r8
	mov r8, qword ptr [rbp + -1496]
	mov r9, qword ptr [rbp + -1504]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -1512], r8
	mov r8, 82
	mov qword ptr [rbp + -1520], r8
	mov r8, qword ptr [rbp + -1512]
	mov r9, qword ptr [rbp + -1520]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -1528], r8
	mov r8, 85
	mov qword ptr [rbp + -1536], r8
	mov r8, qword ptr [rbp + -1528]
	mov r9, qword ptr [rbp + -1536]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 72]
	mov qword ptr [rbp + -1544], r8
	mov r8, 88
	mov qword ptr [rbp + -1552], r8
	mov r8, qword ptr [rbp + -1544]
	mov r9, qword ptr [rbp + -1552]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 80]
	mov qword ptr [rbp + -1560], r8
	mov r8, 66
	mov qword ptr [rbp + -1568], r8
	mov r8, qword ptr [rbp + -1560]
	mov r9, qword ptr [rbp + -1568]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 88]
	mov qword ptr [rbp + -1576], r8
	mov r8, 76
	mov qword ptr [rbp + -1584], r8
	mov r8, qword ptr [rbp + -1576]
	mov r9, qword ptr [rbp + -1584]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 96]
	mov qword ptr [rbp + -1592], r8
	mov r8, 72
	mov qword ptr [rbp + -1600], r8
	mov r8, qword ptr [rbp + -1592]
	mov r9, qword ptr [rbp + -1600]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 104]
	mov qword ptr [rbp + -1608], r8
	mov r8, 87
	mov qword ptr [rbp + -1616], r8
	mov r8, qword ptr [rbp + -1608]
	mov r9, qword ptr [rbp + -1616]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 112]
	mov qword ptr [rbp + -1624], r8
	mov r8, 84
	mov qword ptr [rbp + -1632], r8
	mov r8, qword ptr [rbp + -1624]
	mov r9, qword ptr [rbp + -1632]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 120]
	mov qword ptr [rbp + -1640], r8
	mov r8, 77
	mov qword ptr [rbp + -1648], r8
	mov r8, qword ptr [rbp + -1640]
	mov r9, qword ptr [rbp + -1648]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 128]
	mov qword ptr [rbp + -1656], r8
	mov r8, 67
	mov qword ptr [rbp + -1664], r8
	mov r8, qword ptr [rbp + -1656]
	mov r9, qword ptr [rbp + -1664]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 136]
	mov qword ptr [rbp + -1672], r8
	mov r8, 81
	mov qword ptr [rbp + -1680], r8
	mov r8, qword ptr [rbp + -1672]
	mov r9, qword ptr [rbp + -1680]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 144]
	mov qword ptr [rbp + -1688], r8
	mov r8, 71
	mov qword ptr [rbp + -1696], r8
	mov r8, qword ptr [rbp + -1688]
	mov r9, qword ptr [rbp + -1696]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 152]
	mov qword ptr [rbp + -1704], r8
	mov r8, 90
	mov qword ptr [rbp + -1712], r8
	mov r8, qword ptr [rbp + -1704]
	mov r9, qword ptr [rbp + -1712]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 160]
	mov qword ptr [rbp + -1720], r8
	mov r8, 78
	mov qword ptr [rbp + -1728], r8
	mov r8, qword ptr [rbp + -1720]
	mov r9, qword ptr [rbp + -1728]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 168]
	mov qword ptr [rbp + -1736], r8
	mov r8, 80
	mov qword ptr [rbp + -1744], r8
	mov r8, qword ptr [rbp + -1736]
	mov r9, qword ptr [rbp + -1744]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 176]
	mov qword ptr [rbp + -1752], r8
	mov r8, 89
	mov qword ptr [rbp + -1760], r8
	mov r8, qword ptr [rbp + -1752]
	mov r9, qword ptr [rbp + -1760]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 184]
	mov qword ptr [rbp + -1768], r8
	mov r8, 70
	mov qword ptr [rbp + -1776], r8
	mov r8, qword ptr [rbp + -1768]
	mov r9, qword ptr [rbp + -1776]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 192]
	mov qword ptr [rbp + -1784], r8
	mov r8, 86
	mov qword ptr [rbp + -1792], r8
	mov r8, qword ptr [rbp + -1784]
	mov r9, qword ptr [rbp + -1792]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 200]
	mov qword ptr [rbp + -1800], r8
	mov r8, 79
	mov qword ptr [rbp + -1808], r8
	mov r8, qword ptr [rbp + -1800]
	mov r9, qword ptr [rbp + -1808]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 208]
	mov qword ptr [rbp + -1816], r8
	mov r8, 69
	mov qword ptr [rbp + -1824], r8
	mov r8, qword ptr [rbp + -1816]
	mov r9, qword ptr [rbp + -1824]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -1832], r8
	mov r8, qword ptr [rbp + -1832]
	mov r9, r8
	mov qword ptr [rbp + -1840], r9
	sub rsp, 8
	mov rdi, rsp
	mov r8, qword ptr [rbp + -1840]
	mov rsi, r8
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, rdx
	mov qword ptr [rbp + -1304], r8
	pop r8
	mov qword ptr [rbp + -1312], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -1848], r9
	mov r8, qword ptr [rbp + -1848]
	mov r9, r8
	mov qword ptr [rbp + -1856], r9
	mov r8, qword ptr [rbp + -1304]
	mov r9, r8
	mov qword ptr [rbp + -1864], r9
	mov r8, qword ptr [rbp + -1864]
	mov r9, r8
	mov qword ptr [rbp + -1872], r9
	mov r8, qword ptr [rbp + -1312]
	mov r9, r8
	mov qword ptr [rbp + -1880], r9
	mov r8, qword ptr [rbp + -1880]
	mov r9, r8
	mov qword ptr [rbp + -1888], r9
	mov r8, 216
	mov qword ptr [rbp + -1896], r8
	mov r8, qword ptr [rbp + -1896]
	mov r9, r8
	mov qword ptr [rbp + -1904], r9
	mov r8, qword ptr [rbp + -1904]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -1912], r9
	mov r8, qword ptr [rbp + -1912]
	mov r9, r8
	mov qword ptr [rbp + -1920], r9
	mov r8, qword ptr [rbp + -1920]
	mov r9, r8
	mov qword ptr [rbp + -1928], r9
	mov r8, 26
	mov qword ptr [rbp + -1936], r8
	mov r8, qword ptr [rbp + -1928]
	mov r9, qword ptr [rbp + -1936]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -1944], r8
	mov r8, 66
	mov qword ptr [rbp + -1952], r8
	mov r8, qword ptr [rbp + -1944]
	mov r9, qword ptr [rbp + -1952]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -1960], r8
	mov r8, 68
	mov qword ptr [rbp + -1968], r8
	mov r8, qword ptr [rbp + -1960]
	mov r9, qword ptr [rbp + -1968]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -1976], r8
	mov r8, 70
	mov qword ptr [rbp + -1984], r8
	mov r8, qword ptr [rbp + -1976]
	mov r9, qword ptr [rbp + -1984]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -1992], r8
	mov r8, 72
	mov qword ptr [rbp + -2000], r8
	mov r8, qword ptr [rbp + -1992]
	mov r9, qword ptr [rbp + -2000]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -2008], r8
	mov r8, 74
	mov qword ptr [rbp + -2016], r8
	mov r8, qword ptr [rbp + -2008]
	mov r9, qword ptr [rbp + -2016]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -2024], r8
	mov r8, 76
	mov qword ptr [rbp + -2032], r8
	mov r8, qword ptr [rbp + -2024]
	mov r9, qword ptr [rbp + -2032]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -2040], r8
	mov r8, 67
	mov qword ptr [rbp + -2048], r8
	mov r8, qword ptr [rbp + -2040]
	mov r9, qword ptr [rbp + -2048]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -2056], r8
	mov r8, 80
	mov qword ptr [rbp + -2064], r8
	mov r8, qword ptr [rbp + -2056]
	mov r9, qword ptr [rbp + -2064]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 72]
	mov qword ptr [rbp + -2072], r8
	mov r8, 82
	mov qword ptr [rbp + -2080], r8
	mov r8, qword ptr [rbp + -2072]
	mov r9, qword ptr [rbp + -2080]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 80]
	mov qword ptr [rbp + -2088], r8
	mov r8, 84
	mov qword ptr [rbp + -2096], r8
	mov r8, qword ptr [rbp + -2088]
	mov r9, qword ptr [rbp + -2096]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 88]
	mov qword ptr [rbp + -2104], r8
	mov r8, 88
	mov qword ptr [rbp + -2112], r8
	mov r8, qword ptr [rbp + -2104]
	mov r9, qword ptr [rbp + -2112]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 96]
	mov qword ptr [rbp + -2120], r8
	mov r8, 86
	mov qword ptr [rbp + -2128], r8
	mov r8, qword ptr [rbp + -2120]
	mov r9, qword ptr [rbp + -2128]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 104]
	mov qword ptr [rbp + -2136], r8
	mov r8, 90
	mov qword ptr [rbp + -2144], r8
	mov r8, qword ptr [rbp + -2136]
	mov r9, qword ptr [rbp + -2144]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 112]
	mov qword ptr [rbp + -2152], r8
	mov r8, 78
	mov qword ptr [rbp + -2160], r8
	mov r8, qword ptr [rbp + -2152]
	mov r9, qword ptr [rbp + -2160]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 120]
	mov qword ptr [rbp + -2168], r8
	mov r8, 89
	mov qword ptr [rbp + -2176], r8
	mov r8, qword ptr [rbp + -2168]
	mov r9, qword ptr [rbp + -2176]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 128]
	mov qword ptr [rbp + -2184], r8
	mov r8, 69
	mov qword ptr [rbp + -2192], r8
	mov r8, qword ptr [rbp + -2184]
	mov r9, qword ptr [rbp + -2192]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 136]
	mov qword ptr [rbp + -2200], r8
	mov r8, 73
	mov qword ptr [rbp + -2208], r8
	mov r8, qword ptr [rbp + -2200]
	mov r9, qword ptr [rbp + -2208]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 144]
	mov qword ptr [rbp + -2216], r8
	mov r8, 87
	mov qword ptr [rbp + -2224], r8
	mov r8, qword ptr [rbp + -2216]
	mov r9, qword ptr [rbp + -2224]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 152]
	mov qword ptr [rbp + -2232], r8
	mov r8, 71
	mov qword ptr [rbp + -2240], r8
	mov r8, qword ptr [rbp + -2232]
	mov r9, qword ptr [rbp + -2240]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 160]
	mov qword ptr [rbp + -2248], r8
	mov r8, 65
	mov qword ptr [rbp + -2256], r8
	mov r8, qword ptr [rbp + -2248]
	mov r9, qword ptr [rbp + -2256]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 168]
	mov qword ptr [rbp + -2264], r8
	mov r8, 75
	mov qword ptr [rbp + -2272], r8
	mov r8, qword ptr [rbp + -2264]
	mov r9, qword ptr [rbp + -2272]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 176]
	mov qword ptr [rbp + -2280], r8
	mov r8, 77
	mov qword ptr [rbp + -2288], r8
	mov r8, qword ptr [rbp + -2280]
	mov r9, qword ptr [rbp + -2288]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 184]
	mov qword ptr [rbp + -2296], r8
	mov r8, 85
	mov qword ptr [rbp + -2304], r8
	mov r8, qword ptr [rbp + -2296]
	mov r9, qword ptr [rbp + -2304]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 192]
	mov qword ptr [rbp + -2312], r8
	mov r8, 83
	mov qword ptr [rbp + -2320], r8
	mov r8, qword ptr [rbp + -2312]
	mov r9, qword ptr [rbp + -2320]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 200]
	mov qword ptr [rbp + -2328], r8
	mov r8, 81
	mov qword ptr [rbp + -2336], r8
	mov r8, qword ptr [rbp + -2328]
	mov r9, qword ptr [rbp + -2336]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 208]
	mov qword ptr [rbp + -2344], r8
	mov r8, 79
	mov qword ptr [rbp + -2352], r8
	mov r8, qword ptr [rbp + -2344]
	mov r9, qword ptr [rbp + -2352]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -1928]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -2360], r8
	mov r8, qword ptr [rbp + -2360]
	mov r9, r8
	mov qword ptr [rbp + -2368], r9
	sub rsp, 8
	mov rdi, rsp
	mov r8, qword ptr [rbp + -2368]
	mov rsi, r8
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, rdx
	mov qword ptr [rbp + -1304], r8
	pop r8
	mov qword ptr [rbp + -1312], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -2376], r9
	mov r8, qword ptr [rbp + -2376]
	mov r9, r8
	mov qword ptr [rbp + -2384], r9
	mov r8, qword ptr [rbp + -1304]
	mov r9, r8
	mov qword ptr [rbp + -2392], r9
	mov r8, qword ptr [rbp + -2392]
	mov r9, r8
	mov qword ptr [rbp + -2400], r9
	mov r8, qword ptr [rbp + -1312]
	mov r9, r8
	mov qword ptr [rbp + -2408], r9
	mov r8, qword ptr [rbp + -2408]
	mov r9, r8
	mov qword ptr [rbp + -2416], r9
	mov r8, 216
	mov qword ptr [rbp + -2424], r8
	mov r8, qword ptr [rbp + -2424]
	mov r9, r8
	mov qword ptr [rbp + -2432], r9
	mov r8, qword ptr [rbp + -2432]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -2440], r9
	mov r8, qword ptr [rbp + -2440]
	mov r9, r8
	mov qword ptr [rbp + -2448], r9
	mov r8, qword ptr [rbp + -2448]
	mov r9, r8
	mov qword ptr [rbp + -2456], r9
	mov r8, 26
	mov qword ptr [rbp + -2464], r8
	mov r8, qword ptr [rbp + -2456]
	mov r9, qword ptr [rbp + -2464]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -2472], r8
	mov r8, 89
	mov qword ptr [rbp + -2480], r8
	mov r8, qword ptr [rbp + -2472]
	mov r9, qword ptr [rbp + -2480]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -2488], r8
	mov r8, 82
	mov qword ptr [rbp + -2496], r8
	mov r8, qword ptr [rbp + -2488]
	mov r9, qword ptr [rbp + -2496]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -2504], r8
	mov r8, 85
	mov qword ptr [rbp + -2512], r8
	mov r8, qword ptr [rbp + -2504]
	mov r9, qword ptr [rbp + -2512]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -2520], r8
	mov r8, 72
	mov qword ptr [rbp + -2528], r8
	mov r8, qword ptr [rbp + -2520]
	mov r9, qword ptr [rbp + -2528]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -2536], r8
	mov r8, 81
	mov qword ptr [rbp + -2544], r8
	mov r8, qword ptr [rbp + -2536]
	mov r9, qword ptr [rbp + -2544]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -2552], r8
	mov r8, 83
	mov qword ptr [rbp + -2560], r8
	mov r8, qword ptr [rbp + -2552]
	mov r9, qword ptr [rbp + -2560]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -2568], r8
	mov r8, 76
	mov qword ptr [rbp + -2576], r8
	mov r8, qword ptr [rbp + -2568]
	mov r9, qword ptr [rbp + -2576]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -2584], r8
	mov r8, 68
	mov qword ptr [rbp + -2592], r8
	mov r8, qword ptr [rbp + -2584]
	mov r9, qword ptr [rbp + -2592]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 72]
	mov qword ptr [rbp + -2600], r8
	mov r8, 80
	mov qword ptr [rbp + -2608], r8
	mov r8, qword ptr [rbp + -2600]
	mov r9, qword ptr [rbp + -2608]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 80]
	mov qword ptr [rbp + -2616], r8
	mov r8, 88
	mov qword ptr [rbp + -2624], r8
	mov r8, qword ptr [rbp + -2616]
	mov r9, qword ptr [rbp + -2624]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 88]
	mov qword ptr [rbp + -2632], r8
	mov r8, 78
	mov qword ptr [rbp + -2640], r8
	mov r8, qword ptr [rbp + -2632]
	mov r9, qword ptr [rbp + -2640]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 96]
	mov qword ptr [rbp + -2648], r8
	mov r8, 71
	mov qword ptr [rbp + -2656], r8
	mov r8, qword ptr [rbp + -2648]
	mov r9, qword ptr [rbp + -2656]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 104]
	mov qword ptr [rbp + -2664], r8
	mov r8, 79
	mov qword ptr [rbp + -2672], r8
	mov r8, qword ptr [rbp + -2664]
	mov r9, qword ptr [rbp + -2672]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 112]
	mov qword ptr [rbp + -2680], r8
	mov r8, 75
	mov qword ptr [rbp + -2688], r8
	mov r8, qword ptr [rbp + -2680]
	mov r9, qword ptr [rbp + -2688]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 120]
	mov qword ptr [rbp + -2696], r8
	mov r8, 77
	mov qword ptr [rbp + -2704], r8
	mov r8, qword ptr [rbp + -2696]
	mov r9, qword ptr [rbp + -2704]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 128]
	mov qword ptr [rbp + -2712], r8
	mov r8, 73
	mov qword ptr [rbp + -2720], r8
	mov r8, qword ptr [rbp + -2712]
	mov r9, qword ptr [rbp + -2720]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 136]
	mov qword ptr [rbp + -2728], r8
	mov r8, 69
	mov qword ptr [rbp + -2736], r8
	mov r8, qword ptr [rbp + -2728]
	mov r9, qword ptr [rbp + -2736]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 144]
	mov qword ptr [rbp + -2744], r8
	mov r8, 66
	mov qword ptr [rbp + -2752], r8
	mov r8, qword ptr [rbp + -2744]
	mov r9, qword ptr [rbp + -2752]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 152]
	mov qword ptr [rbp + -2760], r8
	mov r8, 70
	mov qword ptr [rbp + -2768], r8
	mov r8, qword ptr [rbp + -2760]
	mov r9, qword ptr [rbp + -2768]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 160]
	mov qword ptr [rbp + -2776], r8
	mov r8, 90
	mov qword ptr [rbp + -2784], r8
	mov r8, qword ptr [rbp + -2776]
	mov r9, qword ptr [rbp + -2784]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 168]
	mov qword ptr [rbp + -2792], r8
	mov r8, 67
	mov qword ptr [rbp + -2800], r8
	mov r8, qword ptr [rbp + -2792]
	mov r9, qword ptr [rbp + -2800]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 176]
	mov qword ptr [rbp + -2808], r8
	mov r8, 87
	mov qword ptr [rbp + -2816], r8
	mov r8, qword ptr [rbp + -2808]
	mov r9, qword ptr [rbp + -2816]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 184]
	mov qword ptr [rbp + -2824], r8
	mov r8, 86
	mov qword ptr [rbp + -2832], r8
	mov r8, qword ptr [rbp + -2824]
	mov r9, qword ptr [rbp + -2832]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 192]
	mov qword ptr [rbp + -2840], r8
	mov r8, 74
	mov qword ptr [rbp + -2848], r8
	mov r8, qword ptr [rbp + -2840]
	mov r9, qword ptr [rbp + -2848]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 200]
	mov qword ptr [rbp + -2856], r8
	mov r8, 65
	mov qword ptr [rbp + -2864], r8
	mov r8, qword ptr [rbp + -2856]
	mov r9, qword ptr [rbp + -2864]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 208]
	mov qword ptr [rbp + -2872], r8
	mov r8, 84
	mov qword ptr [rbp + -2880], r8
	mov r8, qword ptr [rbp + -2872]
	mov r9, qword ptr [rbp + -2880]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -2456]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -2888], r8
	mov r8, qword ptr [rbp + -2888]
	mov r9, r8
	mov qword ptr [rbp + -2896], r9
	mov r8, qword ptr [rbp + -2896]
	mov rdi, r8
	and rsp, -16
	call _ImakeReflector_aiai
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -2904], r9
	mov r8, qword ptr [rbp + -2904]
	mov r9, r8
	mov qword ptr [rbp + -2912], r9
	mov r8, qword ptr [rbp + -2912]
	mov r9, r8
	mov qword ptr [rbp + -2920], r9
	mov r8, 0
	mov qword ptr [rbp + -2928], r8
	mov r8, qword ptr [rbp + -2928]
	mov r9, r8
	mov qword ptr [rbp + -2936], r9
	_l83:
	mov r8, 17576
	mov qword ptr [rbp + -2944], r8
	mov r8, qword ptr [rbp + -2936]
	mov r9, qword ptr [rbp + -2944]
	cmp r8, r9
	jge _l81
	_l82:
	mov r8, 0
	mov qword ptr [rbp + -2952], r8
	mov r8, qword ptr [rbp + -2952]
	mov r9, r8
	mov qword ptr [rbp + -2960], r9
	_l86:
	mov r8, 26
	mov qword ptr [rbp + -2968], r8
	mov r8, qword ptr [rbp + -2960]
	mov r9, qword ptr [rbp + -2968]
	cmp r8, r9
	jge _l84
	_l85:
	mov r8, 1
	mov qword ptr [rbp + -2976], r8
	mov r8, qword ptr [rbp + -2976]
	mov r9, r8
	mov qword ptr [rbp + -2984], r9
	mov r8, 0
	mov qword ptr [rbp + -2992], r8
	mov r8, qword ptr [rbp + -2992]
	mov r9, r8
	mov qword ptr [rbp + -3000], r9
	_l89:
	mov r8, 8
	mov qword ptr [rbp + -3008], r8
	mov r8, qword ptr [rbp + -816]
	mov r9, r8
	mov qword ptr [rbp + -3016], r9
	mov r8, qword ptr [rbp + -3016]
	mov r9, qword ptr [rbp + -3008]
	sub r8, r9
	mov qword ptr [rbp + -3016], r8
	mov r8, qword ptr [rbp + -3016]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3024], r8
	mov r8, qword ptr [rbp + -3000]
	mov r9, qword ptr [rbp + -3024]
	cmp r8, r9
	jge _l87
	_l88:
	mov r8, qword ptr [rbp + -2960]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, 0
	mov qword ptr [rbp + -3040], r8
	mov r8, qword ptr [rbp + -3040]
	mov r9, r8
	mov qword ptr [rbp + -3048], r9
	_l92:
	mov r8, qword ptr [rbp + -816]
	mov r9, r8
	mov qword ptr [rbp + -3056], r9
	mov r8, qword ptr [rbp + -3000]
	mov r9, r8
	mov qword ptr [rbp + -3064], r9
	mov r8, 8
	mov qword ptr [rbp + -3072], r8
	mov r8, qword ptr [rbp + -3056]
	mov r9, r8
	mov qword ptr [rbp + -3080], r9
	mov r8, qword ptr [rbp + -3080]
	mov r9, qword ptr [rbp + -3072]
	sub r8, r9
	mov qword ptr [rbp + -3080], r8
	mov r8, qword ptr [rbp + -3080]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3088], r8
	mov r8, qword ptr [rbp + -3064]
	mov r9, qword ptr [rbp + -3088]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -3096], r8
	mov r8, 1
	mov qword ptr [rbp + -3104], r8
	mov r8, qword ptr [rbp + -3096]
	mov r9, r8
	mov qword ptr [rbp + -3112], r9
	mov r8, qword ptr [rbp + -3112]
	mov r9, qword ptr [rbp + -3104]
	xor r8, r9
	mov qword ptr [rbp + -3112], r8
	mov r8, qword ptr [rbp + -3112]
	mov r9, qword ptr [rbp + -3112]
	test r8, r9
	jnz _l93
	_l94:
	mov r8, qword ptr [rbp + -3056]
	mov r9, qword ptr [rbp + -3064]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -3120], r8
	mov r8, qword ptr [rbp + -3120]
	mov r9, r8
	mov qword ptr [rbp + -3128], r9
	mov r8, qword ptr [rbp + -3048]
	mov r9, r8
	mov qword ptr [rbp + -3136], r9
	mov r8, 8
	mov qword ptr [rbp + -3144], r8
	mov r8, qword ptr [rbp + -3128]
	mov r9, r8
	mov qword ptr [rbp + -3152], r9
	mov r8, qword ptr [rbp + -3152]
	mov r9, qword ptr [rbp + -3144]
	sub r8, r9
	mov qword ptr [rbp + -3152], r8
	mov r8, qword ptr [rbp + -3152]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3160], r8
	mov r8, qword ptr [rbp + -3136]
	mov r9, qword ptr [rbp + -3160]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -3168], r8
	mov r8, 1
	mov qword ptr [rbp + -3176], r8
	mov r8, qword ptr [rbp + -3168]
	mov r9, r8
	mov qword ptr [rbp + -3184], r9
	mov r8, qword ptr [rbp + -3184]
	mov r9, qword ptr [rbp + -3176]
	xor r8, r9
	mov qword ptr [rbp + -3184], r8
	mov r8, qword ptr [rbp + -3184]
	mov r9, qword ptr [rbp + -3184]
	test r8, r9
	jnz _l95
	_l96:
	mov r8, qword ptr [rbp + -3128]
	mov r9, qword ptr [rbp + -3136]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -3192], r8
	mov r8, -1
	mov qword ptr [rbp + -3200], r8
	mov r8, qword ptr [rbp + -3192]
	mov r9, qword ptr [rbp + -3200]
	cmp r8, r9
	je _l90
	_l91:
	mov r8, qword ptr [rbp + -816]
	mov r9, r8
	mov qword ptr [rbp + -3208], r9
	mov r8, qword ptr [rbp + -3000]
	mov r9, r8
	mov qword ptr [rbp + -3216], r9
	mov r8, 8
	mov qword ptr [rbp + -3224], r8
	mov r8, qword ptr [rbp + -3208]
	mov r9, r8
	mov qword ptr [rbp + -3232], r9
	mov r8, qword ptr [rbp + -3232]
	mov r9, qword ptr [rbp + -3224]
	sub r8, r9
	mov qword ptr [rbp + -3232], r8
	mov r8, qword ptr [rbp + -3232]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3240], r8
	mov r8, qword ptr [rbp + -3216]
	mov r9, qword ptr [rbp + -3240]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -3248], r8
	mov r8, 1
	mov qword ptr [rbp + -3256], r8
	mov r8, qword ptr [rbp + -3248]
	mov r9, r8
	mov qword ptr [rbp + -3264], r9
	mov r8, qword ptr [rbp + -3264]
	mov r9, qword ptr [rbp + -3256]
	xor r8, r9
	mov qword ptr [rbp + -3264], r8
	mov r8, qword ptr [rbp + -3264]
	mov r9, qword ptr [rbp + -3264]
	test r8, r9
	jnz _l97
	_l98:
	mov r8, qword ptr [rbp + -3208]
	mov r9, qword ptr [rbp + -3216]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -3272], r8
	mov r8, qword ptr [rbp + -3272]
	mov r9, r8
	mov qword ptr [rbp + -3280], r9
	mov r8, qword ptr [rbp + -3048]
	mov r9, r8
	mov qword ptr [rbp + -3288], r9
	mov r8, 8
	mov qword ptr [rbp + -3296], r8
	mov r8, qword ptr [rbp + -3280]
	mov r9, r8
	mov qword ptr [rbp + -3304], r9
	mov r8, qword ptr [rbp + -3304]
	mov r9, qword ptr [rbp + -3296]
	sub r8, r9
	mov qword ptr [rbp + -3304], r8
	mov r8, qword ptr [rbp + -3304]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3312], r8
	mov r8, qword ptr [rbp + -3288]
	mov r9, qword ptr [rbp + -3312]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -3320], r8
	mov r8, 1
	mov qword ptr [rbp + -3328], r8
	mov r8, qword ptr [rbp + -3320]
	mov r9, r8
	mov qword ptr [rbp + -3336], r9
	mov r8, qword ptr [rbp + -3336]
	mov r9, qword ptr [rbp + -3328]
	xor r8, r9
	mov qword ptr [rbp + -3336], r8
	mov r8, qword ptr [rbp + -3336]
	mov r9, qword ptr [rbp + -3336]
	test r8, r9
	jnz _l99
	_l100:
	mov r8, qword ptr [rbp + -3280]
	mov r9, qword ptr [rbp + -3288]
	mov r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -3344], r8
	mov r8, qword ptr [rbp + -2936]
	mov r9, qword ptr [rbp + -3344]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3352], r8
	mov r8, qword ptr [rbp + -3352]
	mov r9, r8
	mov qword ptr [rbp + -3360], r9
	mov r8, 26
	mov qword ptr [rbp + -3368], r8
	mov r8, qword ptr [rbp + -3360]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -3368]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -3360], r8
	mov r8, qword ptr [rbp + -3360]
	mov r9, r8
	mov qword ptr [rbp + -1360], r9
	mov r8, 26
	mov qword ptr [rbp + -3376], r8
	mov r8, qword ptr [rbp + -3360]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -3376]
	idiv r8
	mov r8, rax
	mov qword ptr [rbp + -3360], r8
	mov r8, 26
	mov qword ptr [rbp + -3384], r8
	mov r8, qword ptr [rbp + -3360]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -3384]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -3360], r8
	mov r8, qword ptr [rbp + -3360]
	mov r9, r8
	mov qword ptr [rbp + -1888], r9
	mov r8, 676
	mov qword ptr [rbp + -3392], r8
	mov r8, qword ptr [rbp + -3360]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -3392]
	idiv r8
	mov r8, rax
	mov qword ptr [rbp + -3360], r8
	mov r8, 26
	mov qword ptr [rbp + -3400], r8
	mov r8, qword ptr [rbp + -3360]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -3400]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -3360], r8
	mov r8, qword ptr [rbp + -3360]
	mov r9, r8
	mov qword ptr [rbp + -2416], r9
	mov r8, qword ptr [rbp + -1328]
	mov r9, r8
	mov qword ptr [rbp + -3408], r9
	mov r8, qword ptr [rbp + -1344]
	mov r9, r8
	mov qword ptr [rbp + -3416], r9
	mov r8, qword ptr [rbp + -1360]
	mov r9, r8
	mov qword ptr [rbp + -3424], r9
	mov r8, qword ptr [rbp + -3032]
	mov r9, r8
	mov qword ptr [rbp + -3432], r9
	mov r8, qword ptr [rbp + -3432]
	mov rcx, r8
	mov r8, qword ptr [rbp + -3424]
	mov rdx, r8
	mov r8, qword ptr [rbp + -3416]
	mov rsi, r8
	mov r8, qword ptr [rbp + -3408]
	mov rdi, r8
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -3440], r9
	mov r8, qword ptr [rbp + -3440]
	mov r9, r8
	mov qword ptr [rbp + -3448], r9
	mov r8, qword ptr [rbp + -3448]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, qword ptr [rbp + -1856]
	mov r9, r8
	mov qword ptr [rbp + -3456], r9
	mov r8, qword ptr [rbp + -1872]
	mov r9, r8
	mov qword ptr [rbp + -3464], r9
	mov r8, qword ptr [rbp + -1888]
	mov r9, r8
	mov qword ptr [rbp + -3472], r9
	mov r8, qword ptr [rbp + -3032]
	mov r9, r8
	mov qword ptr [rbp + -3480], r9
	mov r8, qword ptr [rbp + -3480]
	mov rcx, r8
	mov r8, qword ptr [rbp + -3472]
	mov rdx, r8
	mov r8, qword ptr [rbp + -3464]
	mov rsi, r8
	mov r8, qword ptr [rbp + -3456]
	mov rdi, r8
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -3488], r9
	mov r8, qword ptr [rbp + -3488]
	mov r9, r8
	mov qword ptr [rbp + -3496], r9
	mov r8, qword ptr [rbp + -3496]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, qword ptr [rbp + -2384]
	mov r9, r8
	mov qword ptr [rbp + -3504], r9
	mov r8, qword ptr [rbp + -2400]
	mov r9, r8
	mov qword ptr [rbp + -3512], r9
	mov r8, qword ptr [rbp + -2416]
	mov r9, r8
	mov qword ptr [rbp + -3520], r9
	mov r8, qword ptr [rbp + -3032]
	mov r9, r8
	mov qword ptr [rbp + -3528], r9
	mov r8, qword ptr [rbp + -3528]
	mov rcx, r8
	mov r8, qword ptr [rbp + -3520]
	mov rdx, r8
	mov r8, qword ptr [rbp + -3512]
	mov rsi, r8
	mov r8, qword ptr [rbp + -3504]
	mov rdi, r8
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -3536], r9
	mov r8, qword ptr [rbp + -3536]
	mov r9, r8
	mov qword ptr [rbp + -3544], r9
	mov r8, qword ptr [rbp + -3544]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, qword ptr [rbp + -2920]
	mov r9, r8
	mov qword ptr [rbp + -3552], r9
	mov r8, qword ptr [rbp + -3032]
	mov r9, r8
	mov qword ptr [rbp + -3560], r9
	mov r8, qword ptr [rbp + -3560]
	mov rsi, r8
	mov r8, qword ptr [rbp + -3552]
	mov rdi, r8
	and rsp, -16
	call _IreflectorEncrypt_iaii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -3568], r9
	mov r8, qword ptr [rbp + -3568]
	mov r9, r8
	mov qword ptr [rbp + -3576], r9
	mov r8, qword ptr [rbp + -3576]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, qword ptr [rbp + -2384]
	mov r9, r8
	mov qword ptr [rbp + -3584], r9
	mov r8, qword ptr [rbp + -2400]
	mov r9, r8
	mov qword ptr [rbp + -3592], r9
	mov r8, qword ptr [rbp + -2416]
	mov r9, r8
	mov qword ptr [rbp + -3600], r9
	mov r8, qword ptr [rbp + -3032]
	mov r9, r8
	mov qword ptr [rbp + -3608], r9
	mov r8, qword ptr [rbp + -3608]
	mov rcx, r8
	mov r8, qword ptr [rbp + -3600]
	mov rdx, r8
	mov r8, qword ptr [rbp + -3592]
	mov rsi, r8
	mov r8, qword ptr [rbp + -3584]
	mov rdi, r8
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -3616], r9
	mov r8, qword ptr [rbp + -3616]
	mov r9, r8
	mov qword ptr [rbp + -3624], r9
	mov r8, qword ptr [rbp + -3624]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, qword ptr [rbp + -1856]
	mov r9, r8
	mov qword ptr [rbp + -3632], r9
	mov r8, qword ptr [rbp + -1872]
	mov r9, r8
	mov qword ptr [rbp + -3640], r9
	mov r8, qword ptr [rbp + -1888]
	mov r9, r8
	mov qword ptr [rbp + -3648], r9
	mov r8, qword ptr [rbp + -3032]
	mov r9, r8
	mov qword ptr [rbp + -3656], r9
	mov r8, qword ptr [rbp + -3656]
	mov rcx, r8
	mov r8, qword ptr [rbp + -3648]
	mov rdx, r8
	mov r8, qword ptr [rbp + -3640]
	mov rsi, r8
	mov r8, qword ptr [rbp + -3632]
	mov rdi, r8
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -3664], r9
	mov r8, qword ptr [rbp + -3664]
	mov r9, r8
	mov qword ptr [rbp + -3672], r9
	mov r8, qword ptr [rbp + -3672]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, qword ptr [rbp + -1328]
	mov r9, r8
	mov qword ptr [rbp + -3680], r9
	mov r8, qword ptr [rbp + -1344]
	mov r9, r8
	mov qword ptr [rbp + -3688], r9
	mov r8, qword ptr [rbp + -1360]
	mov r9, r8
	mov qword ptr [rbp + -3696], r9
	mov r8, qword ptr [rbp + -3032]
	mov r9, r8
	mov qword ptr [rbp + -3704], r9
	mov r8, qword ptr [rbp + -3704]
	mov rcx, r8
	mov r8, qword ptr [rbp + -3696]
	mov rdx, r8
	mov r8, qword ptr [rbp + -3688]
	mov rsi, r8
	mov r8, qword ptr [rbp + -3680]
	mov rdi, r8
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -3712], r9
	mov r8, qword ptr [rbp + -3712]
	mov r9, r8
	mov qword ptr [rbp + -3720], r9
	mov r8, qword ptr [rbp + -3720]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, qword ptr [rbp + -3048]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -3728], r8
	mov r8, qword ptr [rbp + -3728]
	mov r9, r8
	mov qword ptr [rbp + -3048], r9
	jmp _l92
	_l99:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l100
	_l97:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l98
	_l90:
	mov r8, qword ptr [rbp + -3032]
	mov r9, qword ptr [rbp + -2960]
	cmp r8, r9
	je _l101
	_l102:
	mov r8, 0
	mov qword ptr [rbp + -3736], r8
	mov r8, qword ptr [rbp + -3736]
	mov r9, r8
	mov qword ptr [rbp + -2984], r9
	_l101:
	mov r8, qword ptr [rbp + -3000]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -3744], r8
	mov r8, qword ptr [rbp + -3744]
	mov r9, r8
	mov qword ptr [rbp + -3000], r9
	jmp _l89
	_l95:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l96
	_l93:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l94
	_l87:
	mov r8, 1
	mov qword ptr [rbp + -3752], r8
	mov r8, qword ptr [rbp + -2984]
	mov r9, r8
	mov qword ptr [rbp + -3760], r9
	mov r8, qword ptr [rbp + -3760]
	mov r9, qword ptr [rbp + -3752]
	xor r8, r9
	mov qword ptr [rbp + -3760], r8
	mov r8, qword ptr [rbp + -3760]
	mov r9, qword ptr [rbp + -3760]
	test r8, r9
	jnz _l103
	_l104:
	mov r8, 32
	mov qword ptr [rbp + -3768], r8
	mov r8, qword ptr [rbp + -3768]
	mov r9, r8
	mov qword ptr [rbp + -3776], r9
	mov r8, qword ptr [rbp + -3776]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -3784], r9
	mov r8, qword ptr [rbp + -3784]
	mov r9, r8
	mov qword ptr [rbp + -3792], r9
	mov r8, qword ptr [rbp + -3792]
	mov r9, r8
	mov qword ptr [rbp + -3800], r9
	mov r8, 3
	mov qword ptr [rbp + -3808], r8
	mov r8, qword ptr [rbp + -3800]
	mov r9, qword ptr [rbp + -3808]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -3800]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -3816], r8
	mov r8, qword ptr [rbp + -3816]
	mov r9, r8
	mov qword ptr [rbp + -3824], r9
	mov r8, qword ptr [rbp + -3824]
	mov r9, r8
	mov qword ptr [rbp + -3832], r9
	mov r8, 0
	mov qword ptr [rbp + -3840], r8
	mov r8, qword ptr [rbp + -3840]
	mov r9, r8
	mov qword ptr [rbp + -3848], r9
	_l107:
	mov r8, 3
	mov qword ptr [rbp + -3856], r8
	mov r8, qword ptr [rbp + -3848]
	mov r9, qword ptr [rbp + -3856]
	cmp r8, r9
	jge _l105
	_l106:
	mov r8, qword ptr [rbp + -3824]
	mov r9, qword ptr [rbp + -3848]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -3864], r8
	mov r8, qword ptr [rbp + -3864]
	mov r9, qword ptr [rbp + -3872]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -3848]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -3880], r8
	mov r8, qword ptr [rbp + -3880]
	mov r9, r8
	mov qword ptr [rbp + -3848], r9
	jmp _l107
	_l105:
	mov r8, qword ptr [rbp + -3832]
	mov r9, r8
	mov qword ptr [rbp + -3888], r9
	mov r8, 0
	mov qword ptr [rbp + -3896], r8
	mov r8, qword ptr [rbp + -3896]
	mov r9, r8
	mov qword ptr [rbp + -3904], r9
	mov r8, 8
	mov qword ptr [rbp + -3912], r8
	mov r8, qword ptr [rbp + -3888]
	mov r9, r8
	mov qword ptr [rbp + -3920], r9
	mov r8, qword ptr [rbp + -3920]
	mov r9, qword ptr [rbp + -3912]
	sub r8, r9
	mov qword ptr [rbp + -3920], r8
	mov r8, qword ptr [rbp + -3920]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3928], r8
	mov r8, qword ptr [rbp + -3904]
	mov r9, qword ptr [rbp + -3928]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -3936], r8
	mov r8, 1
	mov qword ptr [rbp + -3944], r8
	mov r8, qword ptr [rbp + -3936]
	mov r9, r8
	mov qword ptr [rbp + -3952], r9
	mov r8, qword ptr [rbp + -3952]
	mov r9, qword ptr [rbp + -3944]
	xor r8, r9
	mov qword ptr [rbp + -3952], r8
	mov r8, qword ptr [rbp + -3952]
	mov r9, qword ptr [rbp + -3952]
	test r8, r9
	jnz _l108
	_l109:
	mov r8, qword ptr [rbp + -3888]
	mov r9, qword ptr [rbp + -3904]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -3960], r8
	mov r8, 26
	mov qword ptr [rbp + -3968], r8
	mov r8, qword ptr [rbp + -2936]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -3968]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -2936], r8
	mov r8, qword ptr [rbp + -2936]
	lea r8, qword ptr [r8 + 65]
	mov qword ptr [rbp + -3976], r8
	mov r8, qword ptr [rbp + -3960]
	mov r9, qword ptr [rbp + -3976]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -3832]
	mov r9, r8
	mov qword ptr [rbp + -3984], r9
	mov r8, 1
	mov qword ptr [rbp + -3992], r8
	mov r8, qword ptr [rbp + -3992]
	mov r9, r8
	mov qword ptr [rbp + -4000], r9
	mov r8, 8
	mov qword ptr [rbp + -4008], r8
	mov r8, qword ptr [rbp + -3984]
	mov r9, r8
	mov qword ptr [rbp + -4016], r9
	mov r8, qword ptr [rbp + -4016]
	mov r9, qword ptr [rbp + -4008]
	sub r8, r9
	mov qword ptr [rbp + -4016], r8
	mov r8, qword ptr [rbp + -4016]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -4024], r8
	mov r8, qword ptr [rbp + -4000]
	mov r9, qword ptr [rbp + -4024]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -4032], r8
	mov r8, 1
	mov qword ptr [rbp + -4040], r8
	mov r8, qword ptr [rbp + -4032]
	mov r9, r8
	mov qword ptr [rbp + -4048], r9
	mov r8, qword ptr [rbp + -4048]
	mov r9, qword ptr [rbp + -4040]
	xor r8, r9
	mov qword ptr [rbp + -4048], r8
	mov r8, qword ptr [rbp + -4048]
	mov r9, qword ptr [rbp + -4048]
	test r8, r9
	jnz _l110
	_l111:
	mov r8, qword ptr [rbp + -3984]
	mov r9, qword ptr [rbp + -4000]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -4056], r8
	mov r8, 26
	mov qword ptr [rbp + -4064], r8
	mov r8, qword ptr [rbp + -2936]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -4064]
	idiv r8
	mov r8, rax
	mov qword ptr [rbp + -2936], r8
	mov r8, 26
	mov qword ptr [rbp + -4072], r8
	mov r8, qword ptr [rbp + -2936]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -4072]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -2936], r8
	mov r8, qword ptr [rbp + -2936]
	lea r8, qword ptr [r8 + 65]
	mov qword ptr [rbp + -4080], r8
	mov r8, qword ptr [rbp + -4056]
	mov r9, qword ptr [rbp + -4080]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -3832]
	mov r9, r8
	mov qword ptr [rbp + -4088], r9
	mov r8, 2
	mov qword ptr [rbp + -4096], r8
	mov r8, qword ptr [rbp + -4096]
	mov r9, r8
	mov qword ptr [rbp + -4104], r9
	mov r8, 8
	mov qword ptr [rbp + -4112], r8
	mov r8, qword ptr [rbp + -4088]
	mov r9, r8
	mov qword ptr [rbp + -4120], r9
	mov r8, qword ptr [rbp + -4120]
	mov r9, qword ptr [rbp + -4112]
	sub r8, r9
	mov qword ptr [rbp + -4120], r8
	mov r8, qword ptr [rbp + -4120]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -4128], r8
	mov r8, qword ptr [rbp + -4104]
	mov r9, qword ptr [rbp + -4128]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -4136], r8
	mov r8, 1
	mov qword ptr [rbp + -4144], r8
	mov r8, qword ptr [rbp + -4136]
	mov r9, r8
	mov qword ptr [rbp + -4152], r9
	mov r8, qword ptr [rbp + -4152]
	mov r9, qword ptr [rbp + -4144]
	xor r8, r9
	mov qword ptr [rbp + -4152], r8
	mov r8, qword ptr [rbp + -4152]
	mov r9, qword ptr [rbp + -4152]
	test r8, r9
	jnz _l112
	_l113:
	mov r8, qword ptr [rbp + -4088]
	mov r9, qword ptr [rbp + -4104]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -4160], r8
	mov r8, 676
	mov qword ptr [rbp + -4168], r8
	mov r8, qword ptr [rbp + -2936]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -4168]
	idiv r8
	mov r8, rax
	mov qword ptr [rbp + -2936], r8
	mov r8, 26
	mov qword ptr [rbp + -4176], r8
	mov r8, qword ptr [rbp + -2936]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -4176]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -2936], r8
	mov r8, qword ptr [rbp + -2936]
	lea r8, qword ptr [r8 + 65]
	mov qword ptr [rbp + -4184], r8
	mov r8, qword ptr [rbp + -4160]
	mov r9, qword ptr [rbp + -4184]
	mov qword ptr [r8], r9
	mov r8, 160
	mov qword ptr [rbp + -4192], r8
	mov r8, qword ptr [rbp + -4192]
	mov r9, r8
	mov qword ptr [rbp + -4200], r9
	mov r8, qword ptr [rbp + -4200]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -4208], r9
	mov r8, qword ptr [rbp + -4208]
	mov r9, r8
	mov qword ptr [rbp + -4216], r9
	mov r8, qword ptr [rbp + -4216]
	mov r9, r8
	mov qword ptr [rbp + -4224], r9
	mov r8, 19
	mov qword ptr [rbp + -4232], r8
	mov r8, qword ptr [rbp + -4224]
	mov r9, qword ptr [rbp + -4232]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -4240], r8
	mov r8, 77
	mov qword ptr [rbp + -4248], r8
	mov r8, qword ptr [rbp + -4240]
	mov r9, qword ptr [rbp + -4248]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -4256], r8
	mov r8, 65
	mov qword ptr [rbp + -4264], r8
	mov r8, qword ptr [rbp + -4256]
	mov r9, qword ptr [rbp + -4264]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -4272], r8
	mov r8, 84
	mov qword ptr [rbp + -4280], r8
	mov r8, qword ptr [rbp + -4272]
	mov r9, qword ptr [rbp + -4280]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -4288], r8
	mov r8, 67
	mov qword ptr [rbp + -4296], r8
	mov r8, qword ptr [rbp + -4288]
	mov r9, qword ptr [rbp + -4296]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -4304], r8
	mov r8, 72
	mov qword ptr [rbp + -4312], r8
	mov r8, qword ptr [rbp + -4304]
	mov r9, qword ptr [rbp + -4312]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -4320], r8
	mov r8, 32
	mov qword ptr [rbp + -4328], r8
	mov r8, qword ptr [rbp + -4320]
	mov r9, qword ptr [rbp + -4328]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -4336], r8
	mov r8, 65
	mov qword ptr [rbp + -4344], r8
	mov r8, qword ptr [rbp + -4336]
	mov r9, qword ptr [rbp + -4344]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -4352], r8
	mov r8, 116
	mov qword ptr [rbp + -4360], r8
	mov r8, qword ptr [rbp + -4352]
	mov r9, qword ptr [rbp + -4360]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 72]
	mov qword ptr [rbp + -4368], r8
	mov r8, 32
	mov qword ptr [rbp + -4376], r8
	mov r8, qword ptr [rbp + -4368]
	mov r9, qword ptr [rbp + -4376]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 80]
	mov qword ptr [rbp + -4384], r8
	mov r8, 114
	mov qword ptr [rbp + -4392], r8
	mov r8, qword ptr [rbp + -4384]
	mov r9, qword ptr [rbp + -4392]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 88]
	mov qword ptr [rbp + -4400], r8
	mov r8, 111
	mov qword ptr [rbp + -4408], r8
	mov r8, qword ptr [rbp + -4400]
	mov r9, qword ptr [rbp + -4408]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 96]
	mov qword ptr [rbp + -4416], r8
	mov r8, 116
	mov qword ptr [rbp + -4424], r8
	mov r8, qword ptr [rbp + -4416]
	mov r9, qword ptr [rbp + -4424]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 104]
	mov qword ptr [rbp + -4432], r8
	mov r8, 111
	mov qword ptr [rbp + -4440], r8
	mov r8, qword ptr [rbp + -4432]
	mov r9, qword ptr [rbp + -4440]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 112]
	mov qword ptr [rbp + -4448], r8
	mov r8, 114
	mov qword ptr [rbp + -4456], r8
	mov r8, qword ptr [rbp + -4448]
	mov r9, qword ptr [rbp + -4456]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 120]
	mov qword ptr [rbp + -4464], r8
	mov r8, 32
	mov qword ptr [rbp + -4472], r8
	mov r8, qword ptr [rbp + -4464]
	mov r9, qword ptr [rbp + -4472]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 128]
	mov qword ptr [rbp + -4480], r8
	mov r8, 112
	mov qword ptr [rbp + -4488], r8
	mov r8, qword ptr [rbp + -4480]
	mov r9, qword ptr [rbp + -4488]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 136]
	mov qword ptr [rbp + -4496], r8
	mov r8, 111
	mov qword ptr [rbp + -4504], r8
	mov r8, qword ptr [rbp + -4496]
	mov r9, qword ptr [rbp + -4504]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 144]
	mov qword ptr [rbp + -4512], r8
	mov r8, 115
	mov qword ptr [rbp + -4520], r8
	mov r8, qword ptr [rbp + -4512]
	mov r9, qword ptr [rbp + -4520]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 152]
	mov qword ptr [rbp + -4528], r8
	mov r8, 58
	mov qword ptr [rbp + -4536], r8
	mov r8, qword ptr [rbp + -4528]
	mov r9, qword ptr [rbp + -4536]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4224]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -4544], r8
	mov r8, qword ptr [rbp + -4544]
	mov r9, r8
	mov qword ptr [rbp + -4552], r9
	mov r8, qword ptr [rbp + -4552]
	mov rdi, r8
	and rsp, -16
	call _Iprint_pai
	mov r8, qword ptr [rbp + -3832]
	mov r9, r8
	mov qword ptr [rbp + -4560], r9
	mov r8, qword ptr [rbp + -4560]
	mov rdi, r8
	and rsp, -16
	call _Iprint_pai
	mov r8, 176
	mov qword ptr [rbp + -4568], r8
	mov r8, qword ptr [rbp + -4568]
	mov r9, r8
	mov qword ptr [rbp + -4576], r9
	mov r8, qword ptr [rbp + -4576]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -4584], r9
	mov r8, qword ptr [rbp + -4584]
	mov r9, r8
	mov qword ptr [rbp + -4592], r9
	mov r8, qword ptr [rbp + -4592]
	mov r9, r8
	mov qword ptr [rbp + -4600], r9
	mov r8, 21
	mov qword ptr [rbp + -4608], r8
	mov r8, qword ptr [rbp + -4600]
	mov r9, qword ptr [rbp + -4608]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -4616], r8
	mov r8, 32
	mov qword ptr [rbp + -4624], r8
	mov r8, qword ptr [rbp + -4616]
	mov r9, qword ptr [rbp + -4624]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 16]
	mov qword ptr [rbp + -4632], r8
	mov r8, 102
	mov qword ptr [rbp + -4640], r8
	mov r8, qword ptr [rbp + -4632]
	mov r9, qword ptr [rbp + -4640]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 24]
	mov qword ptr [rbp + -4648], r8
	mov r8, 105
	mov qword ptr [rbp + -4656], r8
	mov r8, qword ptr [rbp + -4648]
	mov r9, qword ptr [rbp + -4656]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 32]
	mov qword ptr [rbp + -4664], r8
	mov r8, 114
	mov qword ptr [rbp + -4672], r8
	mov r8, qword ptr [rbp + -4664]
	mov r9, qword ptr [rbp + -4672]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 40]
	mov qword ptr [rbp + -4680], r8
	mov r8, 115
	mov qword ptr [rbp + -4688], r8
	mov r8, qword ptr [rbp + -4680]
	mov r9, qword ptr [rbp + -4688]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 48]
	mov qword ptr [rbp + -4696], r8
	mov r8, 116
	mov qword ptr [rbp + -4704], r8
	mov r8, qword ptr [rbp + -4696]
	mov r9, qword ptr [rbp + -4704]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 56]
	mov qword ptr [rbp + -4712], r8
	mov r8, 32
	mov qword ptr [rbp + -4720], r8
	mov r8, qword ptr [rbp + -4712]
	mov r9, qword ptr [rbp + -4720]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 64]
	mov qword ptr [rbp + -4728], r8
	mov r8, 99
	mov qword ptr [rbp + -4736], r8
	mov r8, qword ptr [rbp + -4728]
	mov r9, qword ptr [rbp + -4736]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 72]
	mov qword ptr [rbp + -4744], r8
	mov r8, 111
	mov qword ptr [rbp + -4752], r8
	mov r8, qword ptr [rbp + -4744]
	mov r9, qword ptr [rbp + -4752]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 80]
	mov qword ptr [rbp + -4760], r8
	mov r8, 109
	mov qword ptr [rbp + -4768], r8
	mov r8, qword ptr [rbp + -4760]
	mov r9, qword ptr [rbp + -4768]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 88]
	mov qword ptr [rbp + -4776], r8
	mov r8, 101
	mov qword ptr [rbp + -4784], r8
	mov r8, qword ptr [rbp + -4776]
	mov r9, qword ptr [rbp + -4784]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 96]
	mov qword ptr [rbp + -4792], r8
	mov r8, 115
	mov qword ptr [rbp + -4800], r8
	mov r8, qword ptr [rbp + -4792]
	mov r9, qword ptr [rbp + -4800]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 104]
	mov qword ptr [rbp + -4808], r8
	mov r8, 32
	mov qword ptr [rbp + -4816], r8
	mov r8, qword ptr [rbp + -4808]
	mov r9, qword ptr [rbp + -4816]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 112]
	mov qword ptr [rbp + -4824], r8
	mov r8, 105
	mov qword ptr [rbp + -4832], r8
	mov r8, qword ptr [rbp + -4824]
	mov r9, qword ptr [rbp + -4832]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 120]
	mov qword ptr [rbp + -4840], r8
	mov r8, 110
	mov qword ptr [rbp + -4848], r8
	mov r8, qword ptr [rbp + -4840]
	mov r9, qword ptr [rbp + -4848]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 128]
	mov qword ptr [rbp + -4856], r8
	mov r8, 32
	mov qword ptr [rbp + -4864], r8
	mov r8, qword ptr [rbp + -4856]
	mov r9, qword ptr [rbp + -4864]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 136]
	mov qword ptr [rbp + -4872], r8
	mov r8, 102
	mov qword ptr [rbp + -4880], r8
	mov r8, qword ptr [rbp + -4872]
	mov r9, qword ptr [rbp + -4880]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 144]
	mov qword ptr [rbp + -4888], r8
	mov r8, 114
	mov qword ptr [rbp + -4896], r8
	mov r8, qword ptr [rbp + -4888]
	mov r9, qword ptr [rbp + -4896]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 152]
	mov qword ptr [rbp + -4904], r8
	mov r8, 111
	mov qword ptr [rbp + -4912], r8
	mov r8, qword ptr [rbp + -4904]
	mov r9, qword ptr [rbp + -4912]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 160]
	mov qword ptr [rbp + -4920], r8
	mov r8, 109
	mov qword ptr [rbp + -4928], r8
	mov r8, qword ptr [rbp + -4920]
	mov r9, qword ptr [rbp + -4928]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 168]
	mov qword ptr [rbp + -4936], r8
	mov r8, 58
	mov qword ptr [rbp + -4944], r8
	mov r8, qword ptr [rbp + -4936]
	mov r9, qword ptr [rbp + -4944]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -4600]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -4952], r8
	mov r8, qword ptr [rbp + -4952]
	mov r9, r8
	mov qword ptr [rbp + -4960], r9
	mov r8, qword ptr [rbp + -4960]
	mov rdi, r8
	and rsp, -16
	call _Iprint_pai
	mov r8, 16
	mov qword ptr [rbp + -4968], r8
	mov r8, qword ptr [rbp + -4968]
	mov r9, r8
	mov qword ptr [rbp + -4976], r9
	mov r8, qword ptr [rbp + -4976]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -4984], r9
	mov r8, qword ptr [rbp + -4984]
	mov r9, r8
	mov qword ptr [rbp + -4992], r9
	mov r8, qword ptr [rbp + -4992]
	mov r9, r8
	mov qword ptr [rbp + -5000], r9
	mov r8, 1
	mov qword ptr [rbp + -5008], r8
	mov r8, qword ptr [rbp + -5000]
	mov r9, qword ptr [rbp + -5008]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -5000]
	lea r8, qword ptr [r8 + 8]
	mov qword ptr [rbp + -5016], r8
	mov r8, qword ptr [rbp + -5016]
	mov r9, r8
	mov qword ptr [rbp + -5024], r9
	mov r8, qword ptr [rbp + -5024]
	mov r9, r8
	mov qword ptr [rbp + -5032], r9
	mov r8, 0
	mov qword ptr [rbp + -5040], r8
	mov r8, qword ptr [rbp + -5040]
	mov r9, r8
	mov qword ptr [rbp + -5048], r9
	_l116:
	mov r8, 1
	mov qword ptr [rbp + -5056], r8
	mov r8, qword ptr [rbp + -5048]
	mov r9, qword ptr [rbp + -5056]
	cmp r8, r9
	jge _l114
	_l115:
	mov r8, qword ptr [rbp + -5024]
	mov r9, qword ptr [rbp + -5048]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -5064], r8
	mov r8, qword ptr [rbp + -5064]
	mov r9, qword ptr [rbp + -5072]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -5048]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -5080], r8
	mov r8, qword ptr [rbp + -5080]
	mov r9, r8
	mov qword ptr [rbp + -5048], r9
	jmp _l116
	_l114:
	mov r8, qword ptr [rbp + -5032]
	mov r9, r8
	mov qword ptr [rbp + -5088], r9
	mov r8, 0
	mov qword ptr [rbp + -5096], r8
	mov r8, qword ptr [rbp + -5096]
	mov r9, r8
	mov qword ptr [rbp + -5104], r9
	mov r8, 8
	mov qword ptr [rbp + -5112], r8
	mov r8, qword ptr [rbp + -5088]
	mov r9, r8
	mov qword ptr [rbp + -5120], r9
	mov r8, qword ptr [rbp + -5120]
	mov r9, qword ptr [rbp + -5112]
	sub r8, r9
	mov qword ptr [rbp + -5120], r8
	mov r8, qword ptr [rbp + -5120]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -5128], r8
	mov r8, qword ptr [rbp + -5104]
	mov r9, qword ptr [rbp + -5128]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -5136], r8
	mov r8, 1
	mov qword ptr [rbp + -5144], r8
	mov r8, qword ptr [rbp + -5136]
	mov r9, r8
	mov qword ptr [rbp + -5152], r9
	mov r8, qword ptr [rbp + -5152]
	mov r9, qword ptr [rbp + -5144]
	xor r8, r9
	mov qword ptr [rbp + -5152], r8
	mov r8, qword ptr [rbp + -5152]
	mov r9, qword ptr [rbp + -5152]
	test r8, r9
	jnz _l117
	_l118:
	mov r8, qword ptr [rbp + -5088]
	mov r9, qword ptr [rbp + -5104]
	lea r8, qword ptr [r8 + r9 * 8]
	mov qword ptr [rbp + -5160], r8
	mov r8, qword ptr [rbp + -2960]
	lea r8, qword ptr [r8 + 65]
	mov qword ptr [rbp + -5168], r8
	mov r8, qword ptr [rbp + -5160]
	mov r9, qword ptr [rbp + -5168]
	mov qword ptr [r8], r9
	mov r8, qword ptr [rbp + -5032]
	mov r9, r8
	mov qword ptr [rbp + -5176], r9
	mov r8, qword ptr [rbp + -5176]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	_l103:
	mov r8, qword ptr [rbp + -2960]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -5184], r8
	mov r8, qword ptr [rbp + -5184]
	mov r9, r8
	mov qword ptr [rbp + -2960], r9
	jmp _l86
	_l117:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l118
	_l112:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l113
	_l110:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l111
	_l108:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l109
	_l84:
	mov r8, qword ptr [rbp + -2936]
	lea r8, qword ptr [r8 + 1]
	mov qword ptr [rbp + -5192], r8
	mov r8, qword ptr [rbp + -5192]
	mov r9, r8
	mov qword ptr [rbp + -2936], r9
	jmp _l83
	_l81:
	leave
	ret
