.intel_syntax noprefix
.data
g3: .quad 26, 89, 82, 85, 72, 81, 83, 76, 68, 80, 88, 78, 71, 79, 75, 77, 73, 69, 66, 70, 90, 67, 87, 86, 74, 65, 84
g4: .quad 19, 77, 65, 84, 67, 72, 32, 65, 116, 32, 114, 111, 116, 111, 114, 32, 112, 111, 115, 58
g0: .quad 26, 69, 75, 77, 70, 76, 71, 68, 81, 86, 90, 78, 84, 79, 87, 89, 72, 88, 85, 83, 80, 65, 73, 66, 82, 67, 74
g2: .quad 26, 66, 68, 70, 72, 74, 76, 67, 80, 82, 84, 88, 86, 90, 78, 89, 69, 73, 87, 71, 65, 75, 77, 85, 83, 81, 79
g1: .quad 26, 65, 74, 68, 75, 83, 73, 82, 85, 88, 66, 76, 72, 87, 84, 77, 67, 81, 71, 90, 78, 80, 89, 70, 86, 79, 69
g5: .quad 21, 32, 102, 105, 114, 115, 116, 32, 99, 111, 109, 101, 115, 32, 105, 110, 32, 102, 114, 111, 109, 58
.globl _ItoLower_ii, _ImakeInverse_paiai, _ImkMatrix_aaii, _ImakeRotor_t3aaiaaiiai, _IrotorEncryptForward_iaaiaaiii, _IrotorEncryptBack_iaaiaaiii, _ImakeReflector_aiai, _IreflectorEncrypt_iaii, _Imain_paai
.text
_ItoLower_ii:
	enter 88, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r8, 65
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -24]
	mov r9, qword ptr [rbp + -32]
	cmp r8, r9
	jl l0
	l2:
	mov r8, 90
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -24]
	mov r9, qword ptr [rbp + -40]
	cmp r8, r9
	jg l0
	l1:
	mov r8, 65
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r8, qword ptr [rbp + -56]
	mov r9, qword ptr [rbp + -48]
	sub r8, r9
	mov qword ptr [rbp + -56], r8
	mov r8, 97
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -56]
	mov r10, qword ptr [rbp + -64]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov rax, r8
	leave
	ret
	l0:
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r8, qword ptr [rbp + -88]
	mov rax, r8
	leave
	ret
_ImakeInverse_paiai:
	enter 248, 0
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
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	l5:
	mov r8, 26
	mov qword ptr [rbp + -72], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, qword ptr [rbp + -72]
	cmp r8, r9
	jge l3
	l4:
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r8, 8
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -104]
	sub r8, r9
	mov qword ptr [rbp + -112], r8
	mov r9, qword ptr [rbp + -112]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -96]
	mov r9, qword ptr [rbp + -112]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -120], r8
	mov r8, 1
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -120]
	mov r8, r9
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -128]
	xor r8, r9
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -136]
	mov r9, qword ptr [rbp + -136]
	test r8, r9
	jnz l8
	l9:
	mov r8, 8
	mov qword ptr [rbp + -144], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -144]
	imul r8, r9
	mov qword ptr [rbp + -152], r8
	mov r9, qword ptr [rbp + -88]
	mov r10, qword ptr [rbp + -152]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -160], r8
	mov r9, qword ptr [rbp + -160]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -160], r8
	mov r9, qword ptr [rbp + -160]
	mov r8, r9
	mov qword ptr [rbp + -168], r8
	mov r8, 8
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -184], r8
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -176]
	sub r8, r9
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -184], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -184]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -192], r8
	mov r8, 1
	mov qword ptr [rbp + -200], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -200]
	xor r8, r9
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -208]
	test r8, r9
	jnz l6
	l7:
	mov r8, 8
	mov qword ptr [rbp + -216], r8
	mov r9, qword ptr [rbp + -168]
	mov r8, r9
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -224]
	mov r9, qword ptr [rbp + -216]
	imul r8, r9
	mov qword ptr [rbp + -224], r8
	mov r9, qword ptr [rbp + -80]
	mov r10, qword ptr [rbp + -224]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -64]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -240], r8
	mov r9, qword ptr [rbp + -64]
	mov r10, qword ptr [rbp + -240]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -248], r8
	mov r9, qword ptr [rbp + -248]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	jmp l5
	l6:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l7
	l8:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l9
	l3:
	leave
	ret
_ImkMatrix_aaii:
	enter 368, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r8, 8
	mov qword ptr [rbp + -48], r8
	mov r8, 8
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, qword ptr [rbp + -56]
	imul r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -48]
	mov r10, qword ptr [rbp + -64]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -104]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -32]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -112]
	mov r10, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, r9
	mov qword ptr [rbp + -136], r8
	mov r9, qword ptr [rbp + -136]
	mov r8, r9
	mov qword ptr [rbp + -144], r8
	mov r8, 0
	mov qword ptr [rbp + -152], r8
	mov r9, qword ptr [rbp + -152]
	mov r8, r9
	mov qword ptr [rbp + -160], r8
	l15:
	mov r8, qword ptr [rbp + -160]
	mov r9, qword ptr [rbp + -32]
	cmp r8, r9
	jge l13
	l14:
	mov r8, 8
	mov qword ptr [rbp + -168], r8
	mov r8, 8
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -184], r8
	mov r8, qword ptr [rbp + -184]
	mov r9, qword ptr [rbp + -176]
	imul r8, r9
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -168]
	mov r10, qword ptr [rbp + -184]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -192], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -200]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -208], r8
	mov r9, qword ptr [rbp + -208]
	mov r8, r9
	mov qword ptr [rbp + -216], r8
	mov r9, qword ptr [rbp + -216]
	mov r8, r9
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -224]
	mov r9, qword ptr [rbp + -40]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -232], r8
	mov r9, qword ptr [rbp + -224]
	mov r10, qword ptr [rbp + -232]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -240], r8
	mov r9, qword ptr [rbp + -240]
	mov r8, r9
	mov qword ptr [rbp + -248], r8
	mov r9, qword ptr [rbp + -248]
	mov r8, r9
	mov qword ptr [rbp + -256], r8
	mov r8, 0
	mov qword ptr [rbp + -264], r8
	mov r9, qword ptr [rbp + -264]
	mov r8, r9
	mov qword ptr [rbp + -272], r8
	l12:
	mov r8, qword ptr [rbp + -272]
	mov r9, qword ptr [rbp + -40]
	cmp r8, r9
	jge l10
	l11:
	mov r8, 8
	mov qword ptr [rbp + -280], r8
	mov r9, qword ptr [rbp + -272]
	mov r8, r9
	mov qword ptr [rbp + -288], r8
	mov r8, qword ptr [rbp + -288]
	mov r9, qword ptr [rbp + -280]
	imul r8, r9
	mov qword ptr [rbp + -288], r8
	mov r9, qword ptr [rbp + -248]
	mov r10, qword ptr [rbp + -288]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -296], r8
	mov r8, qword ptr [rbp + -296]
	mov r9, qword ptr [rbp + -304]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -312], r8
	mov r9, qword ptr [rbp + -272]
	mov r10, qword ptr [rbp + -312]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -320], r8
	mov r9, qword ptr [rbp + -320]
	mov r8, r9
	mov qword ptr [rbp + -272], r8
	jmp l12
	l10:
	mov r8, 8
	mov qword ptr [rbp + -328], r8
	mov r9, qword ptr [rbp + -160]
	mov r8, r9
	mov qword ptr [rbp + -336], r8
	mov r8, qword ptr [rbp + -336]
	mov r9, qword ptr [rbp + -328]
	imul r8, r9
	mov qword ptr [rbp + -336], r8
	mov r9, qword ptr [rbp + -136]
	mov r10, qword ptr [rbp + -336]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -344], r8
	mov r8, qword ptr [rbp + -344]
	mov r9, qword ptr [rbp + -256]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -352], r8
	mov r9, qword ptr [rbp + -160]
	mov r10, qword ptr [rbp + -352]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -360], r8
	mov r9, qword ptr [rbp + -360]
	mov r8, r9
	mov qword ptr [rbp + -160], r8
	jmp l15
	l13:
	mov r9, qword ptr [rbp + -144]
	mov r8, r9
	mov qword ptr [rbp + -368], r8
	mov r8, qword ptr [rbp + -368]
	mov rax, r8
	leave
	ret
_ImakeRotor_t3aaiaaiiai:
	enter 1800, 0
	mov r8, rsi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r8, 26
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -40]
	mov rdi, r8
	and rsp, -16
	call _ImkMatrix_aaii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, 26
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r8, qword ptr [rbp + -88]
	mov rdi, r8
	and rsp, -16
	call _ImkMatrix_aaii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -104]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, 216
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -120]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -136], r8
	mov r9, qword ptr [rbp + -136]
	mov r8, r9
	mov qword ptr [rbp + -144], r8
	mov r9, qword ptr [rbp + -144]
	mov r8, r9
	mov qword ptr [rbp + -152], r8
	mov r8, 26
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -160]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -168], r8
	mov r9, qword ptr [rbp + -152]
	mov r10, qword ptr [rbp + -168]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -176]
	mov r8, r9
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -192], r8
	mov r8, 0
	mov qword ptr [rbp + -200], r8
	mov r9, qword ptr [rbp + -200]
	mov r8, r9
	mov qword ptr [rbp + -208], r8
	l18:
	mov r8, 26
	mov qword ptr [rbp + -216], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -216]
	cmp r8, r9
	jge l16
	l17:
	mov r8, 8
	mov qword ptr [rbp + -224], r8
	mov r9, qword ptr [rbp + -208]
	mov r8, r9
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -224]
	imul r8, r9
	mov qword ptr [rbp + -232], r8
	mov r9, qword ptr [rbp + -184]
	mov r10, qword ptr [rbp + -232]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -240]
	mov r9, qword ptr [rbp + -248]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -256], r8
	mov r9, qword ptr [rbp + -208]
	mov r10, qword ptr [rbp + -256]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -264], r8
	mov r9, qword ptr [rbp + -264]
	mov r8, r9
	mov qword ptr [rbp + -208], r8
	jmp l18
	l16:
	mov r8, 0
	mov qword ptr [rbp + -272], r8
	mov r9, qword ptr [rbp + -272]
	mov r8, r9
	mov qword ptr [rbp + -280], r8
	l21:
	mov r8, 26
	mov qword ptr [rbp + -288], r8
	mov r8, qword ptr [rbp + -280]
	mov r9, qword ptr [rbp + -288]
	cmp r8, r9
	jge l19
	l20:
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -296], r8
	mov r9, qword ptr [rbp + -280]
	mov r8, r9
	mov qword ptr [rbp + -304], r8
	mov r8, 8
	mov qword ptr [rbp + -312], r8
	mov r9, qword ptr [rbp + -296]
	mov r8, r9
	mov qword ptr [rbp + -320], r8
	mov r8, qword ptr [rbp + -320]
	mov r9, qword ptr [rbp + -312]
	sub r8, r9
	mov qword ptr [rbp + -320], r8
	mov r9, qword ptr [rbp + -320]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -320], r8
	mov r8, qword ptr [rbp + -304]
	mov r9, qword ptr [rbp + -320]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -328], r8
	mov r8, 1
	mov qword ptr [rbp + -336], r8
	mov r9, qword ptr [rbp + -328]
	mov r8, r9
	mov qword ptr [rbp + -344], r8
	mov r8, qword ptr [rbp + -344]
	mov r9, qword ptr [rbp + -336]
	xor r8, r9
	mov qword ptr [rbp + -344], r8
	mov r8, qword ptr [rbp + -344]
	mov r9, qword ptr [rbp + -344]
	test r8, r9
	jnz l22
	l23:
	mov r8, 8
	mov qword ptr [rbp + -352], r8
	mov r9, qword ptr [rbp + -304]
	mov r8, r9
	mov qword ptr [rbp + -360], r8
	mov r8, qword ptr [rbp + -360]
	mov r9, qword ptr [rbp + -352]
	imul r8, r9
	mov qword ptr [rbp + -360], r8
	mov r9, qword ptr [rbp + -296]
	mov r10, qword ptr [rbp + -360]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -368], r8
	mov r9, qword ptr [rbp + -368]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -368], r8
	mov r9, qword ptr [rbp + -368]
	mov r8, r9
	mov qword ptr [rbp + -376], r8
	mov r8, qword ptr [rbp + -376]
	mov rdi, r8
	and rsp, -16
	call _ItoLower_ii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -384], r8
	mov r9, qword ptr [rbp + -384]
	mov r8, r9
	mov qword ptr [rbp + -392], r8
	mov r8, 97
	mov qword ptr [rbp + -400], r8
	mov r9, qword ptr [rbp + -392]
	mov r8, r9
	mov qword ptr [rbp + -408], r8
	mov r8, qword ptr [rbp + -408]
	mov r9, qword ptr [rbp + -400]
	sub r8, r9
	mov qword ptr [rbp + -408], r8
	mov r9, qword ptr [rbp + -408]
	mov r8, r9
	mov qword ptr [rbp + -416], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -424], r8
	mov r9, qword ptr [rbp + -280]
	mov r8, r9
	mov qword ptr [rbp + -432], r8
	mov r8, 8
	mov qword ptr [rbp + -440], r8
	mov r9, qword ptr [rbp + -424]
	mov r8, r9
	mov qword ptr [rbp + -448], r8
	mov r8, qword ptr [rbp + -448]
	mov r9, qword ptr [rbp + -440]
	sub r8, r9
	mov qword ptr [rbp + -448], r8
	mov r9, qword ptr [rbp + -448]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -448], r8
	mov r8, qword ptr [rbp + -432]
	mov r9, qword ptr [rbp + -448]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -456], r8
	mov r8, 1
	mov qword ptr [rbp + -464], r8
	mov r9, qword ptr [rbp + -456]
	mov r8, r9
	mov qword ptr [rbp + -472], r8
	mov r8, qword ptr [rbp + -472]
	mov r9, qword ptr [rbp + -464]
	xor r8, r9
	mov qword ptr [rbp + -472], r8
	mov r8, qword ptr [rbp + -472]
	mov r9, qword ptr [rbp + -472]
	test r8, r9
	jnz l24
	l25:
	mov r8, 8
	mov qword ptr [rbp + -480], r8
	mov r9, qword ptr [rbp + -432]
	mov r8, r9
	mov qword ptr [rbp + -488], r8
	mov r8, qword ptr [rbp + -488]
	mov r9, qword ptr [rbp + -480]
	imul r8, r9
	mov qword ptr [rbp + -488], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -488]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -496], r8
	mov r8, qword ptr [rbp + -496]
	mov r9, qword ptr [rbp + -416]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -504], r8
	mov r9, qword ptr [rbp + -280]
	mov r10, qword ptr [rbp + -504]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -512], r8
	mov r9, qword ptr [rbp + -512]
	mov r8, r9
	mov qword ptr [rbp + -280], r8
	jmp l21
	l24:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l25
	l22:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l23
	l19:
	mov r8, 216
	mov qword ptr [rbp + -520], r8
	mov r9, qword ptr [rbp + -520]
	mov r8, r9
	mov qword ptr [rbp + -528], r8
	mov r8, qword ptr [rbp + -528]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -536], r8
	mov r9, qword ptr [rbp + -536]
	mov r8, r9
	mov qword ptr [rbp + -544], r8
	mov r9, qword ptr [rbp + -544]
	mov r8, r9
	mov qword ptr [rbp + -552], r8
	mov r8, 26
	mov qword ptr [rbp + -560], r8
	mov r8, qword ptr [rbp + -552]
	mov r9, qword ptr [rbp + -560]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -568], r8
	mov r9, qword ptr [rbp + -552]
	mov r10, qword ptr [rbp + -568]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -576], r8
	mov r9, qword ptr [rbp + -576]
	mov r8, r9
	mov qword ptr [rbp + -584], r8
	mov r9, qword ptr [rbp + -584]
	mov r8, r9
	mov qword ptr [rbp + -592], r8
	mov r8, 0
	mov qword ptr [rbp + -600], r8
	mov r9, qword ptr [rbp + -600]
	mov r8, r9
	mov qword ptr [rbp + -608], r8
	l28:
	mov r8, 26
	mov qword ptr [rbp + -616], r8
	mov r8, qword ptr [rbp + -608]
	mov r9, qword ptr [rbp + -616]
	cmp r8, r9
	jge l26
	l27:
	mov r8, 8
	mov qword ptr [rbp + -624], r8
	mov r9, qword ptr [rbp + -608]
	mov r8, r9
	mov qword ptr [rbp + -632], r8
	mov r8, qword ptr [rbp + -632]
	mov r9, qword ptr [rbp + -624]
	imul r8, r9
	mov qword ptr [rbp + -632], r8
	mov r9, qword ptr [rbp + -584]
	mov r10, qword ptr [rbp + -632]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -640], r8
	mov r8, qword ptr [rbp + -640]
	mov r9, qword ptr [rbp + -648]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -656], r8
	mov r9, qword ptr [rbp + -608]
	mov r10, qword ptr [rbp + -656]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -664], r8
	mov r9, qword ptr [rbp + -664]
	mov r8, r9
	mov qword ptr [rbp + -608], r8
	jmp l28
	l26:
	mov r8, 0
	mov qword ptr [rbp + -672], r8
	mov r9, qword ptr [rbp + -672]
	mov r8, r9
	mov qword ptr [rbp + -680], r8
	l31:
	mov r8, 26
	mov qword ptr [rbp + -688], r8
	mov r8, qword ptr [rbp + -680]
	mov r9, qword ptr [rbp + -688]
	cmp r8, r9
	jge l29
	l30:
	mov r8, 0
	mov qword ptr [rbp + -696], r8
	mov r9, qword ptr [rbp + -696]
	mov r8, r9
	mov qword ptr [rbp + -280], r8
	l34:
	mov r8, 26
	mov qword ptr [rbp + -704], r8
	mov r8, qword ptr [rbp + -280]
	mov r9, qword ptr [rbp + -704]
	cmp r8, r9
	jge l32
	l33:
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -712], r8
	mov r9, qword ptr [rbp + -680]
	mov r8, r9
	mov qword ptr [rbp + -720], r8
	mov r8, 8
	mov qword ptr [rbp + -728], r8
	mov r9, qword ptr [rbp + -712]
	mov r8, r9
	mov qword ptr [rbp + -736], r8
	mov r8, qword ptr [rbp + -736]
	mov r9, qword ptr [rbp + -728]
	sub r8, r9
	mov qword ptr [rbp + -736], r8
	mov r9, qword ptr [rbp + -736]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -736], r8
	mov r8, qword ptr [rbp + -720]
	mov r9, qword ptr [rbp + -736]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -744], r8
	mov r8, 1
	mov qword ptr [rbp + -752], r8
	mov r9, qword ptr [rbp + -744]
	mov r8, r9
	mov qword ptr [rbp + -760], r8
	mov r8, qword ptr [rbp + -760]
	mov r9, qword ptr [rbp + -752]
	xor r8, r9
	mov qword ptr [rbp + -760], r8
	mov r8, qword ptr [rbp + -760]
	mov r9, qword ptr [rbp + -760]
	test r8, r9
	jnz l39
	l40:
	mov r8, 8
	mov qword ptr [rbp + -768], r8
	mov r9, qword ptr [rbp + -720]
	mov r8, r9
	mov qword ptr [rbp + -776], r8
	mov r8, qword ptr [rbp + -776]
	mov r9, qword ptr [rbp + -768]
	imul r8, r9
	mov qword ptr [rbp + -776], r8
	mov r9, qword ptr [rbp + -712]
	mov r10, qword ptr [rbp + -776]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -784], r8
	mov r9, qword ptr [rbp + -784]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -784], r8
	mov r9, qword ptr [rbp + -784]
	mov r8, r9
	mov qword ptr [rbp + -792], r8
	mov r9, qword ptr [rbp + -280]
	mov r8, r9
	mov qword ptr [rbp + -800], r8
	mov r8, 8
	mov qword ptr [rbp + -808], r8
	mov r9, qword ptr [rbp + -792]
	mov r8, r9
	mov qword ptr [rbp + -816], r8
	mov r8, qword ptr [rbp + -816]
	mov r9, qword ptr [rbp + -808]
	sub r8, r9
	mov qword ptr [rbp + -816], r8
	mov r9, qword ptr [rbp + -816]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -816], r8
	mov r8, qword ptr [rbp + -800]
	mov r9, qword ptr [rbp + -816]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -824], r8
	mov r8, 1
	mov qword ptr [rbp + -832], r8
	mov r9, qword ptr [rbp + -824]
	mov r8, r9
	mov qword ptr [rbp + -840], r8
	mov r8, qword ptr [rbp + -840]
	mov r9, qword ptr [rbp + -832]
	xor r8, r9
	mov qword ptr [rbp + -840], r8
	mov r8, qword ptr [rbp + -840]
	mov r9, qword ptr [rbp + -840]
	test r8, r9
	jnz l37
	l38:
	mov r8, 8
	mov qword ptr [rbp + -848], r8
	mov r9, qword ptr [rbp + -800]
	mov r8, r9
	mov qword ptr [rbp + -856], r8
	mov r8, qword ptr [rbp + -856]
	mov r9, qword ptr [rbp + -848]
	imul r8, r9
	mov qword ptr [rbp + -856], r8
	mov r9, qword ptr [rbp + -792]
	mov r10, qword ptr [rbp + -856]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -864], r8
	mov r9, qword ptr [rbp + -864]
	mov r8, r9
	mov qword ptr [rbp + -872], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -880], r8
	mov r9, qword ptr [rbp + -280]
	mov r8, r9
	mov qword ptr [rbp + -888], r8
	mov r8, 8
	mov qword ptr [rbp + -896], r8
	mov r9, qword ptr [rbp + -880]
	mov r8, r9
	mov qword ptr [rbp + -904], r8
	mov r8, qword ptr [rbp + -904]
	mov r9, qword ptr [rbp + -896]
	sub r8, r9
	mov qword ptr [rbp + -904], r8
	mov r9, qword ptr [rbp + -904]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -904], r8
	mov r8, qword ptr [rbp + -888]
	mov r9, qword ptr [rbp + -904]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -912], r8
	mov r8, 1
	mov qword ptr [rbp + -920], r8
	mov r9, qword ptr [rbp + -912]
	mov r8, r9
	mov qword ptr [rbp + -928], r8
	mov r8, qword ptr [rbp + -928]
	mov r9, qword ptr [rbp + -920]
	xor r8, r9
	mov qword ptr [rbp + -928], r8
	mov r8, qword ptr [rbp + -928]
	mov r9, qword ptr [rbp + -928]
	test r8, r9
	jnz l35
	l36:
	mov r8, 8
	mov qword ptr [rbp + -936], r8
	mov r9, qword ptr [rbp + -888]
	mov r8, r9
	mov qword ptr [rbp + -944], r8
	mov r8, qword ptr [rbp + -944]
	mov r9, qword ptr [rbp + -936]
	imul r8, r9
	mov qword ptr [rbp + -944], r8
	mov r9, qword ptr [rbp + -880]
	mov r10, qword ptr [rbp + -944]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -952], r8
	mov r9, qword ptr [rbp + -952]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -952], r8
	mov r8, qword ptr [rbp + -872]
	mov r9, qword ptr [rbp + -952]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -960], r8
	mov r9, qword ptr [rbp + -280]
	mov r10, qword ptr [rbp + -960]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -968], r8
	mov r9, qword ptr [rbp + -968]
	mov r8, r9
	mov qword ptr [rbp + -280], r8
	jmp l34
	l35:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l36
	l37:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l38
	l39:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l40
	l32:
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -976], r8
	mov r9, qword ptr [rbp + -592]
	mov r8, r9
	mov qword ptr [rbp + -984], r8
	mov r8, qword ptr [rbp + -984]
	mov rdi, r8
	mov r8, qword ptr [rbp + -976]
	mov rsi, r8
	and rsp, -16
	call _ImakeInverse_paiai
	mov r8, 0
	mov qword ptr [rbp + -992], r8
	mov r9, qword ptr [rbp + -992]
	mov r8, r9
	mov qword ptr [rbp + -280], r8
	l43:
	mov r8, 26
	mov qword ptr [rbp + -1000], r8
	mov r8, qword ptr [rbp + -280]
	mov r9, qword ptr [rbp + -1000]
	cmp r8, r9
	jge l41
	l42:
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -1008], r8
	mov r9, qword ptr [rbp + -680]
	mov r8, r9
	mov qword ptr [rbp + -1016], r8
	mov r8, 8
	mov qword ptr [rbp + -1024], r8
	mov r9, qword ptr [rbp + -1008]
	mov r8, r9
	mov qword ptr [rbp + -1032], r8
	mov r8, qword ptr [rbp + -1032]
	mov r9, qword ptr [rbp + -1024]
	sub r8, r9
	mov qword ptr [rbp + -1032], r8
	mov r9, qword ptr [rbp + -1032]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1032], r8
	mov r8, qword ptr [rbp + -1016]
	mov r9, qword ptr [rbp + -1032]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1040], r8
	mov r8, 1
	mov qword ptr [rbp + -1048], r8
	mov r9, qword ptr [rbp + -1040]
	mov r8, r9
	mov qword ptr [rbp + -1056], r8
	mov r8, qword ptr [rbp + -1056]
	mov r9, qword ptr [rbp + -1048]
	xor r8, r9
	mov qword ptr [rbp + -1056], r8
	mov r8, qword ptr [rbp + -1056]
	mov r9, qword ptr [rbp + -1056]
	test r8, r9
	jnz l48
	l49:
	mov r8, 8
	mov qword ptr [rbp + -1064], r8
	mov r9, qword ptr [rbp + -1016]
	mov r8, r9
	mov qword ptr [rbp + -1072], r8
	mov r8, qword ptr [rbp + -1072]
	mov r9, qword ptr [rbp + -1064]
	imul r8, r9
	mov qword ptr [rbp + -1072], r8
	mov r9, qword ptr [rbp + -1008]
	mov r10, qword ptr [rbp + -1072]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1080], r8
	mov r9, qword ptr [rbp + -1080]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1080], r8
	mov r9, qword ptr [rbp + -1080]
	mov r8, r9
	mov qword ptr [rbp + -1088], r8
	mov r9, qword ptr [rbp + -280]
	mov r8, r9
	mov qword ptr [rbp + -1096], r8
	mov r8, 8
	mov qword ptr [rbp + -1104], r8
	mov r9, qword ptr [rbp + -1088]
	mov r8, r9
	mov qword ptr [rbp + -1112], r8
	mov r8, qword ptr [rbp + -1112]
	mov r9, qword ptr [rbp + -1104]
	sub r8, r9
	mov qword ptr [rbp + -1112], r8
	mov r9, qword ptr [rbp + -1112]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1112], r8
	mov r8, qword ptr [rbp + -1096]
	mov r9, qword ptr [rbp + -1112]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1120], r8
	mov r8, 1
	mov qword ptr [rbp + -1128], r8
	mov r9, qword ptr [rbp + -1120]
	mov r8, r9
	mov qword ptr [rbp + -1136], r8
	mov r8, qword ptr [rbp + -1136]
	mov r9, qword ptr [rbp + -1128]
	xor r8, r9
	mov qword ptr [rbp + -1136], r8
	mov r8, qword ptr [rbp + -1136]
	mov r9, qword ptr [rbp + -1136]
	test r8, r9
	jnz l46
	l47:
	mov r8, 8
	mov qword ptr [rbp + -1144], r8
	mov r9, qword ptr [rbp + -1096]
	mov r8, r9
	mov qword ptr [rbp + -1152], r8
	mov r8, qword ptr [rbp + -1152]
	mov r9, qword ptr [rbp + -1144]
	imul r8, r9
	mov qword ptr [rbp + -1152], r8
	mov r9, qword ptr [rbp + -1088]
	mov r10, qword ptr [rbp + -1152]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1160], r8
	mov r9, qword ptr [rbp + -1160]
	mov r8, r9
	mov qword ptr [rbp + -1168], r8
	mov r9, qword ptr [rbp + -592]
	mov r8, r9
	mov qword ptr [rbp + -1176], r8
	mov r9, qword ptr [rbp + -280]
	mov r8, r9
	mov qword ptr [rbp + -1184], r8
	mov r8, 8
	mov qword ptr [rbp + -1192], r8
	mov r9, qword ptr [rbp + -1176]
	mov r8, r9
	mov qword ptr [rbp + -1200], r8
	mov r8, qword ptr [rbp + -1200]
	mov r9, qword ptr [rbp + -1192]
	sub r8, r9
	mov qword ptr [rbp + -1200], r8
	mov r9, qword ptr [rbp + -1200]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1200], r8
	mov r8, qword ptr [rbp + -1184]
	mov r9, qword ptr [rbp + -1200]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1208], r8
	mov r8, 1
	mov qword ptr [rbp + -1216], r8
	mov r9, qword ptr [rbp + -1208]
	mov r8, r9
	mov qword ptr [rbp + -1224], r8
	mov r8, qword ptr [rbp + -1224]
	mov r9, qword ptr [rbp + -1216]
	xor r8, r9
	mov qword ptr [rbp + -1224], r8
	mov r8, qword ptr [rbp + -1224]
	mov r9, qword ptr [rbp + -1224]
	test r8, r9
	jnz l44
	l45:
	mov r8, 8
	mov qword ptr [rbp + -1232], r8
	mov r9, qword ptr [rbp + -1184]
	mov r8, r9
	mov qword ptr [rbp + -1240], r8
	mov r8, qword ptr [rbp + -1240]
	mov r9, qword ptr [rbp + -1232]
	imul r8, r9
	mov qword ptr [rbp + -1240], r8
	mov r9, qword ptr [rbp + -1176]
	mov r10, qword ptr [rbp + -1240]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1248], r8
	mov r9, qword ptr [rbp + -1248]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1248], r8
	mov r8, qword ptr [rbp + -1168]
	mov r9, qword ptr [rbp + -1248]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1256], r8
	mov r9, qword ptr [rbp + -280]
	mov r10, qword ptr [rbp + -1256]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1264], r8
	mov r9, qword ptr [rbp + -1264]
	mov r8, r9
	mov qword ptr [rbp + -280], r8
	jmp l43
	l44:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l45
	l46:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l47
	l48:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l49
	l41:
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -1272], r8
	mov r8, 0
	mov qword ptr [rbp + -1280], r8
	mov r9, qword ptr [rbp + -1280]
	mov r8, r9
	mov qword ptr [rbp + -1288], r8
	mov r8, 8
	mov qword ptr [rbp + -1296], r8
	mov r9, qword ptr [rbp + -1272]
	mov r8, r9
	mov qword ptr [rbp + -1304], r8
	mov r8, qword ptr [rbp + -1304]
	mov r9, qword ptr [rbp + -1296]
	sub r8, r9
	mov qword ptr [rbp + -1304], r8
	mov r9, qword ptr [rbp + -1304]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1304], r8
	mov r8, qword ptr [rbp + -1288]
	mov r9, qword ptr [rbp + -1304]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1312], r8
	mov r8, 1
	mov qword ptr [rbp + -1320], r8
	mov r9, qword ptr [rbp + -1312]
	mov r8, r9
	mov qword ptr [rbp + -1328], r8
	mov r8, qword ptr [rbp + -1328]
	mov r9, qword ptr [rbp + -1320]
	xor r8, r9
	mov qword ptr [rbp + -1328], r8
	mov r8, qword ptr [rbp + -1328]
	mov r9, qword ptr [rbp + -1328]
	test r8, r9
	jnz l50
	l51:
	mov r8, 8
	mov qword ptr [rbp + -1336], r8
	mov r9, qword ptr [rbp + -1288]
	mov r8, r9
	mov qword ptr [rbp + -1344], r8
	mov r8, qword ptr [rbp + -1344]
	mov r9, qword ptr [rbp + -1336]
	imul r8, r9
	mov qword ptr [rbp + -1344], r8
	mov r9, qword ptr [rbp + -1272]
	mov r10, qword ptr [rbp + -1344]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1352], r8
	mov r9, qword ptr [rbp + -1352]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1352], r8
	mov r8, 1
	mov qword ptr [rbp + -1360], r8
	mov r9, qword ptr [rbp + -1352]
	mov r8, r9
	mov qword ptr [rbp + -1368], r8
	mov r8, qword ptr [rbp + -1368]
	mov r9, qword ptr [rbp + -1360]
	sub r8, r9
	mov qword ptr [rbp + -1368], r8
	mov r8, 26
	mov qword ptr [rbp + -1376], r8
	mov r9, qword ptr [rbp + -1368]
	mov r10, qword ptr [rbp + -1376]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1384], r8
	mov r8, 26
	mov qword ptr [rbp + -1392], r8
	mov r8, qword ptr [rbp + -1384]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1392]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1384], r8
	mov r9, qword ptr [rbp + -1384]
	mov r8, r9
	mov qword ptr [rbp + -1400], r8
	mov r8, 1
	mov qword ptr [rbp + -1408], r8
	mov r9, qword ptr [rbp + -1408]
	mov r8, r9
	mov qword ptr [rbp + -1416], r8
	l54:
	mov r8, 26
	mov qword ptr [rbp + -1424], r8
	mov r8, qword ptr [rbp + -1416]
	mov r9, qword ptr [rbp + -1424]
	cmp r8, r9
	jge l52
	l53:
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -1432], r8
	mov r8, 1
	mov qword ptr [rbp + -1440], r8
	mov r9, qword ptr [rbp + -1416]
	mov r8, r9
	mov qword ptr [rbp + -1448], r8
	mov r8, qword ptr [rbp + -1448]
	mov r9, qword ptr [rbp + -1440]
	sub r8, r9
	mov qword ptr [rbp + -1448], r8
	mov r9, qword ptr [rbp + -1448]
	mov r8, r9
	mov qword ptr [rbp + -1456], r8
	mov r8, 8
	mov qword ptr [rbp + -1464], r8
	mov r9, qword ptr [rbp + -1432]
	mov r8, r9
	mov qword ptr [rbp + -1472], r8
	mov r8, qword ptr [rbp + -1472]
	mov r9, qword ptr [rbp + -1464]
	sub r8, r9
	mov qword ptr [rbp + -1472], r8
	mov r9, qword ptr [rbp + -1472]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1472], r8
	mov r8, qword ptr [rbp + -1456]
	mov r9, qword ptr [rbp + -1472]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1480], r8
	mov r8, 1
	mov qword ptr [rbp + -1488], r8
	mov r9, qword ptr [rbp + -1480]
	mov r8, r9
	mov qword ptr [rbp + -1496], r8
	mov r8, qword ptr [rbp + -1496]
	mov r9, qword ptr [rbp + -1488]
	xor r8, r9
	mov qword ptr [rbp + -1496], r8
	mov r8, qword ptr [rbp + -1496]
	mov r9, qword ptr [rbp + -1496]
	test r8, r9
	jnz l57
	l58:
	mov r8, 8
	mov qword ptr [rbp + -1504], r8
	mov r9, qword ptr [rbp + -1456]
	mov r8, r9
	mov qword ptr [rbp + -1512], r8
	mov r8, qword ptr [rbp + -1512]
	mov r9, qword ptr [rbp + -1504]
	imul r8, r9
	mov qword ptr [rbp + -1512], r8
	mov r9, qword ptr [rbp + -1432]
	mov r10, qword ptr [rbp + -1512]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1520], r8
	mov r9, qword ptr [rbp + -1520]
	mov r8, r9
	mov qword ptr [rbp + -1528], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -1536], r8
	mov r9, qword ptr [rbp + -1416]
	mov r8, r9
	mov qword ptr [rbp + -1544], r8
	mov r8, 8
	mov qword ptr [rbp + -1552], r8
	mov r9, qword ptr [rbp + -1536]
	mov r8, r9
	mov qword ptr [rbp + -1560], r8
	mov r8, qword ptr [rbp + -1560]
	mov r9, qword ptr [rbp + -1552]
	sub r8, r9
	mov qword ptr [rbp + -1560], r8
	mov r9, qword ptr [rbp + -1560]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1560], r8
	mov r8, qword ptr [rbp + -1544]
	mov r9, qword ptr [rbp + -1560]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1568], r8
	mov r8, 1
	mov qword ptr [rbp + -1576], r8
	mov r9, qword ptr [rbp + -1568]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r8, qword ptr [rbp + -1584]
	mov r9, qword ptr [rbp + -1576]
	xor r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r8, qword ptr [rbp + -1584]
	mov r9, qword ptr [rbp + -1584]
	test r8, r9
	jnz l55
	l56:
	mov r8, 8
	mov qword ptr [rbp + -1592], r8
	mov r9, qword ptr [rbp + -1544]
	mov r8, r9
	mov qword ptr [rbp + -1600], r8
	mov r8, qword ptr [rbp + -1600]
	mov r9, qword ptr [rbp + -1592]
	imul r8, r9
	mov qword ptr [rbp + -1600], r8
	mov r9, qword ptr [rbp + -1536]
	mov r10, qword ptr [rbp + -1600]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1608], r8
	mov r9, qword ptr [rbp + -1608]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1608], r8
	mov r8, 1
	mov qword ptr [rbp + -1616], r8
	mov r9, qword ptr [rbp + -1608]
	mov r8, r9
	mov qword ptr [rbp + -1624], r8
	mov r8, qword ptr [rbp + -1624]
	mov r9, qword ptr [rbp + -1616]
	sub r8, r9
	mov qword ptr [rbp + -1624], r8
	mov r8, 26
	mov qword ptr [rbp + -1632], r8
	mov r9, qword ptr [rbp + -1624]
	mov r10, qword ptr [rbp + -1632]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1640], r8
	mov r8, 26
	mov qword ptr [rbp + -1648], r8
	mov r8, qword ptr [rbp + -1640]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1648]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1640], r8
	mov r8, qword ptr [rbp + -1528]
	mov r9, qword ptr [rbp + -1640]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1656], r8
	mov r9, qword ptr [rbp + -1416]
	mov r10, qword ptr [rbp + -1656]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1664], r8
	mov r9, qword ptr [rbp + -1664]
	mov r8, r9
	mov qword ptr [rbp + -1416], r8
	jmp l54
	l55:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l56
	l57:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l58
	l52:
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -1672], r8
	mov r8, 25
	mov qword ptr [rbp + -1680], r8
	mov r9, qword ptr [rbp + -1680]
	mov r8, r9
	mov qword ptr [rbp + -1688], r8
	mov r8, 8
	mov qword ptr [rbp + -1696], r8
	mov r9, qword ptr [rbp + -1672]
	mov r8, r9
	mov qword ptr [rbp + -1704], r8
	mov r8, qword ptr [rbp + -1704]
	mov r9, qword ptr [rbp + -1696]
	sub r8, r9
	mov qword ptr [rbp + -1704], r8
	mov r9, qword ptr [rbp + -1704]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1704], r8
	mov r8, qword ptr [rbp + -1688]
	mov r9, qword ptr [rbp + -1704]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1712], r8
	mov r8, 1
	mov qword ptr [rbp + -1720], r8
	mov r9, qword ptr [rbp + -1712]
	mov r8, r9
	mov qword ptr [rbp + -1728], r8
	mov r8, qword ptr [rbp + -1728]
	mov r9, qword ptr [rbp + -1720]
	xor r8, r9
	mov qword ptr [rbp + -1728], r8
	mov r8, qword ptr [rbp + -1728]
	mov r9, qword ptr [rbp + -1728]
	test r8, r9
	jnz l59
	l60:
	mov r8, 8
	mov qword ptr [rbp + -1736], r8
	mov r9, qword ptr [rbp + -1688]
	mov r8, r9
	mov qword ptr [rbp + -1744], r8
	mov r8, qword ptr [rbp + -1744]
	mov r9, qword ptr [rbp + -1736]
	imul r8, r9
	mov qword ptr [rbp + -1744], r8
	mov r9, qword ptr [rbp + -1672]
	mov r10, qword ptr [rbp + -1744]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1752], r8
	mov r8, qword ptr [rbp + -1752]
	mov r9, qword ptr [rbp + -1400]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1760], r8
	mov r9, qword ptr [rbp + -680]
	mov r10, qword ptr [rbp + -1760]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1768], r8
	mov r9, qword ptr [rbp + -1768]
	mov r8, r9
	mov qword ptr [rbp + -680], r8
	jmp l31
	l59:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l60
	l50:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l51
	l29:
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -1776], r8
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -1784], r8
	mov r8, 0
	mov qword ptr [rbp + -1792], r8
	mov r9, qword ptr [rbp + -1792]
	mov r8, r9
	mov qword ptr [rbp + -1800], r8
	mov r8, qword ptr [rbp + -1800]
	mov rax, r8
	mov r8, qword ptr [rbp + -1784]
	mov rdx, r8
	mov r8, qword ptr [rbp + -1776]
	push r8
	leave
	ret
_IrotorEncryptForward_iaaiaaiii:
	enter 264, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r8, rdx
	mov qword ptr [rbp + -24], r8
	mov r8, rcx
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, 8
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -104]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, qword ptr [rbp + -120]
	sub r8, r9
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -128]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -136], r8
	mov r8, 1
	mov qword ptr [rbp + -144], r8
	mov r9, qword ptr [rbp + -136]
	mov r8, r9
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -144]
	xor r8, r9
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -152]
	test r8, r9
	jnz l61
	l62:
	mov r8, 8
	mov qword ptr [rbp + -160], r8
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -160]
	imul r8, r9
	mov qword ptr [rbp + -168], r8
	mov r9, qword ptr [rbp + -104]
	mov r10, qword ptr [rbp + -168]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -176]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -176]
	mov r8, r9
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -192], r8
	mov r8, 8
	mov qword ptr [rbp + -200], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -200]
	sub r8, r9
	mov qword ptr [rbp + -208], r8
	mov r9, qword ptr [rbp + -208]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -192]
	mov r9, qword ptr [rbp + -208]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -216], r8
	mov r8, 1
	mov qword ptr [rbp + -224], r8
	mov r9, qword ptr [rbp + -216]
	mov r8, r9
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -224]
	xor r8, r9
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -232]
	test r8, r9
	jnz l63
	l64:
	mov r8, 8
	mov qword ptr [rbp + -240], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -248], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, qword ptr [rbp + -240]
	imul r8, r9
	mov qword ptr [rbp + -248], r8
	mov r9, qword ptr [rbp + -184]
	mov r10, qword ptr [rbp + -248]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -256], r8
	mov r9, qword ptr [rbp + -256]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -256], r8
	mov r9, qword ptr [rbp + -256]
	mov r8, r9
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov rax, r8
	leave
	ret
	l63:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l64
	l61:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l62
_IrotorEncryptBack_iaaiaaiii:
	enter 264, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, rsi
	mov qword ptr [rbp + -16], r8
	mov r8, rdx
	mov qword ptr [rbp + -24], r8
	mov r8, rcx
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r9, qword ptr [rbp + -40]
	mov r8, r9
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -72]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, 8
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -104]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, qword ptr [rbp + -120]
	sub r8, r9
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, qword ptr [rbp + -128]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -136], r8
	mov r8, 1
	mov qword ptr [rbp + -144], r8
	mov r9, qword ptr [rbp + -136]
	mov r8, r9
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -144]
	xor r8, r9
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -152]
	test r8, r9
	jnz l65
	l66:
	mov r8, 8
	mov qword ptr [rbp + -160], r8
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -168], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -160]
	imul r8, r9
	mov qword ptr [rbp + -168], r8
	mov r9, qword ptr [rbp + -104]
	mov r10, qword ptr [rbp + -168]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -176]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -176]
	mov r8, r9
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -192], r8
	mov r8, 8
	mov qword ptr [rbp + -200], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, qword ptr [rbp + -200]
	sub r8, r9
	mov qword ptr [rbp + -208], r8
	mov r9, qword ptr [rbp + -208]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -192]
	mov r9, qword ptr [rbp + -208]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -216], r8
	mov r8, 1
	mov qword ptr [rbp + -224], r8
	mov r9, qword ptr [rbp + -216]
	mov r8, r9
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -224]
	xor r8, r9
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, qword ptr [rbp + -232]
	test r8, r9
	jnz l67
	l68:
	mov r8, 8
	mov qword ptr [rbp + -240], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -248], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, qword ptr [rbp + -240]
	imul r8, r9
	mov qword ptr [rbp + -248], r8
	mov r9, qword ptr [rbp + -184]
	mov r10, qword ptr [rbp + -248]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -256], r8
	mov r9, qword ptr [rbp + -256]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -256], r8
	mov r9, qword ptr [rbp + -256]
	mov r8, r9
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov rax, r8
	leave
	ret
	l67:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l68
	l65:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l66
_ImakeReflector_aiai:
	enter 440, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r8, 216
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -40]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, 26
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, qword ptr [rbp + -80]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -72]
	mov r10, qword ptr [rbp + -88]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r9, qword ptr [rbp + -104]
	mov r8, r9
	mov qword ptr [rbp + -112], r8
	mov r8, 0
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -120]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	l71:
	mov r8, 26
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, qword ptr [rbp + -136]
	cmp r8, r9
	jge l69
	l70:
	mov r8, 8
	mov qword ptr [rbp + -144], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, r9
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -152]
	mov r9, qword ptr [rbp + -144]
	imul r8, r9
	mov qword ptr [rbp + -152], r8
	mov r9, qword ptr [rbp + -104]
	mov r10, qword ptr [rbp + -152]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, qword ptr [rbp + -168]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -176], r8
	mov r9, qword ptr [rbp + -128]
	mov r10, qword ptr [rbp + -176]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -184]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	jmp l71
	l69:
	mov r8, 0
	mov qword ptr [rbp + -192], r8
	mov r9, qword ptr [rbp + -192]
	mov r8, r9
	mov qword ptr [rbp + -200], r8
	l74:
	mov r8, 26
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -200]
	mov r9, qword ptr [rbp + -208]
	cmp r8, r9
	jge l72
	l73:
	mov r9, qword ptr [rbp + -24]
	mov r8, r9
	mov qword ptr [rbp + -216], r8
	mov r9, qword ptr [rbp + -200]
	mov r8, r9
	mov qword ptr [rbp + -224], r8
	mov r8, 8
	mov qword ptr [rbp + -232], r8
	mov r9, qword ptr [rbp + -216]
	mov r8, r9
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -240]
	mov r9, qword ptr [rbp + -232]
	sub r8, r9
	mov qword ptr [rbp + -240], r8
	mov r9, qword ptr [rbp + -240]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -240], r8
	mov r8, qword ptr [rbp + -224]
	mov r9, qword ptr [rbp + -240]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -248], r8
	mov r8, 1
	mov qword ptr [rbp + -256], r8
	mov r9, qword ptr [rbp + -248]
	mov r8, r9
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, qword ptr [rbp + -256]
	xor r8, r9
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, qword ptr [rbp + -264]
	test r8, r9
	jnz l75
	l76:
	mov r8, 8
	mov qword ptr [rbp + -272], r8
	mov r9, qword ptr [rbp + -224]
	mov r8, r9
	mov qword ptr [rbp + -280], r8
	mov r8, qword ptr [rbp + -280]
	mov r9, qword ptr [rbp + -272]
	imul r8, r9
	mov qword ptr [rbp + -280], r8
	mov r9, qword ptr [rbp + -216]
	mov r10, qword ptr [rbp + -280]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -288], r8
	mov r9, qword ptr [rbp + -288]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -288], r8
	mov r9, qword ptr [rbp + -288]
	mov r8, r9
	mov qword ptr [rbp + -296], r8
	mov r8, qword ptr [rbp + -296]
	mov rdi, r8
	and rsp, -16
	call _ItoLower_ii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -304], r8
	mov r9, qword ptr [rbp + -304]
	mov r8, r9
	mov qword ptr [rbp + -312], r8
	mov r8, 97
	mov qword ptr [rbp + -320], r8
	mov r9, qword ptr [rbp + -312]
	mov r8, r9
	mov qword ptr [rbp + -328], r8
	mov r8, qword ptr [rbp + -328]
	mov r9, qword ptr [rbp + -320]
	sub r8, r9
	mov qword ptr [rbp + -328], r8
	mov r9, qword ptr [rbp + -328]
	mov r8, r9
	mov qword ptr [rbp + -336], r8
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -344], r8
	mov r9, qword ptr [rbp + -200]
	mov r8, r9
	mov qword ptr [rbp + -352], r8
	mov r8, 8
	mov qword ptr [rbp + -360], r8
	mov r9, qword ptr [rbp + -344]
	mov r8, r9
	mov qword ptr [rbp + -368], r8
	mov r8, qword ptr [rbp + -368]
	mov r9, qword ptr [rbp + -360]
	sub r8, r9
	mov qword ptr [rbp + -368], r8
	mov r9, qword ptr [rbp + -368]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -368], r8
	mov r8, qword ptr [rbp + -352]
	mov r9, qword ptr [rbp + -368]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -376], r8
	mov r8, 1
	mov qword ptr [rbp + -384], r8
	mov r9, qword ptr [rbp + -376]
	mov r8, r9
	mov qword ptr [rbp + -392], r8
	mov r8, qword ptr [rbp + -392]
	mov r9, qword ptr [rbp + -384]
	xor r8, r9
	mov qword ptr [rbp + -392], r8
	mov r8, qword ptr [rbp + -392]
	mov r9, qword ptr [rbp + -392]
	test r8, r9
	jnz l77
	l78:
	mov r8, 8
	mov qword ptr [rbp + -400], r8
	mov r9, qword ptr [rbp + -352]
	mov r8, r9
	mov qword ptr [rbp + -408], r8
	mov r8, qword ptr [rbp + -408]
	mov r9, qword ptr [rbp + -400]
	imul r8, r9
	mov qword ptr [rbp + -408], r8
	mov r9, qword ptr [rbp + -344]
	mov r10, qword ptr [rbp + -408]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -416], r8
	mov r8, qword ptr [rbp + -416]
	mov r9, qword ptr [rbp + -336]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -424], r8
	mov r9, qword ptr [rbp + -200]
	mov r10, qword ptr [rbp + -424]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -432], r8
	mov r9, qword ptr [rbp + -432]
	mov r8, r9
	mov qword ptr [rbp + -200], r8
	jmp l74
	l77:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l78
	l75:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l76
	l72:
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -440], r8
	mov r8, qword ptr [rbp + -440]
	mov rax, r8
	leave
	ret
_IreflectorEncrypt_iaii:
	enter 136, 0
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
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r8, 8
	mov qword ptr [rbp + -72], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov r9, qword ptr [rbp + -72]
	sub r8, r9
	mov qword ptr [rbp + -80], r8
	mov r9, qword ptr [rbp + -80]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, qword ptr [rbp + -80]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -88], r8
	mov r8, 1
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -88]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -96]
	xor r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -104]
	test r8, r9
	jnz l79
	l80:
	mov r8, 8
	mov qword ptr [rbp + -112], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -120]
	mov r9, qword ptr [rbp + -112]
	imul r8, r9
	mov qword ptr [rbp + -120], r8
	mov r9, qword ptr [rbp + -56]
	mov r10, qword ptr [rbp + -120]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, r9
	mov qword ptr [rbp + -136], r8
	mov r8, qword ptr [rbp + -136]
	mov rax, r8
	leave
	ret
	l79:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l80
_Imain_paai:
	enter 3264, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r9, qword ptr [rbp + -8]
	mov r8, r9
	mov qword ptr [rbp + -16], r8
	mov r9, qword ptr [rbp + -16]
	mov r8, r9
	mov qword ptr [rbp + -24], r8
	mov r8, 40
	mov qword ptr [rbp + -32], r8
	mov r9, qword ptr [rbp + -32]
	mov r8, r9
	mov qword ptr [rbp + -40], r8
	mov r8, qword ptr [rbp + -40]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -56], r8
	mov r9, qword ptr [rbp + -56]
	mov r8, r9
	mov qword ptr [rbp + -64], r8
	mov r9, qword ptr [rbp + -64]
	mov r8, r9
	mov qword ptr [rbp + -72], r8
	mov r8, 4
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -72]
	mov r9, qword ptr [rbp + -80]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -88], r8
	mov r9, qword ptr [rbp + -72]
	mov r10, qword ptr [rbp + -88]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -96], r8
	mov r9, qword ptr [rbp + -96]
	mov r8, r9
	mov qword ptr [rbp + -104], r8
	mov r8, 72
	mov qword ptr [rbp + -112], r8
	mov r9, qword ptr [rbp + -112]
	mov r8, r9
	mov qword ptr [rbp + -120], r8
	mov r8, qword ptr [rbp + -120]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -128], r8
	mov r9, qword ptr [rbp + -128]
	mov r8, r9
	mov qword ptr [rbp + -136], r8
	mov r9, qword ptr [rbp + -136]
	mov r8, r9
	mov qword ptr [rbp + -144], r8
	mov r8, 8
	mov qword ptr [rbp + -152], r8
	mov r8, qword ptr [rbp + -144]
	mov r9, qword ptr [rbp + -152]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -160], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -160]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -168], r8
	mov r8, 12
	mov qword ptr [rbp + -176], r8
	mov r8, qword ptr [rbp + -168]
	mov r9, qword ptr [rbp + -176]
	mov qword ptr [r8], r9
	mov r8, 16
	mov qword ptr [rbp + -184], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -184]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -192], r8
	mov r8, 27
	mov qword ptr [rbp + -200], r8
	mov r8, qword ptr [rbp + -192]
	mov r9, qword ptr [rbp + -200]
	mov qword ptr [r8], r9
	mov r8, 24
	mov qword ptr [rbp + -208], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -208]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -216], r8
	mov r8, 6
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, qword ptr [rbp + -224]
	mov qword ptr [r8], r9
	mov r8, 32
	mov qword ptr [rbp + -232], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -232]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -240], r8
	mov r8, 57
	mov qword ptr [rbp + -248], r8
	mov r8, qword ptr [rbp + -240]
	mov r9, qword ptr [rbp + -248]
	mov qword ptr [r8], r9
	mov r8, 40
	mov qword ptr [rbp + -256], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -256]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -264], r8
	mov r8, 25
	mov qword ptr [rbp + -272], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, qword ptr [rbp + -272]
	mov qword ptr [r8], r9
	mov r8, 48
	mov qword ptr [rbp + -280], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -280]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -288], r8
	mov r8, 51
	mov qword ptr [rbp + -296], r8
	mov r8, qword ptr [rbp + -288]
	mov r9, qword ptr [rbp + -296]
	mov qword ptr [r8], r9
	mov r8, 56
	mov qword ptr [rbp + -304], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -304]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -312], r8
	mov r8, 52
	mov qword ptr [rbp + -320], r8
	mov r8, qword ptr [rbp + -312]
	mov r9, qword ptr [rbp + -320]
	mov qword ptr [r8], r9
	mov r8, 64
	mov qword ptr [rbp + -328], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -328]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -336], r8
	mov r8, -1
	mov qword ptr [rbp + -344], r8
	mov r8, qword ptr [rbp + -336]
	mov r9, qword ptr [rbp + -344]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -352], r8
	mov r9, qword ptr [rbp + -144]
	mov r10, qword ptr [rbp + -352]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -360], r8
	mov r8, qword ptr [rbp + -104]
	mov r9, qword ptr [rbp + -360]
	mov qword ptr [r8], r9
	mov r8, 16
	mov qword ptr [rbp + -368], r8
	mov r9, qword ptr [rbp + -72]
	mov r10, qword ptr [rbp + -368]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -376], r8
	mov r9, qword ptr [rbp + -376]
	mov r8, r9
	mov qword ptr [rbp + -384], r8
	mov r8, 72
	mov qword ptr [rbp + -392], r8
	mov r9, qword ptr [rbp + -392]
	mov r8, r9
	mov qword ptr [rbp + -400], r8
	mov r8, qword ptr [rbp + -400]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -408], r8
	mov r9, qword ptr [rbp + -408]
	mov r8, r9
	mov qword ptr [rbp + -416], r8
	mov r9, qword ptr [rbp + -416]
	mov r8, r9
	mov qword ptr [rbp + -424], r8
	mov r8, 8
	mov qword ptr [rbp + -432], r8
	mov r8, qword ptr [rbp + -424]
	mov r9, qword ptr [rbp + -432]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -440], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -440]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -448], r8
	mov r8, 12
	mov qword ptr [rbp + -456], r8
	mov r8, qword ptr [rbp + -448]
	mov r9, qword ptr [rbp + -456]
	mov qword ptr [r8], r9
	mov r8, 16
	mov qword ptr [rbp + -464], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -464]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -472], r8
	mov r8, 27
	mov qword ptr [rbp + -480], r8
	mov r8, qword ptr [rbp + -472]
	mov r9, qword ptr [rbp + -480]
	mov qword ptr [r8], r9
	mov r8, 24
	mov qword ptr [rbp + -488], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -488]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -496], r8
	mov r8, 6
	mov qword ptr [rbp + -504], r8
	mov r8, qword ptr [rbp + -496]
	mov r9, qword ptr [rbp + -504]
	mov qword ptr [r8], r9
	mov r8, 32
	mov qword ptr [rbp + -512], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -512]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -520], r8
	mov r8, 55
	mov qword ptr [rbp + -528], r8
	mov r8, qword ptr [rbp + -520]
	mov r9, qword ptr [rbp + -528]
	mov qword ptr [r8], r9
	mov r8, 40
	mov qword ptr [rbp + -536], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -536]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -544], r8
	mov r8, 25
	mov qword ptr [rbp + -552], r8
	mov r8, qword ptr [rbp + -544]
	mov r9, qword ptr [rbp + -552]
	mov qword ptr [r8], r9
	mov r8, 48
	mov qword ptr [rbp + -560], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -560]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -568], r8
	mov r8, 51
	mov qword ptr [rbp + -576], r8
	mov r8, qword ptr [rbp + -568]
	mov r9, qword ptr [rbp + -576]
	mov qword ptr [r8], r9
	mov r8, 56
	mov qword ptr [rbp + -584], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -584]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -592], r8
	mov r8, 52
	mov qword ptr [rbp + -600], r8
	mov r8, qword ptr [rbp + -592]
	mov r9, qword ptr [rbp + -600]
	mov qword ptr [r8], r9
	mov r8, 64
	mov qword ptr [rbp + -608], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -608]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -616], r8
	mov r8, -1
	mov qword ptr [rbp + -624], r8
	mov r8, qword ptr [rbp + -616]
	mov r9, qword ptr [rbp + -624]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -632], r8
	mov r9, qword ptr [rbp + -424]
	mov r10, qword ptr [rbp + -632]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -640], r8
	mov r8, qword ptr [rbp + -384]
	mov r9, qword ptr [rbp + -640]
	mov qword ptr [r8], r9
	mov r8, 24
	mov qword ptr [rbp + -648], r8
	mov r9, qword ptr [rbp + -72]
	mov r10, qword ptr [rbp + -648]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -656], r8
	mov r9, qword ptr [rbp + -656]
	mov r8, r9
	mov qword ptr [rbp + -664], r8
	mov r8, 40
	mov qword ptr [rbp + -672], r8
	mov r9, qword ptr [rbp + -672]
	mov r8, r9
	mov qword ptr [rbp + -680], r8
	mov r8, qword ptr [rbp + -680]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -688], r8
	mov r9, qword ptr [rbp + -688]
	mov r8, r9
	mov qword ptr [rbp + -696], r8
	mov r9, qword ptr [rbp + -696]
	mov r8, r9
	mov qword ptr [rbp + -704], r8
	mov r8, 4
	mov qword ptr [rbp + -712], r8
	mov r8, qword ptr [rbp + -704]
	mov r9, qword ptr [rbp + -712]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -720], r8
	mov r9, qword ptr [rbp + -704]
	mov r10, qword ptr [rbp + -720]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -728], r8
	mov r8, 12
	mov qword ptr [rbp + -736], r8
	mov r8, qword ptr [rbp + -728]
	mov r9, qword ptr [rbp + -736]
	mov qword ptr [r8], r9
	mov r8, 16
	mov qword ptr [rbp + -744], r8
	mov r9, qword ptr [rbp + -704]
	mov r10, qword ptr [rbp + -744]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -752], r8
	mov r8, 46
	mov qword ptr [rbp + -760], r8
	mov r8, qword ptr [rbp + -752]
	mov r9, qword ptr [rbp + -760]
	mov qword ptr [r8], r9
	mov r8, 24
	mov qword ptr [rbp + -768], r8
	mov r9, qword ptr [rbp + -704]
	mov r10, qword ptr [rbp + -768]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -776], r8
	mov r8, 47
	mov qword ptr [rbp + -784], r8
	mov r8, qword ptr [rbp + -776]
	mov r9, qword ptr [rbp + -784]
	mov qword ptr [r8], r9
	mov r8, 32
	mov qword ptr [rbp + -792], r8
	mov r9, qword ptr [rbp + -704]
	mov r10, qword ptr [rbp + -792]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -800], r8
	mov r8, -1
	mov qword ptr [rbp + -808], r8
	mov r8, qword ptr [rbp + -800]
	mov r9, qword ptr [rbp + -808]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -816], r8
	mov r9, qword ptr [rbp + -704]
	mov r10, qword ptr [rbp + -816]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -824], r8
	mov r8, qword ptr [rbp + -664]
	mov r9, qword ptr [rbp + -824]
	mov qword ptr [r8], r9
	mov r8, 32
	mov qword ptr [rbp + -832], r8
	mov r9, qword ptr [rbp + -72]
	mov r10, qword ptr [rbp + -832]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -840], r8
	mov r9, qword ptr [rbp + -840]
	mov r8, r9
	mov qword ptr [rbp + -848], r8
	mov r8, 64
	mov qword ptr [rbp + -856], r8
	mov r9, qword ptr [rbp + -856]
	mov r8, r9
	mov qword ptr [rbp + -864], r8
	mov r8, qword ptr [rbp + -864]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -872], r8
	mov r9, qword ptr [rbp + -872]
	mov r8, r9
	mov qword ptr [rbp + -880], r8
	mov r9, qword ptr [rbp + -880]
	mov r8, r9
	mov qword ptr [rbp + -888], r8
	mov r8, 7
	mov qword ptr [rbp + -896], r8
	mov r8, qword ptr [rbp + -888]
	mov r9, qword ptr [rbp + -896]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -904], r8
	mov r9, qword ptr [rbp + -888]
	mov r10, qword ptr [rbp + -904]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -912], r8
	mov r8, 12
	mov qword ptr [rbp + -920], r8
	mov r8, qword ptr [rbp + -912]
	mov r9, qword ptr [rbp + -920]
	mov qword ptr [r8], r9
	mov r8, 16
	mov qword ptr [rbp + -928], r8
	mov r9, qword ptr [rbp + -888]
	mov r10, qword ptr [rbp + -928]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -936], r8
	mov r8, 27
	mov qword ptr [rbp + -944], r8
	mov r8, qword ptr [rbp + -936]
	mov r9, qword ptr [rbp + -944]
	mov qword ptr [r8], r9
	mov r8, 24
	mov qword ptr [rbp + -952], r8
	mov r9, qword ptr [rbp + -888]
	mov r10, qword ptr [rbp + -952]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -960], r8
	mov r8, 6
	mov qword ptr [rbp + -968], r8
	mov r8, qword ptr [rbp + -960]
	mov r9, qword ptr [rbp + -968]
	mov qword ptr [r8], r9
	mov r8, 32
	mov qword ptr [rbp + -976], r8
	mov r9, qword ptr [rbp + -888]
	mov r10, qword ptr [rbp + -976]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -984], r8
	mov r8, 16
	mov qword ptr [rbp + -992], r8
	mov r8, qword ptr [rbp + -984]
	mov r9, qword ptr [rbp + -992]
	mov qword ptr [r8], r9
	mov r8, 40
	mov qword ptr [rbp + -1000], r8
	mov r9, qword ptr [rbp + -888]
	mov r10, qword ptr [rbp + -1000]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1008], r8
	mov r8, 11
	mov qword ptr [rbp + -1016], r8
	mov r8, qword ptr [rbp + -1008]
	mov r9, qword ptr [rbp + -1016]
	mov qword ptr [r8], r9
	mov r8, 48
	mov qword ptr [rbp + -1024], r8
	mov r9, qword ptr [rbp + -888]
	mov r10, qword ptr [rbp + -1024]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1032], r8
	mov r8, 52
	mov qword ptr [rbp + -1040], r8
	mov r8, qword ptr [rbp + -1032]
	mov r9, qword ptr [rbp + -1040]
	mov qword ptr [r8], r9
	mov r8, 56
	mov qword ptr [rbp + -1048], r8
	mov r9, qword ptr [rbp + -888]
	mov r10, qword ptr [rbp + -1048]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1056], r8
	mov r8, -1
	mov qword ptr [rbp + -1064], r8
	mov r8, qword ptr [rbp + -1056]
	mov r9, qword ptr [rbp + -1064]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -1072], r8
	mov r9, qword ptr [rbp + -888]
	mov r10, qword ptr [rbp + -1072]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1080], r8
	mov r8, qword ptr [rbp + -848]
	mov r9, qword ptr [rbp + -1080]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -1088], r8
	mov r9, qword ptr [rbp + -72]
	mov r10, qword ptr [rbp + -1088]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1096], r8
	mov r9, qword ptr [rbp + -1096]
	mov r8, r9
	mov qword ptr [rbp + -1104], r8
	lea r8, qword ptr g0[rip]
	mov qword ptr [rbp + -1112], r8
	mov r9, qword ptr [rbp + -1112]
	mov r8, r9
	mov qword ptr [rbp + -1120], r8
	mov r8, 8
	mov qword ptr [rbp + -1128], r8
	mov r9, qword ptr [rbp + -1120]
	mov r10, qword ptr [rbp + -1128]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1136], r8
	mov r9, qword ptr [rbp + -1136]
	mov r8, r9
	mov qword ptr [rbp + -1144], r8
	mov r9, qword ptr [rbp + -1144]
	mov r8, r9
	mov qword ptr [rbp + -1152], r8
	sub rsp, 8
	mov rdi, rsp
	mov r8, qword ptr [rbp + -1152]
	mov rsi, r8
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov r8, qword ptr [rbp + -1160]
	pop r8
	mov r8, rdx
	mov qword ptr [rbp + -1168], r8
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -1176], r8
	mov r9, qword ptr [rbp + -1176]
	mov r8, r9
	mov qword ptr [rbp + -1184], r8
	mov r9, qword ptr [rbp + -1168]
	mov r8, r9
	mov qword ptr [rbp + -1192], r8
	mov r9, qword ptr [rbp + -1192]
	mov r8, r9
	mov qword ptr [rbp + -1200], r8
	mov r9, qword ptr [rbp + -1160]
	mov r8, r9
	mov qword ptr [rbp + -1208], r8
	mov r9, qword ptr [rbp + -1208]
	mov r8, r9
	mov qword ptr [rbp + -1216], r8
	lea r8, qword ptr g1[rip]
	mov qword ptr [rbp + -1224], r8
	mov r9, qword ptr [rbp + -1224]
	mov r8, r9
	mov qword ptr [rbp + -1232], r8
	mov r8, 8
	mov qword ptr [rbp + -1240], r8
	mov r9, qword ptr [rbp + -1232]
	mov r10, qword ptr [rbp + -1240]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1248], r8
	mov r9, qword ptr [rbp + -1248]
	mov r8, r9
	mov qword ptr [rbp + -1256], r8
	mov r9, qword ptr [rbp + -1256]
	mov r8, r9
	mov qword ptr [rbp + -1264], r8
	sub rsp, 8
	mov rdi, rsp
	mov r8, qword ptr [rbp + -1264]
	mov rsi, r8
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov r8, qword ptr [rbp + -1160]
	pop r8
	mov r8, rdx
	mov qword ptr [rbp + -1168], r8
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -1272], r8
	mov r9, qword ptr [rbp + -1272]
	mov r8, r9
	mov qword ptr [rbp + -1280], r8
	mov r9, qword ptr [rbp + -1168]
	mov r8, r9
	mov qword ptr [rbp + -1288], r8
	mov r9, qword ptr [rbp + -1288]
	mov r8, r9
	mov qword ptr [rbp + -1296], r8
	mov r9, qword ptr [rbp + -1160]
	mov r8, r9
	mov qword ptr [rbp + -1304], r8
	mov r9, qword ptr [rbp + -1304]
	mov r8, r9
	mov qword ptr [rbp + -1312], r8
	lea r8, qword ptr g2[rip]
	mov qword ptr [rbp + -1320], r8
	mov r9, qword ptr [rbp + -1320]
	mov r8, r9
	mov qword ptr [rbp + -1328], r8
	mov r8, 8
	mov qword ptr [rbp + -1336], r8
	mov r9, qword ptr [rbp + -1328]
	mov r10, qword ptr [rbp + -1336]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1344], r8
	mov r9, qword ptr [rbp + -1344]
	mov r8, r9
	mov qword ptr [rbp + -1352], r8
	mov r9, qword ptr [rbp + -1352]
	mov r8, r9
	mov qword ptr [rbp + -1360], r8
	sub rsp, 8
	mov rdi, rsp
	mov r8, qword ptr [rbp + -1360]
	mov rsi, r8
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov r8, qword ptr [rbp + -1160]
	pop r8
	mov r8, rdx
	mov qword ptr [rbp + -1168], r8
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -1368], r8
	mov r9, qword ptr [rbp + -1368]
	mov r8, r9
	mov qword ptr [rbp + -1376], r8
	mov r9, qword ptr [rbp + -1168]
	mov r8, r9
	mov qword ptr [rbp + -1384], r8
	mov r9, qword ptr [rbp + -1384]
	mov r8, r9
	mov qword ptr [rbp + -1392], r8
	mov r9, qword ptr [rbp + -1160]
	mov r8, r9
	mov qword ptr [rbp + -1400], r8
	mov r9, qword ptr [rbp + -1400]
	mov r8, r9
	mov qword ptr [rbp + -1408], r8
	lea r8, qword ptr g3[rip]
	mov qword ptr [rbp + -1416], r8
	mov r9, qword ptr [rbp + -1416]
	mov r8, r9
	mov qword ptr [rbp + -1424], r8
	mov r8, 8
	mov qword ptr [rbp + -1432], r8
	mov r9, qword ptr [rbp + -1424]
	mov r10, qword ptr [rbp + -1432]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1440], r8
	mov r9, qword ptr [rbp + -1440]
	mov r8, r9
	mov qword ptr [rbp + -1448], r8
	mov r9, qword ptr [rbp + -1448]
	mov r8, r9
	mov qword ptr [rbp + -1456], r8
	mov r8, qword ptr [rbp + -1456]
	mov rdi, r8
	and rsp, -16
	call _ImakeReflector_aiai
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -1464], r8
	mov r9, qword ptr [rbp + -1464]
	mov r8, r9
	mov qword ptr [rbp + -1472], r8
	mov r9, qword ptr [rbp + -1472]
	mov r8, r9
	mov qword ptr [rbp + -1480], r8
	mov r8, 0
	mov qword ptr [rbp + -1488], r8
	mov r9, qword ptr [rbp + -1488]
	mov r8, r9
	mov qword ptr [rbp + -1496], r8
	l83:
	mov r8, 17576
	mov qword ptr [rbp + -1504], r8
	mov r8, qword ptr [rbp + -1496]
	mov r9, qword ptr [rbp + -1504]
	cmp r8, r9
	jge l81
	l82:
	mov r8, 0
	mov qword ptr [rbp + -1512], r8
	mov r9, qword ptr [rbp + -1512]
	mov r8, r9
	mov qword ptr [rbp + -1520], r8
	l86:
	mov r8, 26
	mov qword ptr [rbp + -1528], r8
	mov r8, qword ptr [rbp + -1520]
	mov r9, qword ptr [rbp + -1528]
	cmp r8, r9
	jge l84
	l85:
	mov r8, 1
	mov qword ptr [rbp + -1536], r8
	mov r9, qword ptr [rbp + -1536]
	mov r8, r9
	mov qword ptr [rbp + -1544], r8
	mov r8, 0
	mov qword ptr [rbp + -1552], r8
	mov r9, qword ptr [rbp + -1552]
	mov r8, r9
	mov qword ptr [rbp + -1560], r8
	l89:
	mov r8, 8
	mov qword ptr [rbp + -1568], r8
	mov r9, qword ptr [rbp + -1104]
	mov r8, r9
	mov qword ptr [rbp + -1576], r8
	mov r8, qword ptr [rbp + -1576]
	mov r9, qword ptr [rbp + -1568]
	sub r8, r9
	mov qword ptr [rbp + -1576], r8
	mov r9, qword ptr [rbp + -1576]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1576], r8
	mov r8, qword ptr [rbp + -1560]
	mov r9, qword ptr [rbp + -1576]
	cmp r8, r9
	jge l87
	l88:
	mov r9, qword ptr [rbp + -1520]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r8, 0
	mov qword ptr [rbp + -1592], r8
	mov r9, qword ptr [rbp + -1592]
	mov r8, r9
	mov qword ptr [rbp + -1600], r8
	l92:
	mov r9, qword ptr [rbp + -1104]
	mov r8, r9
	mov qword ptr [rbp + -1608], r8
	mov r9, qword ptr [rbp + -1560]
	mov r8, r9
	mov qword ptr [rbp + -1616], r8
	mov r8, 8
	mov qword ptr [rbp + -1624], r8
	mov r9, qword ptr [rbp + -1608]
	mov r8, r9
	mov qword ptr [rbp + -1632], r8
	mov r8, qword ptr [rbp + -1632]
	mov r9, qword ptr [rbp + -1624]
	sub r8, r9
	mov qword ptr [rbp + -1632], r8
	mov r9, qword ptr [rbp + -1632]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1632], r8
	mov r8, qword ptr [rbp + -1616]
	mov r9, qword ptr [rbp + -1632]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1640], r8
	mov r8, 1
	mov qword ptr [rbp + -1648], r8
	mov r9, qword ptr [rbp + -1640]
	mov r8, r9
	mov qword ptr [rbp + -1656], r8
	mov r8, qword ptr [rbp + -1656]
	mov r9, qword ptr [rbp + -1648]
	xor r8, r9
	mov qword ptr [rbp + -1656], r8
	mov r8, qword ptr [rbp + -1656]
	mov r9, qword ptr [rbp + -1656]
	test r8, r9
	jnz l93
	l94:
	mov r8, 8
	mov qword ptr [rbp + -1664], r8
	mov r9, qword ptr [rbp + -1616]
	mov r8, r9
	mov qword ptr [rbp + -1672], r8
	mov r8, qword ptr [rbp + -1672]
	mov r9, qword ptr [rbp + -1664]
	imul r8, r9
	mov qword ptr [rbp + -1672], r8
	mov r9, qword ptr [rbp + -1608]
	mov r10, qword ptr [rbp + -1672]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1680], r8
	mov r9, qword ptr [rbp + -1680]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1680], r8
	mov r9, qword ptr [rbp + -1680]
	mov r8, r9
	mov qword ptr [rbp + -1688], r8
	mov r9, qword ptr [rbp + -1600]
	mov r8, r9
	mov qword ptr [rbp + -1696], r8
	mov r8, 8
	mov qword ptr [rbp + -1704], r8
	mov r9, qword ptr [rbp + -1688]
	mov r8, r9
	mov qword ptr [rbp + -1712], r8
	mov r8, qword ptr [rbp + -1712]
	mov r9, qword ptr [rbp + -1704]
	sub r8, r9
	mov qword ptr [rbp + -1712], r8
	mov r9, qword ptr [rbp + -1712]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1712], r8
	mov r8, qword ptr [rbp + -1696]
	mov r9, qword ptr [rbp + -1712]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1720], r8
	mov r8, 1
	mov qword ptr [rbp + -1728], r8
	mov r9, qword ptr [rbp + -1720]
	mov r8, r9
	mov qword ptr [rbp + -1736], r8
	mov r8, qword ptr [rbp + -1736]
	mov r9, qword ptr [rbp + -1728]
	xor r8, r9
	mov qword ptr [rbp + -1736], r8
	mov r8, qword ptr [rbp + -1736]
	mov r9, qword ptr [rbp + -1736]
	test r8, r9
	jnz l95
	l96:
	mov r8, 8
	mov qword ptr [rbp + -1744], r8
	mov r9, qword ptr [rbp + -1696]
	mov r8, r9
	mov qword ptr [rbp + -1752], r8
	mov r8, qword ptr [rbp + -1752]
	mov r9, qword ptr [rbp + -1744]
	imul r8, r9
	mov qword ptr [rbp + -1752], r8
	mov r9, qword ptr [rbp + -1688]
	mov r10, qword ptr [rbp + -1752]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1760], r8
	mov r9, qword ptr [rbp + -1760]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1760], r8
	mov r8, -1
	mov qword ptr [rbp + -1768], r8
	mov r8, qword ptr [rbp + -1760]
	mov r9, qword ptr [rbp + -1768]
	cmp r8, r9
	je l90
	l91:
	mov r9, qword ptr [rbp + -1104]
	mov r8, r9
	mov qword ptr [rbp + -1776], r8
	mov r9, qword ptr [rbp + -1560]
	mov r8, r9
	mov qword ptr [rbp + -1784], r8
	mov r8, 8
	mov qword ptr [rbp + -1792], r8
	mov r9, qword ptr [rbp + -1776]
	mov r8, r9
	mov qword ptr [rbp + -1800], r8
	mov r8, qword ptr [rbp + -1800]
	mov r9, qword ptr [rbp + -1792]
	sub r8, r9
	mov qword ptr [rbp + -1800], r8
	mov r9, qword ptr [rbp + -1800]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1800], r8
	mov r8, qword ptr [rbp + -1784]
	mov r9, qword ptr [rbp + -1800]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1808], r8
	mov r8, 1
	mov qword ptr [rbp + -1816], r8
	mov r9, qword ptr [rbp + -1808]
	mov r8, r9
	mov qword ptr [rbp + -1824], r8
	mov r8, qword ptr [rbp + -1824]
	mov r9, qword ptr [rbp + -1816]
	xor r8, r9
	mov qword ptr [rbp + -1824], r8
	mov r8, qword ptr [rbp + -1824]
	mov r9, qword ptr [rbp + -1824]
	test r8, r9
	jnz l97
	l98:
	mov r8, 8
	mov qword ptr [rbp + -1832], r8
	mov r9, qword ptr [rbp + -1784]
	mov r8, r9
	mov qword ptr [rbp + -1840], r8
	mov r8, qword ptr [rbp + -1840]
	mov r9, qword ptr [rbp + -1832]
	imul r8, r9
	mov qword ptr [rbp + -1840], r8
	mov r9, qword ptr [rbp + -1776]
	mov r10, qword ptr [rbp + -1840]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1848], r8
	mov r9, qword ptr [rbp + -1848]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1848], r8
	mov r9, qword ptr [rbp + -1848]
	mov r8, r9
	mov qword ptr [rbp + -1856], r8
	mov r9, qword ptr [rbp + -1600]
	mov r8, r9
	mov qword ptr [rbp + -1864], r8
	mov r8, 8
	mov qword ptr [rbp + -1872], r8
	mov r9, qword ptr [rbp + -1856]
	mov r8, r9
	mov qword ptr [rbp + -1880], r8
	mov r8, qword ptr [rbp + -1880]
	mov r9, qword ptr [rbp + -1872]
	sub r8, r9
	mov qword ptr [rbp + -1880], r8
	mov r9, qword ptr [rbp + -1880]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1880], r8
	mov r8, qword ptr [rbp + -1864]
	mov r9, qword ptr [rbp + -1880]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -1888], r8
	mov r8, 1
	mov qword ptr [rbp + -1896], r8
	mov r9, qword ptr [rbp + -1888]
	mov r8, r9
	mov qword ptr [rbp + -1904], r8
	mov r8, qword ptr [rbp + -1904]
	mov r9, qword ptr [rbp + -1896]
	xor r8, r9
	mov qword ptr [rbp + -1904], r8
	mov r8, qword ptr [rbp + -1904]
	mov r9, qword ptr [rbp + -1904]
	test r8, r9
	jnz l99
	l100:
	mov r8, 8
	mov qword ptr [rbp + -1912], r8
	mov r9, qword ptr [rbp + -1864]
	mov r8, r9
	mov qword ptr [rbp + -1920], r8
	mov r8, qword ptr [rbp + -1920]
	mov r9, qword ptr [rbp + -1912]
	imul r8, r9
	mov qword ptr [rbp + -1920], r8
	mov r9, qword ptr [rbp + -1856]
	mov r10, qword ptr [rbp + -1920]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1928], r8
	mov r9, qword ptr [rbp + -1928]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -1928], r8
	mov r9, qword ptr [rbp + -1496]
	mov r10, qword ptr [rbp + -1928]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -1936], r8
	mov r9, qword ptr [rbp + -1936]
	mov r8, r9
	mov qword ptr [rbp + -1944], r8
	mov r8, 26
	mov qword ptr [rbp + -1952], r8
	mov r8, qword ptr [rbp + -1944]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1952]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1944], r8
	mov r9, qword ptr [rbp + -1944]
	mov r8, r9
	mov qword ptr [rbp + -1216], r8
	mov r8, 26
	mov qword ptr [rbp + -1960], r8
	mov r8, qword ptr [rbp + -1944]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1960]
	idiv r8
	mov r8, rax
	mov qword ptr [rbp + -1944], r8
	mov r8, 26
	mov qword ptr [rbp + -1968], r8
	mov r8, qword ptr [rbp + -1944]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1968]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1944], r8
	mov r9, qword ptr [rbp + -1944]
	mov r8, r9
	mov qword ptr [rbp + -1312], r8
	mov r8, 676
	mov qword ptr [rbp + -1976], r8
	mov r8, qword ptr [rbp + -1944]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1976]
	idiv r8
	mov r8, rax
	mov qword ptr [rbp + -1944], r8
	mov r8, 26
	mov qword ptr [rbp + -1984], r8
	mov r8, qword ptr [rbp + -1944]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -1984]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1944], r8
	mov r9, qword ptr [rbp + -1944]
	mov r8, r9
	mov qword ptr [rbp + -1408], r8
	mov r9, qword ptr [rbp + -1184]
	mov r8, r9
	mov qword ptr [rbp + -1992], r8
	mov r9, qword ptr [rbp + -1200]
	mov r8, r9
	mov qword ptr [rbp + -2000], r8
	mov r9, qword ptr [rbp + -1216]
	mov r8, r9
	mov qword ptr [rbp + -2008], r8
	mov r9, qword ptr [rbp + -1584]
	mov r8, r9
	mov qword ptr [rbp + -2016], r8
	mov r8, qword ptr [rbp + -2016]
	mov rdi, r8
	mov r8, qword ptr [rbp + -2008]
	mov rsi, r8
	mov r8, qword ptr [rbp + -2000]
	mov rdx, r8
	mov r8, qword ptr [rbp + -1992]
	mov rcx, r8
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2024], r8
	mov r9, qword ptr [rbp + -2024]
	mov r8, r9
	mov qword ptr [rbp + -2032], r8
	mov r9, qword ptr [rbp + -2032]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r9, qword ptr [rbp + -1280]
	mov r8, r9
	mov qword ptr [rbp + -2040], r8
	mov r9, qword ptr [rbp + -1296]
	mov r8, r9
	mov qword ptr [rbp + -2048], r8
	mov r9, qword ptr [rbp + -1312]
	mov r8, r9
	mov qword ptr [rbp + -2056], r8
	mov r9, qword ptr [rbp + -1584]
	mov r8, r9
	mov qword ptr [rbp + -2064], r8
	mov r8, qword ptr [rbp + -2064]
	mov rdi, r8
	mov r8, qword ptr [rbp + -2056]
	mov rsi, r8
	mov r8, qword ptr [rbp + -2048]
	mov rdx, r8
	mov r8, qword ptr [rbp + -2040]
	mov rcx, r8
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2072], r8
	mov r9, qword ptr [rbp + -2072]
	mov r8, r9
	mov qword ptr [rbp + -2080], r8
	mov r9, qword ptr [rbp + -2080]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r9, qword ptr [rbp + -1376]
	mov r8, r9
	mov qword ptr [rbp + -2088], r8
	mov r9, qword ptr [rbp + -1392]
	mov r8, r9
	mov qword ptr [rbp + -2096], r8
	mov r9, qword ptr [rbp + -1408]
	mov r8, r9
	mov qword ptr [rbp + -2104], r8
	mov r9, qword ptr [rbp + -1584]
	mov r8, r9
	mov qword ptr [rbp + -2112], r8
	mov r8, qword ptr [rbp + -2112]
	mov rdi, r8
	mov r8, qword ptr [rbp + -2104]
	mov rsi, r8
	mov r8, qword ptr [rbp + -2096]
	mov rdx, r8
	mov r8, qword ptr [rbp + -2088]
	mov rcx, r8
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2120], r8
	mov r9, qword ptr [rbp + -2120]
	mov r8, r9
	mov qword ptr [rbp + -2128], r8
	mov r9, qword ptr [rbp + -2128]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r9, qword ptr [rbp + -1480]
	mov r8, r9
	mov qword ptr [rbp + -2136], r8
	mov r9, qword ptr [rbp + -1584]
	mov r8, r9
	mov qword ptr [rbp + -2144], r8
	mov r8, qword ptr [rbp + -2144]
	mov rdi, r8
	mov r8, qword ptr [rbp + -2136]
	mov rsi, r8
	and rsp, -16
	call _IreflectorEncrypt_iaii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2152], r8
	mov r9, qword ptr [rbp + -2152]
	mov r8, r9
	mov qword ptr [rbp + -2160], r8
	mov r9, qword ptr [rbp + -2160]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r9, qword ptr [rbp + -1376]
	mov r8, r9
	mov qword ptr [rbp + -2168], r8
	mov r9, qword ptr [rbp + -1392]
	mov r8, r9
	mov qword ptr [rbp + -2176], r8
	mov r9, qword ptr [rbp + -1408]
	mov r8, r9
	mov qword ptr [rbp + -2184], r8
	mov r9, qword ptr [rbp + -1584]
	mov r8, r9
	mov qword ptr [rbp + -2192], r8
	mov r8, qword ptr [rbp + -2192]
	mov rdi, r8
	mov r8, qword ptr [rbp + -2184]
	mov rsi, r8
	mov r8, qword ptr [rbp + -2176]
	mov rdx, r8
	mov r8, qword ptr [rbp + -2168]
	mov rcx, r8
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2200], r8
	mov r9, qword ptr [rbp + -2200]
	mov r8, r9
	mov qword ptr [rbp + -2208], r8
	mov r9, qword ptr [rbp + -2208]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r9, qword ptr [rbp + -1280]
	mov r8, r9
	mov qword ptr [rbp + -2216], r8
	mov r9, qword ptr [rbp + -1296]
	mov r8, r9
	mov qword ptr [rbp + -2224], r8
	mov r9, qword ptr [rbp + -1312]
	mov r8, r9
	mov qword ptr [rbp + -2232], r8
	mov r9, qword ptr [rbp + -1584]
	mov r8, r9
	mov qword ptr [rbp + -2240], r8
	mov r8, qword ptr [rbp + -2240]
	mov rdi, r8
	mov r8, qword ptr [rbp + -2232]
	mov rsi, r8
	mov r8, qword ptr [rbp + -2224]
	mov rdx, r8
	mov r8, qword ptr [rbp + -2216]
	mov rcx, r8
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2248], r8
	mov r9, qword ptr [rbp + -2248]
	mov r8, r9
	mov qword ptr [rbp + -2256], r8
	mov r9, qword ptr [rbp + -2256]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r9, qword ptr [rbp + -1184]
	mov r8, r9
	mov qword ptr [rbp + -2264], r8
	mov r9, qword ptr [rbp + -1200]
	mov r8, r9
	mov qword ptr [rbp + -2272], r8
	mov r9, qword ptr [rbp + -1216]
	mov r8, r9
	mov qword ptr [rbp + -2280], r8
	mov r9, qword ptr [rbp + -1584]
	mov r8, r9
	mov qword ptr [rbp + -2288], r8
	mov r8, qword ptr [rbp + -2288]
	mov rdi, r8
	mov r8, qword ptr [rbp + -2280]
	mov rsi, r8
	mov r8, qword ptr [rbp + -2272]
	mov rdx, r8
	mov r8, qword ptr [rbp + -2264]
	mov rcx, r8
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2296], r8
	mov r9, qword ptr [rbp + -2296]
	mov r8, r9
	mov qword ptr [rbp + -2304], r8
	mov r9, qword ptr [rbp + -2304]
	mov r8, r9
	mov qword ptr [rbp + -1584], r8
	mov r8, 1
	mov qword ptr [rbp + -2312], r8
	mov r9, qword ptr [rbp + -1600]
	mov r10, qword ptr [rbp + -2312]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2320], r8
	mov r9, qword ptr [rbp + -2320]
	mov r8, r9
	mov qword ptr [rbp + -1600], r8
	jmp l92
	l99:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l100
	l97:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l98
	l90:
	mov r8, qword ptr [rbp + -1584]
	mov r9, qword ptr [rbp + -1520]
	cmp r8, r9
	je l101
	l102:
	mov r8, 0
	mov qword ptr [rbp + -2328], r8
	mov r9, qword ptr [rbp + -2328]
	mov r8, r9
	mov qword ptr [rbp + -1544], r8
	l101:
	mov r8, 1
	mov qword ptr [rbp + -2336], r8
	mov r9, qword ptr [rbp + -1560]
	mov r10, qword ptr [rbp + -2336]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2344], r8
	mov r9, qword ptr [rbp + -2344]
	mov r8, r9
	mov qword ptr [rbp + -1560], r8
	jmp l89
	l95:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l96
	l93:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l94
	l87:
	mov r8, 1
	mov qword ptr [rbp + -2352], r8
	mov r9, qword ptr [rbp + -1544]
	mov r8, r9
	mov qword ptr [rbp + -2360], r8
	mov r8, qword ptr [rbp + -2360]
	mov r9, qword ptr [rbp + -2352]
	xor r8, r9
	mov qword ptr [rbp + -2360], r8
	mov r8, qword ptr [rbp + -2360]
	mov r9, qword ptr [rbp + -2360]
	test r8, r9
	jnz l103
	l104:
	mov r8, 32
	mov qword ptr [rbp + -2368], r8
	mov r9, qword ptr [rbp + -2368]
	mov r8, r9
	mov qword ptr [rbp + -2376], r8
	mov r8, qword ptr [rbp + -2376]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2384], r8
	mov r9, qword ptr [rbp + -2384]
	mov r8, r9
	mov qword ptr [rbp + -2392], r8
	mov r9, qword ptr [rbp + -2392]
	mov r8, r9
	mov qword ptr [rbp + -2400], r8
	mov r8, 3
	mov qword ptr [rbp + -2408], r8
	mov r8, qword ptr [rbp + -2400]
	mov r9, qword ptr [rbp + -2408]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -2416], r8
	mov r9, qword ptr [rbp + -2400]
	mov r10, qword ptr [rbp + -2416]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2424], r8
	mov r9, qword ptr [rbp + -2424]
	mov r8, r9
	mov qword ptr [rbp + -2432], r8
	mov r9, qword ptr [rbp + -2432]
	mov r8, r9
	mov qword ptr [rbp + -2440], r8
	mov r8, 0
	mov qword ptr [rbp + -2448], r8
	mov r9, qword ptr [rbp + -2448]
	mov r8, r9
	mov qword ptr [rbp + -2456], r8
	l107:
	mov r8, 3
	mov qword ptr [rbp + -2464], r8
	mov r8, qword ptr [rbp + -2456]
	mov r9, qword ptr [rbp + -2464]
	cmp r8, r9
	jge l105
	l106:
	mov r8, 8
	mov qword ptr [rbp + -2472], r8
	mov r9, qword ptr [rbp + -2456]
	mov r8, r9
	mov qword ptr [rbp + -2480], r8
	mov r8, qword ptr [rbp + -2480]
	mov r9, qword ptr [rbp + -2472]
	imul r8, r9
	mov qword ptr [rbp + -2480], r8
	mov r9, qword ptr [rbp + -2432]
	mov r10, qword ptr [rbp + -2480]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2488], r8
	mov r8, qword ptr [rbp + -2488]
	mov r9, qword ptr [rbp + -2496]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2504], r8
	mov r9, qword ptr [rbp + -2456]
	mov r10, qword ptr [rbp + -2504]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2512], r8
	mov r9, qword ptr [rbp + -2512]
	mov r8, r9
	mov qword ptr [rbp + -2456], r8
	jmp l107
	l105:
	mov r9, qword ptr [rbp + -2440]
	mov r8, r9
	mov qword ptr [rbp + -2520], r8
	mov r8, 0
	mov qword ptr [rbp + -2528], r8
	mov r9, qword ptr [rbp + -2528]
	mov r8, r9
	mov qword ptr [rbp + -2536], r8
	mov r8, 8
	mov qword ptr [rbp + -2544], r8
	mov r9, qword ptr [rbp + -2520]
	mov r8, r9
	mov qword ptr [rbp + -2552], r8
	mov r8, qword ptr [rbp + -2552]
	mov r9, qword ptr [rbp + -2544]
	sub r8, r9
	mov qword ptr [rbp + -2552], r8
	mov r9, qword ptr [rbp + -2552]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -2552], r8
	mov r8, qword ptr [rbp + -2536]
	mov r9, qword ptr [rbp + -2552]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -2560], r8
	mov r8, 1
	mov qword ptr [rbp + -2568], r8
	mov r9, qword ptr [rbp + -2560]
	mov r8, r9
	mov qword ptr [rbp + -2576], r8
	mov r8, qword ptr [rbp + -2576]
	mov r9, qword ptr [rbp + -2568]
	xor r8, r9
	mov qword ptr [rbp + -2576], r8
	mov r8, qword ptr [rbp + -2576]
	mov r9, qword ptr [rbp + -2576]
	test r8, r9
	jnz l108
	l109:
	mov r8, 8
	mov qword ptr [rbp + -2584], r8
	mov r9, qword ptr [rbp + -2536]
	mov r8, r9
	mov qword ptr [rbp + -2592], r8
	mov r8, qword ptr [rbp + -2592]
	mov r9, qword ptr [rbp + -2584]
	imul r8, r9
	mov qword ptr [rbp + -2592], r8
	mov r9, qword ptr [rbp + -2520]
	mov r10, qword ptr [rbp + -2592]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2600], r8
	mov r8, 26
	mov qword ptr [rbp + -2608], r8
	mov r8, qword ptr [rbp + -1496]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -2608]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1496], r8
	mov r8, 65
	mov qword ptr [rbp + -2616], r8
	mov r9, qword ptr [rbp + -1496]
	mov r10, qword ptr [rbp + -2616]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2624], r8
	mov r8, qword ptr [rbp + -2600]
	mov r9, qword ptr [rbp + -2624]
	mov qword ptr [r8], r9
	mov r9, qword ptr [rbp + -2440]
	mov r8, r9
	mov qword ptr [rbp + -2632], r8
	mov r8, 1
	mov qword ptr [rbp + -2640], r8
	mov r9, qword ptr [rbp + -2640]
	mov r8, r9
	mov qword ptr [rbp + -2648], r8
	mov r8, 8
	mov qword ptr [rbp + -2656], r8
	mov r9, qword ptr [rbp + -2632]
	mov r8, r9
	mov qword ptr [rbp + -2664], r8
	mov r8, qword ptr [rbp + -2664]
	mov r9, qword ptr [rbp + -2656]
	sub r8, r9
	mov qword ptr [rbp + -2664], r8
	mov r9, qword ptr [rbp + -2664]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -2664], r8
	mov r8, qword ptr [rbp + -2648]
	mov r9, qword ptr [rbp + -2664]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -2672], r8
	mov r8, 1
	mov qword ptr [rbp + -2680], r8
	mov r9, qword ptr [rbp + -2672]
	mov r8, r9
	mov qword ptr [rbp + -2688], r8
	mov r8, qword ptr [rbp + -2688]
	mov r9, qword ptr [rbp + -2680]
	xor r8, r9
	mov qword ptr [rbp + -2688], r8
	mov r8, qword ptr [rbp + -2688]
	mov r9, qword ptr [rbp + -2688]
	test r8, r9
	jnz l110
	l111:
	mov r8, 8
	mov qword ptr [rbp + -2696], r8
	mov r9, qword ptr [rbp + -2648]
	mov r8, r9
	mov qword ptr [rbp + -2704], r8
	mov r8, qword ptr [rbp + -2704]
	mov r9, qword ptr [rbp + -2696]
	imul r8, r9
	mov qword ptr [rbp + -2704], r8
	mov r9, qword ptr [rbp + -2632]
	mov r10, qword ptr [rbp + -2704]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2712], r8
	mov r8, 26
	mov qword ptr [rbp + -2720], r8
	mov r8, qword ptr [rbp + -1496]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -2720]
	idiv r8
	mov r8, rax
	mov qword ptr [rbp + -1496], r8
	mov r8, 26
	mov qword ptr [rbp + -2728], r8
	mov r8, qword ptr [rbp + -1496]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -2728]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1496], r8
	mov r8, 65
	mov qword ptr [rbp + -2736], r8
	mov r9, qword ptr [rbp + -1496]
	mov r10, qword ptr [rbp + -2736]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2744], r8
	mov r8, qword ptr [rbp + -2712]
	mov r9, qword ptr [rbp + -2744]
	mov qword ptr [r8], r9
	mov r9, qword ptr [rbp + -2440]
	mov r8, r9
	mov qword ptr [rbp + -2752], r8
	mov r8, 2
	mov qword ptr [rbp + -2760], r8
	mov r9, qword ptr [rbp + -2760]
	mov r8, r9
	mov qword ptr [rbp + -2768], r8
	mov r8, 8
	mov qword ptr [rbp + -2776], r8
	mov r9, qword ptr [rbp + -2752]
	mov r8, r9
	mov qword ptr [rbp + -2784], r8
	mov r8, qword ptr [rbp + -2784]
	mov r9, qword ptr [rbp + -2776]
	sub r8, r9
	mov qword ptr [rbp + -2784], r8
	mov r9, qword ptr [rbp + -2784]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -2784], r8
	mov r8, qword ptr [rbp + -2768]
	mov r9, qword ptr [rbp + -2784]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -2792], r8
	mov r8, 1
	mov qword ptr [rbp + -2800], r8
	mov r9, qword ptr [rbp + -2792]
	mov r8, r9
	mov qword ptr [rbp + -2808], r8
	mov r8, qword ptr [rbp + -2808]
	mov r9, qword ptr [rbp + -2800]
	xor r8, r9
	mov qword ptr [rbp + -2808], r8
	mov r8, qword ptr [rbp + -2808]
	mov r9, qword ptr [rbp + -2808]
	test r8, r9
	jnz l112
	l113:
	mov r8, 8
	mov qword ptr [rbp + -2816], r8
	mov r9, qword ptr [rbp + -2768]
	mov r8, r9
	mov qword ptr [rbp + -2824], r8
	mov r8, qword ptr [rbp + -2824]
	mov r9, qword ptr [rbp + -2816]
	imul r8, r9
	mov qword ptr [rbp + -2824], r8
	mov r9, qword ptr [rbp + -2752]
	mov r10, qword ptr [rbp + -2824]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2832], r8
	mov r8, 676
	mov qword ptr [rbp + -2840], r8
	mov r8, qword ptr [rbp + -1496]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -2840]
	idiv r8
	mov r8, rax
	mov qword ptr [rbp + -1496], r8
	mov r8, 26
	mov qword ptr [rbp + -2848], r8
	mov r8, qword ptr [rbp + -1496]
	mov rax, r8
	xor rdx, rdx
	mov r8, qword ptr [rbp + -2848]
	idiv r8
	mov r8, rdx
	mov qword ptr [rbp + -1496], r8
	mov r8, 65
	mov qword ptr [rbp + -2856], r8
	mov r9, qword ptr [rbp + -1496]
	mov r10, qword ptr [rbp + -2856]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2864], r8
	mov r8, qword ptr [rbp + -2832]
	mov r9, qword ptr [rbp + -2864]
	mov qword ptr [r8], r9
	lea r8, qword ptr g4[rip]
	mov qword ptr [rbp + -2872], r8
	mov r9, qword ptr [rbp + -2872]
	mov r8, r9
	mov qword ptr [rbp + -2880], r8
	mov r8, 8
	mov qword ptr [rbp + -2888], r8
	mov r9, qword ptr [rbp + -2880]
	mov r10, qword ptr [rbp + -2888]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2896], r8
	mov r9, qword ptr [rbp + -2896]
	mov r8, r9
	mov qword ptr [rbp + -2904], r8
	mov r9, qword ptr [rbp + -2904]
	mov r8, r9
	mov qword ptr [rbp + -2912], r8
	mov r8, qword ptr [rbp + -2912]
	mov rdi, r8
	and rsp, -16
	call _Iprint_pai
	mov r9, qword ptr [rbp + -2440]
	mov r8, r9
	mov qword ptr [rbp + -2920], r8
	mov r8, qword ptr [rbp + -2920]
	mov rdi, r8
	and rsp, -16
	call _Iprint_pai
	lea r8, qword ptr g5[rip]
	mov qword ptr [rbp + -2928], r8
	mov r9, qword ptr [rbp + -2928]
	mov r8, r9
	mov qword ptr [rbp + -2936], r8
	mov r8, 8
	mov qword ptr [rbp + -2944], r8
	mov r9, qword ptr [rbp + -2936]
	mov r10, qword ptr [rbp + -2944]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -2952], r8
	mov r9, qword ptr [rbp + -2952]
	mov r8, r9
	mov qword ptr [rbp + -2960], r8
	mov r9, qword ptr [rbp + -2960]
	mov r8, r9
	mov qword ptr [rbp + -2968], r8
	mov r8, qword ptr [rbp + -2968]
	mov rdi, r8
	and rsp, -16
	call _Iprint_pai
	mov r8, 16
	mov qword ptr [rbp + -2976], r8
	mov r9, qword ptr [rbp + -2976]
	mov r8, r9
	mov qword ptr [rbp + -2984], r8
	mov r8, qword ptr [rbp + -2984]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -48], r8
	mov r9, qword ptr [rbp + -48]
	mov r8, r9
	mov qword ptr [rbp + -2992], r8
	mov r9, qword ptr [rbp + -2992]
	mov r8, r9
	mov qword ptr [rbp + -3000], r8
	mov r9, qword ptr [rbp + -3000]
	mov r8, r9
	mov qword ptr [rbp + -3008], r8
	mov r8, 1
	mov qword ptr [rbp + -3016], r8
	mov r8, qword ptr [rbp + -3008]
	mov r9, qword ptr [rbp + -3016]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -3024], r8
	mov r9, qword ptr [rbp + -3008]
	mov r10, qword ptr [rbp + -3024]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -3032], r8
	mov r9, qword ptr [rbp + -3032]
	mov r8, r9
	mov qword ptr [rbp + -3040], r8
	mov r9, qword ptr [rbp + -3040]
	mov r8, r9
	mov qword ptr [rbp + -3048], r8
	mov r8, 0
	mov qword ptr [rbp + -3056], r8
	mov r9, qword ptr [rbp + -3056]
	mov r8, r9
	mov qword ptr [rbp + -3064], r8
	l116:
	mov r8, 1
	mov qword ptr [rbp + -3072], r8
	mov r8, qword ptr [rbp + -3064]
	mov r9, qword ptr [rbp + -3072]
	cmp r8, r9
	jge l114
	l115:
	mov r8, 8
	mov qword ptr [rbp + -3080], r8
	mov r9, qword ptr [rbp + -3064]
	mov r8, r9
	mov qword ptr [rbp + -3088], r8
	mov r8, qword ptr [rbp + -3088]
	mov r9, qword ptr [rbp + -3080]
	imul r8, r9
	mov qword ptr [rbp + -3088], r8
	mov r9, qword ptr [rbp + -3040]
	mov r10, qword ptr [rbp + -3088]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -3096], r8
	mov r8, qword ptr [rbp + -3096]
	mov r9, qword ptr [rbp + -3104]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3112], r8
	mov r9, qword ptr [rbp + -3064]
	mov r10, qword ptr [rbp + -3112]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -3120], r8
	mov r9, qword ptr [rbp + -3120]
	mov r8, r9
	mov qword ptr [rbp + -3064], r8
	jmp l116
	l114:
	mov r9, qword ptr [rbp + -3048]
	mov r8, r9
	mov qword ptr [rbp + -3128], r8
	mov r8, 0
	mov qword ptr [rbp + -3136], r8
	mov r9, qword ptr [rbp + -3136]
	mov r8, r9
	mov qword ptr [rbp + -3144], r8
	mov r8, 8
	mov qword ptr [rbp + -3152], r8
	mov r9, qword ptr [rbp + -3128]
	mov r8, r9
	mov qword ptr [rbp + -3160], r8
	mov r8, qword ptr [rbp + -3160]
	mov r9, qword ptr [rbp + -3152]
	sub r8, r9
	mov qword ptr [rbp + -3160], r8
	mov r9, qword ptr [rbp + -3160]
	mov r8, qword ptr [r9]
	mov qword ptr [rbp + -3160], r8
	mov r8, qword ptr [rbp + -3144]
	mov r9, qword ptr [rbp + -3160]
	cmp r8, r9
	setb r8b
	movzx r8, r8b
	mov qword ptr [rbp + -3168], r8
	mov r8, 1
	mov qword ptr [rbp + -3176], r8
	mov r9, qword ptr [rbp + -3168]
	mov r8, r9
	mov qword ptr [rbp + -3184], r8
	mov r8, qword ptr [rbp + -3184]
	mov r9, qword ptr [rbp + -3176]
	xor r8, r9
	mov qword ptr [rbp + -3184], r8
	mov r8, qword ptr [rbp + -3184]
	mov r9, qword ptr [rbp + -3184]
	test r8, r9
	jnz l117
	l118:
	mov r8, 8
	mov qword ptr [rbp + -3192], r8
	mov r9, qword ptr [rbp + -3144]
	mov r8, r9
	mov qword ptr [rbp + -3200], r8
	mov r8, qword ptr [rbp + -3200]
	mov r9, qword ptr [rbp + -3192]
	imul r8, r9
	mov qword ptr [rbp + -3200], r8
	mov r9, qword ptr [rbp + -3128]
	mov r10, qword ptr [rbp + -3200]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -3208], r8
	mov r8, 65
	mov qword ptr [rbp + -3216], r8
	mov r9, qword ptr [rbp + -1520]
	mov r10, qword ptr [rbp + -3216]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -3224], r8
	mov r8, qword ptr [rbp + -3208]
	mov r9, qword ptr [rbp + -3224]
	mov qword ptr [r8], r9
	mov r9, qword ptr [rbp + -3048]
	mov r8, r9
	mov qword ptr [rbp + -3232], r8
	mov r8, qword ptr [rbp + -3232]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	l103:
	mov r8, 1
	mov qword ptr [rbp + -3240], r8
	mov r9, qword ptr [rbp + -1520]
	mov r10, qword ptr [rbp + -3240]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -3248], r8
	mov r9, qword ptr [rbp + -3248]
	mov r8, r9
	mov qword ptr [rbp + -1520], r8
	jmp l86
	l117:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l118
	l112:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l113
	l110:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l111
	l108:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l109
	l84:
	mov r8, 1
	mov qword ptr [rbp + -3256], r8
	mov r9, qword ptr [rbp + -1496]
	mov r10, qword ptr [rbp + -3256]
	lea r8, qword ptr [r9 + r10]
	mov qword ptr [rbp + -3264], r8
	mov r9, qword ptr [rbp + -3264]
	mov r8, r9
	mov qword ptr [rbp + -1496], r8
	jmp l83
	l81:
	leave
	ret
