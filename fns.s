.intel_syntax noprefix
.data
_g7: .quad 2, 116, 32
_g6: .quad 2, 115, 32
_g5: .quad 2, 114, 32
_g4: .quad 2, 113, 32
_g3: .quad 2, 112, 32
_g2: .quad 2, 111, 32
_g0: .quad 2, 110, 32
_g1: .quad 2, 109, 32
_g15: .quad 2, 104, 32
_g14: .quad 2, 103, 32
_g13: .quad 2, 102, 32
_g12: .quad 2, 101, 32
_g11: .quad 2, 100, 32
_g10: .quad 2, 99, 32
_g9: .quad 2, 98, 32
_g8: .quad 2, 97, 32
.globl _Imain_paai, _Ifoo_t8iiiiiiiiiiiiiiii
.text
_Imain_paai:
	enter 3928, 0
	mov r8, rdi
	mov qword ptr [rbp + -8], r8
	mov r8, qword ptr [rbp + -8]
	mov r9, r8
	mov qword ptr [rbp + -16], r9
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -24], r9
	mov r8, 32
	mov qword ptr [rbp + -32], r8
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -40], r9
	mov r8, 34
	mov qword ptr [rbp + -48], r8
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -56], r9
	mov r8, 13
	mov qword ptr [rbp + -64], r8
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -72], r9
	mov r8, 14
	mov qword ptr [rbp + -80], r8
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, 15
	mov qword ptr [rbp + -96], r8
	mov r8, qword ptr [rbp + -96]
	mov r9, r8
	mov qword ptr [rbp + -104], r9
	mov r8, 16
	mov qword ptr [rbp + -112], r8
	mov r8, qword ptr [rbp + -112]
	mov r9, r8
	mov qword ptr [rbp + -120], r9
	mov r8, 17
	mov qword ptr [rbp + -128], r8
	mov r8, qword ptr [rbp + -128]
	mov r9, r8
	mov qword ptr [rbp + -136], r9
	mov r8, 38
	mov qword ptr [rbp + -144], r8
	mov r8, qword ptr [rbp + -144]
	mov r9, r8
	mov qword ptr [rbp + -152], r9
	sub rsp, 48
	mov rdi, rsp
	mov r8, qword ptr [rbp + -152]
	push r8
	mov r8, qword ptr [rbp + -136]
	push r8
	mov r8, qword ptr [rbp + -120]
	push r8
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov r8, qword ptr [rbp + -88]
	mov r8, r8
	mov r8, qword ptr [rbp + -72]
	mov rcx, r8
	mov r8, qword ptr [rbp + -56]
	mov rdx, r8
	mov r8, qword ptr [rbp + -40]
	mov rsi, r8
	and rsp, -16
	call _Ifoo_t8iiiiiiiiiiiiiiii
	add rsp, 24
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, rdx
	mov qword ptr [rbp + -168], r8
	pop r8
	mov qword ptr [rbp + -176], r8
	pop r8
	mov qword ptr [rbp + -184], r8
	pop r8
	mov qword ptr [rbp + -192], r8
	pop r8
	mov qword ptr [rbp + -200], r8
	pop r8
	mov qword ptr [rbp + -208], r8
	pop r8
	mov qword ptr [rbp + -216], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -224], r9
	mov r8, qword ptr [rbp + -224]
	mov r9, r8
	mov qword ptr [rbp + -232], r9
	mov r8, qword ptr [rbp + -168]
	mov r9, r8
	mov qword ptr [rbp + -240], r9
	mov r8, qword ptr [rbp + -240]
	mov r9, r8
	mov qword ptr [rbp + -248], r9
	mov r8, qword ptr [rbp + -176]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	mov r8, qword ptr [rbp + -256]
	mov r9, r8
	mov qword ptr [rbp + -264], r9
	mov r8, qword ptr [rbp + -184]
	mov r9, r8
	mov qword ptr [rbp + -272], r9
	mov r8, qword ptr [rbp + -272]
	mov r9, r8
	mov qword ptr [rbp + -280], r9
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -288], r9
	mov r8, qword ptr [rbp + -288]
	mov r9, r8
	mov qword ptr [rbp + -296], r9
	mov r8, qword ptr [rbp + -200]
	mov r9, r8
	mov qword ptr [rbp + -304], r9
	mov r8, qword ptr [rbp + -304]
	mov r9, r8
	mov qword ptr [rbp + -312], r9
	mov r8, qword ptr [rbp + -208]
	mov r9, r8
	mov qword ptr [rbp + -320], r9
	mov r8, qword ptr [rbp + -320]
	mov r9, r8
	mov qword ptr [rbp + -328], r9
	mov r8, qword ptr [rbp + -216]
	mov r9, r8
	mov qword ptr [rbp + -336], r9
	mov r8, qword ptr [rbp + -336]
	mov r9, r8
	mov qword ptr [rbp + -344], r9
	lea r8, qword ptr _g0[rip]
	mov qword ptr [rbp + -352], r8
	mov r8, qword ptr [rbp + -352]
	mov r9, r8
	mov qword ptr [rbp + -360], r9
	mov r8, 8
	mov qword ptr [rbp + -368], r8
	mov r8, qword ptr [rbp + -360]
	mov r9, qword ptr [rbp + -368]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -376], r8
	mov r8, qword ptr [rbp + -376]
	mov r9, r8
	mov qword ptr [rbp + -384], r9
	mov r8, qword ptr [rbp + -384]
	mov r9, r8
	mov qword ptr [rbp + -392], r9
	mov r8, qword ptr [rbp + -232]
	mov r9, r8
	mov qword ptr [rbp + -400], r9
	mov r8, qword ptr [rbp + -400]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -408], r9
	mov r8, qword ptr [rbp + -408]
	mov r9, r8
	mov qword ptr [rbp + -416], r9
	mov r8, qword ptr [rbp + -416]
	mov r9, r8
	mov qword ptr [rbp + -424], r9
	mov r8, 8
	mov qword ptr [rbp + -432], r8
	mov r8, qword ptr [rbp + -392]
	mov r9, r8
	mov qword ptr [rbp + -440], r9
	mov r8, qword ptr [rbp + -440]
	mov r9, qword ptr [rbp + -432]
	sub r8, r9
	mov qword ptr [rbp + -440], r8
	mov r8, qword ptr [rbp + -440]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -440], r8
	mov r8, qword ptr [rbp + -440]
	mov r9, r8
	mov qword ptr [rbp + -448], r9
	mov r8, 8
	mov qword ptr [rbp + -456], r8
	mov r8, qword ptr [rbp + -424]
	mov r9, r8
	mov qword ptr [rbp + -464], r9
	mov r8, qword ptr [rbp + -464]
	mov r9, qword ptr [rbp + -456]
	sub r8, r9
	mov qword ptr [rbp + -464], r8
	mov r8, qword ptr [rbp + -464]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -464], r8
	mov r8, qword ptr [rbp + -464]
	mov r9, r8
	mov qword ptr [rbp + -472], r9
	mov r8, qword ptr [rbp + -448]
	mov r9, qword ptr [rbp + -472]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -480], r8
	mov r8, qword ptr [rbp + -480]
	mov r9, r8
	mov qword ptr [rbp + -488], r9
	mov r8, 8
	mov qword ptr [rbp + -496], r8
	mov r8, 8
	mov qword ptr [rbp + -504], r8
	mov r8, qword ptr [rbp + -488]
	mov r9, r8
	mov qword ptr [rbp + -512], r9
	mov r8, qword ptr [rbp + -512]
	mov r9, qword ptr [rbp + -504]
	imul r8, r9
	mov qword ptr [rbp + -512], r8
	mov r8, qword ptr [rbp + -496]
	mov r9, qword ptr [rbp + -512]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -520], r8
	mov r8, qword ptr [rbp + -520]
	mov r9, r8
	mov qword ptr [rbp + -528], r9
	mov r8, qword ptr [rbp + -528]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -536], r9
	mov r8, qword ptr [rbp + -536]
	mov r9, r8
	mov qword ptr [rbp + -544], r9
	mov r8, qword ptr [rbp + -544]
	mov r9, r8
	mov qword ptr [rbp + -552], r9
	mov r8, qword ptr [rbp + -552]
	mov r9, qword ptr [rbp + -488]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -560], r8
	mov r8, qword ptr [rbp + -552]
	mov r9, qword ptr [rbp + -560]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -568], r8
	mov r8, qword ptr [rbp + -568]
	mov r9, r8
	mov qword ptr [rbp + -576], r9
	mov r8, 0
	mov qword ptr [rbp + -584], r8
	mov r8, qword ptr [rbp + -584]
	mov r9, r8
	mov qword ptr [rbp + -592], r9
	_l5:
	mov r8, qword ptr [rbp + -592]
	mov r9, qword ptr [rbp + -448]
	cmp r8, r9
	jge _l3
	_l4:
	mov r8, 8
	mov qword ptr [rbp + -600], r8
	mov r8, qword ptr [rbp + -592]
	mov r9, r8
	mov qword ptr [rbp + -608], r9
	mov r8, qword ptr [rbp + -608]
	mov r9, qword ptr [rbp + -600]
	imul r8, r9
	mov qword ptr [rbp + -608], r8
	mov r8, qword ptr [rbp + -576]
	mov r9, qword ptr [rbp + -608]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -616], r8
	mov r8, qword ptr [rbp + -616]
	mov r9, r8
	mov qword ptr [rbp + -624], r9
	mov r8, 8
	mov qword ptr [rbp + -632], r8
	mov r8, qword ptr [rbp + -592]
	mov r9, r8
	mov qword ptr [rbp + -640], r9
	mov r8, qword ptr [rbp + -640]
	mov r9, qword ptr [rbp + -632]
	imul r8, r9
	mov qword ptr [rbp + -640], r8
	mov r8, qword ptr [rbp + -392]
	mov r9, qword ptr [rbp + -640]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -648], r8
	mov r8, qword ptr [rbp + -648]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -648], r8
	mov r8, qword ptr [rbp + -624]
	mov r9, qword ptr [rbp + -648]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -656], r8
	mov r8, qword ptr [rbp + -592]
	mov r9, qword ptr [rbp + -656]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -664], r8
	mov r8, qword ptr [rbp + -664]
	mov r9, r8
	mov qword ptr [rbp + -592], r9
	jmp _l5
	_l3:
	mov r8, 8
	mov qword ptr [rbp + -672], r8
	mov r8, qword ptr [rbp + -448]
	mov r9, r8
	mov qword ptr [rbp + -680], r9
	mov r8, qword ptr [rbp + -680]
	mov r9, qword ptr [rbp + -672]
	imul r8, r9
	mov qword ptr [rbp + -680], r8
	mov r8, qword ptr [rbp + -576]
	mov r9, qword ptr [rbp + -680]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -688], r8
	mov r8, qword ptr [rbp + -688]
	mov r9, r8
	mov qword ptr [rbp + -696], r9
	mov r8, 0
	mov qword ptr [rbp + -704], r8
	mov r8, qword ptr [rbp + -704]
	mov r9, r8
	mov qword ptr [rbp + -712], r9
	_l2:
	mov r8, qword ptr [rbp + -712]
	mov r9, qword ptr [rbp + -472]
	cmp r8, r9
	jge _l0
	_l1:
	mov r8, 8
	mov qword ptr [rbp + -720], r8
	mov r8, qword ptr [rbp + -712]
	mov r9, r8
	mov qword ptr [rbp + -728], r9
	mov r8, qword ptr [rbp + -728]
	mov r9, qword ptr [rbp + -720]
	imul r8, r9
	mov qword ptr [rbp + -728], r8
	mov r8, qword ptr [rbp + -696]
	mov r9, qword ptr [rbp + -728]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -736], r8
	mov r8, qword ptr [rbp + -736]
	mov r9, r8
	mov qword ptr [rbp + -744], r9
	mov r8, 8
	mov qword ptr [rbp + -752], r8
	mov r8, qword ptr [rbp + -712]
	mov r9, r8
	mov qword ptr [rbp + -760], r9
	mov r8, qword ptr [rbp + -760]
	mov r9, qword ptr [rbp + -752]
	imul r8, r9
	mov qword ptr [rbp + -760], r8
	mov r8, qword ptr [rbp + -424]
	mov r9, qword ptr [rbp + -760]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -768], r8
	mov r8, qword ptr [rbp + -768]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -768], r8
	mov r8, qword ptr [rbp + -744]
	mov r9, qword ptr [rbp + -768]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -776], r8
	mov r8, qword ptr [rbp + -712]
	mov r9, qword ptr [rbp + -776]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -784], r8
	mov r8, qword ptr [rbp + -784]
	mov r9, r8
	mov qword ptr [rbp + -712], r9
	jmp _l2
	_l0:
	mov r8, qword ptr [rbp + -576]
	mov r9, r8
	mov qword ptr [rbp + -792], r9
	mov r8, qword ptr [rbp + -792]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g1[rip]
	mov qword ptr [rbp + -800], r8
	mov r8, qword ptr [rbp + -800]
	mov r9, r8
	mov qword ptr [rbp + -808], r9
	mov r8, 8
	mov qword ptr [rbp + -816], r8
	mov r8, qword ptr [rbp + -808]
	mov r9, qword ptr [rbp + -816]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -824], r8
	mov r8, qword ptr [rbp + -824]
	mov r9, r8
	mov qword ptr [rbp + -832], r9
	mov r8, qword ptr [rbp + -832]
	mov r9, r8
	mov qword ptr [rbp + -840], r9
	mov r8, qword ptr [rbp + -248]
	mov r9, r8
	mov qword ptr [rbp + -848], r9
	mov r8, qword ptr [rbp + -848]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -856], r9
	mov r8, qword ptr [rbp + -856]
	mov r9, r8
	mov qword ptr [rbp + -864], r9
	mov r8, qword ptr [rbp + -864]
	mov r9, r8
	mov qword ptr [rbp + -872], r9
	mov r8, 8
	mov qword ptr [rbp + -880], r8
	mov r8, qword ptr [rbp + -840]
	mov r9, r8
	mov qword ptr [rbp + -888], r9
	mov r8, qword ptr [rbp + -888]
	mov r9, qword ptr [rbp + -880]
	sub r8, r9
	mov qword ptr [rbp + -888], r8
	mov r8, qword ptr [rbp + -888]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -888], r8
	mov r8, qword ptr [rbp + -888]
	mov r9, r8
	mov qword ptr [rbp + -896], r9
	mov r8, 8
	mov qword ptr [rbp + -904], r8
	mov r8, qword ptr [rbp + -872]
	mov r9, r8
	mov qword ptr [rbp + -912], r9
	mov r8, qword ptr [rbp + -912]
	mov r9, qword ptr [rbp + -904]
	sub r8, r9
	mov qword ptr [rbp + -912], r8
	mov r8, qword ptr [rbp + -912]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -912], r8
	mov r8, qword ptr [rbp + -912]
	mov r9, r8
	mov qword ptr [rbp + -920], r9
	mov r8, qword ptr [rbp + -896]
	mov r9, qword ptr [rbp + -920]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -928], r8
	mov r8, qword ptr [rbp + -928]
	mov r9, r8
	mov qword ptr [rbp + -936], r9
	mov r8, 8
	mov qword ptr [rbp + -944], r8
	mov r8, 8
	mov qword ptr [rbp + -952], r8
	mov r8, qword ptr [rbp + -936]
	mov r9, r8
	mov qword ptr [rbp + -960], r9
	mov r8, qword ptr [rbp + -960]
	mov r9, qword ptr [rbp + -952]
	imul r8, r9
	mov qword ptr [rbp + -960], r8
	mov r8, qword ptr [rbp + -944]
	mov r9, qword ptr [rbp + -960]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -968], r8
	mov r8, qword ptr [rbp + -968]
	mov r9, r8
	mov qword ptr [rbp + -976], r9
	mov r8, qword ptr [rbp + -976]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -984], r9
	mov r8, qword ptr [rbp + -984]
	mov r9, r8
	mov qword ptr [rbp + -992], r9
	mov r8, qword ptr [rbp + -992]
	mov r9, r8
	mov qword ptr [rbp + -1000], r9
	mov r8, qword ptr [rbp + -1000]
	mov r9, qword ptr [rbp + -936]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -1008], r8
	mov r8, qword ptr [rbp + -1000]
	mov r9, qword ptr [rbp + -1008]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1016], r8
	mov r8, qword ptr [rbp + -1016]
	mov r9, r8
	mov qword ptr [rbp + -1024], r9
	mov r8, 0
	mov qword ptr [rbp + -1032], r8
	mov r8, qword ptr [rbp + -1032]
	mov r9, r8
	mov qword ptr [rbp + -1040], r9
	_l11:
	mov r8, qword ptr [rbp + -1040]
	mov r9, qword ptr [rbp + -896]
	cmp r8, r9
	jge _l9
	_l10:
	mov r8, 8
	mov qword ptr [rbp + -1048], r8
	mov r8, qword ptr [rbp + -1040]
	mov r9, r8
	mov qword ptr [rbp + -1056], r9
	mov r8, qword ptr [rbp + -1056]
	mov r9, qword ptr [rbp + -1048]
	imul r8, r9
	mov qword ptr [rbp + -1056], r8
	mov r8, qword ptr [rbp + -1024]
	mov r9, qword ptr [rbp + -1056]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1064], r8
	mov r8, qword ptr [rbp + -1064]
	mov r9, r8
	mov qword ptr [rbp + -1072], r9
	mov r8, 8
	mov qword ptr [rbp + -1080], r8
	mov r8, qword ptr [rbp + -1040]
	mov r9, r8
	mov qword ptr [rbp + -1088], r9
	mov r8, qword ptr [rbp + -1088]
	mov r9, qword ptr [rbp + -1080]
	imul r8, r9
	mov qword ptr [rbp + -1088], r8
	mov r8, qword ptr [rbp + -840]
	mov r9, qword ptr [rbp + -1088]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1096], r8
	mov r8, qword ptr [rbp + -1096]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1096], r8
	mov r8, qword ptr [rbp + -1072]
	mov r9, qword ptr [rbp + -1096]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1104], r8
	mov r8, qword ptr [rbp + -1040]
	mov r9, qword ptr [rbp + -1104]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1112], r8
	mov r8, qword ptr [rbp + -1112]
	mov r9, r8
	mov qword ptr [rbp + -1040], r9
	jmp _l11
	_l9:
	mov r8, 8
	mov qword ptr [rbp + -1120], r8
	mov r8, qword ptr [rbp + -896]
	mov r9, r8
	mov qword ptr [rbp + -1128], r9
	mov r8, qword ptr [rbp + -1128]
	mov r9, qword ptr [rbp + -1120]
	imul r8, r9
	mov qword ptr [rbp + -1128], r8
	mov r8, qword ptr [rbp + -1024]
	mov r9, qword ptr [rbp + -1128]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1136], r8
	mov r8, qword ptr [rbp + -1136]
	mov r9, r8
	mov qword ptr [rbp + -1144], r9
	mov r8, 0
	mov qword ptr [rbp + -1152], r8
	mov r8, qword ptr [rbp + -1152]
	mov r9, r8
	mov qword ptr [rbp + -1160], r9
	_l8:
	mov r8, qword ptr [rbp + -1160]
	mov r9, qword ptr [rbp + -920]
	cmp r8, r9
	jge _l6
	_l7:
	mov r8, 8
	mov qword ptr [rbp + -1168], r8
	mov r8, qword ptr [rbp + -1160]
	mov r9, r8
	mov qword ptr [rbp + -1176], r9
	mov r8, qword ptr [rbp + -1176]
	mov r9, qword ptr [rbp + -1168]
	imul r8, r9
	mov qword ptr [rbp + -1176], r8
	mov r8, qword ptr [rbp + -1144]
	mov r9, qword ptr [rbp + -1176]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1184], r8
	mov r8, qword ptr [rbp + -1184]
	mov r9, r8
	mov qword ptr [rbp + -1192], r9
	mov r8, 8
	mov qword ptr [rbp + -1200], r8
	mov r8, qword ptr [rbp + -1160]
	mov r9, r8
	mov qword ptr [rbp + -1208], r9
	mov r8, qword ptr [rbp + -1208]
	mov r9, qword ptr [rbp + -1200]
	imul r8, r9
	mov qword ptr [rbp + -1208], r8
	mov r8, qword ptr [rbp + -872]
	mov r9, qword ptr [rbp + -1208]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1216], r8
	mov r8, qword ptr [rbp + -1216]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1216], r8
	mov r8, qword ptr [rbp + -1192]
	mov r9, qword ptr [rbp + -1216]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1224], r8
	mov r8, qword ptr [rbp + -1160]
	mov r9, qword ptr [rbp + -1224]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1232], r8
	mov r8, qword ptr [rbp + -1232]
	mov r9, r8
	mov qword ptr [rbp + -1160], r9
	jmp _l8
	_l6:
	mov r8, qword ptr [rbp + -1024]
	mov r9, r8
	mov qword ptr [rbp + -1240], r9
	mov r8, qword ptr [rbp + -1240]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g2[rip]
	mov qword ptr [rbp + -1248], r8
	mov r8, qword ptr [rbp + -1248]
	mov r9, r8
	mov qword ptr [rbp + -1256], r9
	mov r8, 8
	mov qword ptr [rbp + -1264], r8
	mov r8, qword ptr [rbp + -1256]
	mov r9, qword ptr [rbp + -1264]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1272], r8
	mov r8, qword ptr [rbp + -1272]
	mov r9, r8
	mov qword ptr [rbp + -1280], r9
	mov r8, qword ptr [rbp + -1280]
	mov r9, r8
	mov qword ptr [rbp + -1288], r9
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -1296], r9
	mov r8, qword ptr [rbp + -1296]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -1304], r9
	mov r8, qword ptr [rbp + -1304]
	mov r9, r8
	mov qword ptr [rbp + -1312], r9
	mov r8, qword ptr [rbp + -1312]
	mov r9, r8
	mov qword ptr [rbp + -1320], r9
	mov r8, 8
	mov qword ptr [rbp + -1328], r8
	mov r8, qword ptr [rbp + -1288]
	mov r9, r8
	mov qword ptr [rbp + -1336], r9
	mov r8, qword ptr [rbp + -1336]
	mov r9, qword ptr [rbp + -1328]
	sub r8, r9
	mov qword ptr [rbp + -1336], r8
	mov r8, qword ptr [rbp + -1336]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1336], r8
	mov r8, qword ptr [rbp + -1336]
	mov r9, r8
	mov qword ptr [rbp + -1344], r9
	mov r8, 8
	mov qword ptr [rbp + -1352], r8
	mov r8, qword ptr [rbp + -1320]
	mov r9, r8
	mov qword ptr [rbp + -1360], r9
	mov r8, qword ptr [rbp + -1360]
	mov r9, qword ptr [rbp + -1352]
	sub r8, r9
	mov qword ptr [rbp + -1360], r8
	mov r8, qword ptr [rbp + -1360]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1360], r8
	mov r8, qword ptr [rbp + -1360]
	mov r9, r8
	mov qword ptr [rbp + -1368], r9
	mov r8, qword ptr [rbp + -1344]
	mov r9, qword ptr [rbp + -1368]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1376], r8
	mov r8, qword ptr [rbp + -1376]
	mov r9, r8
	mov qword ptr [rbp + -1384], r9
	mov r8, 8
	mov qword ptr [rbp + -1392], r8
	mov r8, 8
	mov qword ptr [rbp + -1400], r8
	mov r8, qword ptr [rbp + -1384]
	mov r9, r8
	mov qword ptr [rbp + -1408], r9
	mov r8, qword ptr [rbp + -1408]
	mov r9, qword ptr [rbp + -1400]
	imul r8, r9
	mov qword ptr [rbp + -1408], r8
	mov r8, qword ptr [rbp + -1392]
	mov r9, qword ptr [rbp + -1408]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1416], r8
	mov r8, qword ptr [rbp + -1416]
	mov r9, r8
	mov qword ptr [rbp + -1424], r9
	mov r8, qword ptr [rbp + -1424]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -1432], r9
	mov r8, qword ptr [rbp + -1432]
	mov r9, r8
	mov qword ptr [rbp + -1440], r9
	mov r8, qword ptr [rbp + -1440]
	mov r9, r8
	mov qword ptr [rbp + -1448], r9
	mov r8, qword ptr [rbp + -1448]
	mov r9, qword ptr [rbp + -1384]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -1456], r8
	mov r8, qword ptr [rbp + -1448]
	mov r9, qword ptr [rbp + -1456]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1464], r8
	mov r8, qword ptr [rbp + -1464]
	mov r9, r8
	mov qword ptr [rbp + -1472], r9
	mov r8, 0
	mov qword ptr [rbp + -1480], r8
	mov r8, qword ptr [rbp + -1480]
	mov r9, r8
	mov qword ptr [rbp + -1488], r9
	_l17:
	mov r8, qword ptr [rbp + -1488]
	mov r9, qword ptr [rbp + -1344]
	cmp r8, r9
	jge _l15
	_l16:
	mov r8, 8
	mov qword ptr [rbp + -1496], r8
	mov r8, qword ptr [rbp + -1488]
	mov r9, r8
	mov qword ptr [rbp + -1504], r9
	mov r8, qword ptr [rbp + -1504]
	mov r9, qword ptr [rbp + -1496]
	imul r8, r9
	mov qword ptr [rbp + -1504], r8
	mov r8, qword ptr [rbp + -1472]
	mov r9, qword ptr [rbp + -1504]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1512], r8
	mov r8, qword ptr [rbp + -1512]
	mov r9, r8
	mov qword ptr [rbp + -1520], r9
	mov r8, 8
	mov qword ptr [rbp + -1528], r8
	mov r8, qword ptr [rbp + -1488]
	mov r9, r8
	mov qword ptr [rbp + -1536], r9
	mov r8, qword ptr [rbp + -1536]
	mov r9, qword ptr [rbp + -1528]
	imul r8, r9
	mov qword ptr [rbp + -1536], r8
	mov r8, qword ptr [rbp + -1288]
	mov r9, qword ptr [rbp + -1536]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1544], r8
	mov r8, qword ptr [rbp + -1544]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1544], r8
	mov r8, qword ptr [rbp + -1520]
	mov r9, qword ptr [rbp + -1544]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1552], r8
	mov r8, qword ptr [rbp + -1488]
	mov r9, qword ptr [rbp + -1552]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1560], r8
	mov r8, qword ptr [rbp + -1560]
	mov r9, r8
	mov qword ptr [rbp + -1488], r9
	jmp _l17
	_l15:
	mov r8, 8
	mov qword ptr [rbp + -1568], r8
	mov r8, qword ptr [rbp + -1344]
	mov r9, r8
	mov qword ptr [rbp + -1576], r9
	mov r8, qword ptr [rbp + -1576]
	mov r9, qword ptr [rbp + -1568]
	imul r8, r9
	mov qword ptr [rbp + -1576], r8
	mov r8, qword ptr [rbp + -1472]
	mov r9, qword ptr [rbp + -1576]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1584], r8
	mov r8, qword ptr [rbp + -1584]
	mov r9, r8
	mov qword ptr [rbp + -1592], r9
	mov r8, 0
	mov qword ptr [rbp + -1600], r8
	mov r8, qword ptr [rbp + -1600]
	mov r9, r8
	mov qword ptr [rbp + -1608], r9
	_l14:
	mov r8, qword ptr [rbp + -1608]
	mov r9, qword ptr [rbp + -1368]
	cmp r8, r9
	jge _l12
	_l13:
	mov r8, 8
	mov qword ptr [rbp + -1616], r8
	mov r8, qword ptr [rbp + -1608]
	mov r9, r8
	mov qword ptr [rbp + -1624], r9
	mov r8, qword ptr [rbp + -1624]
	mov r9, qword ptr [rbp + -1616]
	imul r8, r9
	mov qword ptr [rbp + -1624], r8
	mov r8, qword ptr [rbp + -1592]
	mov r9, qword ptr [rbp + -1624]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1632], r8
	mov r8, qword ptr [rbp + -1632]
	mov r9, r8
	mov qword ptr [rbp + -1640], r9
	mov r8, 8
	mov qword ptr [rbp + -1648], r8
	mov r8, qword ptr [rbp + -1608]
	mov r9, r8
	mov qword ptr [rbp + -1656], r9
	mov r8, qword ptr [rbp + -1656]
	mov r9, qword ptr [rbp + -1648]
	imul r8, r9
	mov qword ptr [rbp + -1656], r8
	mov r8, qword ptr [rbp + -1320]
	mov r9, qword ptr [rbp + -1656]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1664], r8
	mov r8, qword ptr [rbp + -1664]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1664], r8
	mov r8, qword ptr [rbp + -1640]
	mov r9, qword ptr [rbp + -1664]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1672], r8
	mov r8, qword ptr [rbp + -1608]
	mov r9, qword ptr [rbp + -1672]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1680], r8
	mov r8, qword ptr [rbp + -1680]
	mov r9, r8
	mov qword ptr [rbp + -1608], r9
	jmp _l14
	_l12:
	mov r8, qword ptr [rbp + -1472]
	mov r9, r8
	mov qword ptr [rbp + -1688], r9
	mov r8, qword ptr [rbp + -1688]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g3[rip]
	mov qword ptr [rbp + -1696], r8
	mov r8, qword ptr [rbp + -1696]
	mov r9, r8
	mov qword ptr [rbp + -1704], r9
	mov r8, 8
	mov qword ptr [rbp + -1712], r8
	mov r8, qword ptr [rbp + -1704]
	mov r9, qword ptr [rbp + -1712]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1720], r8
	mov r8, qword ptr [rbp + -1720]
	mov r9, r8
	mov qword ptr [rbp + -1728], r9
	mov r8, qword ptr [rbp + -1728]
	mov r9, r8
	mov qword ptr [rbp + -1736], r9
	mov r8, qword ptr [rbp + -280]
	mov r9, r8
	mov qword ptr [rbp + -1744], r9
	mov r8, qword ptr [rbp + -1744]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -1752], r9
	mov r8, qword ptr [rbp + -1752]
	mov r9, r8
	mov qword ptr [rbp + -1760], r9
	mov r8, qword ptr [rbp + -1760]
	mov r9, r8
	mov qword ptr [rbp + -1768], r9
	mov r8, 8
	mov qword ptr [rbp + -1776], r8
	mov r8, qword ptr [rbp + -1736]
	mov r9, r8
	mov qword ptr [rbp + -1784], r9
	mov r8, qword ptr [rbp + -1784]
	mov r9, qword ptr [rbp + -1776]
	sub r8, r9
	mov qword ptr [rbp + -1784], r8
	mov r8, qword ptr [rbp + -1784]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1784], r8
	mov r8, qword ptr [rbp + -1784]
	mov r9, r8
	mov qword ptr [rbp + -1792], r9
	mov r8, 8
	mov qword ptr [rbp + -1800], r8
	mov r8, qword ptr [rbp + -1768]
	mov r9, r8
	mov qword ptr [rbp + -1808], r9
	mov r8, qword ptr [rbp + -1808]
	mov r9, qword ptr [rbp + -1800]
	sub r8, r9
	mov qword ptr [rbp + -1808], r8
	mov r8, qword ptr [rbp + -1808]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1808], r8
	mov r8, qword ptr [rbp + -1808]
	mov r9, r8
	mov qword ptr [rbp + -1816], r9
	mov r8, qword ptr [rbp + -1792]
	mov r9, qword ptr [rbp + -1816]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1824], r8
	mov r8, qword ptr [rbp + -1824]
	mov r9, r8
	mov qword ptr [rbp + -1832], r9
	mov r8, 8
	mov qword ptr [rbp + -1840], r8
	mov r8, 8
	mov qword ptr [rbp + -1848], r8
	mov r8, qword ptr [rbp + -1832]
	mov r9, r8
	mov qword ptr [rbp + -1856], r9
	mov r8, qword ptr [rbp + -1856]
	mov r9, qword ptr [rbp + -1848]
	imul r8, r9
	mov qword ptr [rbp + -1856], r8
	mov r8, qword ptr [rbp + -1840]
	mov r9, qword ptr [rbp + -1856]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1864], r8
	mov r8, qword ptr [rbp + -1864]
	mov r9, r8
	mov qword ptr [rbp + -1872], r9
	mov r8, qword ptr [rbp + -1872]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -1880], r9
	mov r8, qword ptr [rbp + -1880]
	mov r9, r8
	mov qword ptr [rbp + -1888], r9
	mov r8, qword ptr [rbp + -1888]
	mov r9, r8
	mov qword ptr [rbp + -1896], r9
	mov r8, qword ptr [rbp + -1896]
	mov r9, qword ptr [rbp + -1832]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -1904], r8
	mov r8, qword ptr [rbp + -1896]
	mov r9, qword ptr [rbp + -1904]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1912], r8
	mov r8, qword ptr [rbp + -1912]
	mov r9, r8
	mov qword ptr [rbp + -1920], r9
	mov r8, 0
	mov qword ptr [rbp + -1928], r8
	mov r8, qword ptr [rbp + -1928]
	mov r9, r8
	mov qword ptr [rbp + -1936], r9
	_l23:
	mov r8, qword ptr [rbp + -1936]
	mov r9, qword ptr [rbp + -1792]
	cmp r8, r9
	jge _l21
	_l22:
	mov r8, 8
	mov qword ptr [rbp + -1944], r8
	mov r8, qword ptr [rbp + -1936]
	mov r9, r8
	mov qword ptr [rbp + -1952], r9
	mov r8, qword ptr [rbp + -1952]
	mov r9, qword ptr [rbp + -1944]
	imul r8, r9
	mov qword ptr [rbp + -1952], r8
	mov r8, qword ptr [rbp + -1920]
	mov r9, qword ptr [rbp + -1952]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1960], r8
	mov r8, qword ptr [rbp + -1960]
	mov r9, r8
	mov qword ptr [rbp + -1968], r9
	mov r8, 8
	mov qword ptr [rbp + -1976], r8
	mov r8, qword ptr [rbp + -1936]
	mov r9, r8
	mov qword ptr [rbp + -1984], r9
	mov r8, qword ptr [rbp + -1984]
	mov r9, qword ptr [rbp + -1976]
	imul r8, r9
	mov qword ptr [rbp + -1984], r8
	mov r8, qword ptr [rbp + -1736]
	mov r9, qword ptr [rbp + -1984]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1992], r8
	mov r8, qword ptr [rbp + -1992]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1992], r8
	mov r8, qword ptr [rbp + -1968]
	mov r9, qword ptr [rbp + -1992]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2000], r8
	mov r8, qword ptr [rbp + -1936]
	mov r9, qword ptr [rbp + -2000]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2008], r8
	mov r8, qword ptr [rbp + -2008]
	mov r9, r8
	mov qword ptr [rbp + -1936], r9
	jmp _l23
	_l21:
	mov r8, 8
	mov qword ptr [rbp + -2016], r8
	mov r8, qword ptr [rbp + -1792]
	mov r9, r8
	mov qword ptr [rbp + -2024], r9
	mov r8, qword ptr [rbp + -2024]
	mov r9, qword ptr [rbp + -2016]
	imul r8, r9
	mov qword ptr [rbp + -2024], r8
	mov r8, qword ptr [rbp + -1920]
	mov r9, qword ptr [rbp + -2024]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2032], r8
	mov r8, qword ptr [rbp + -2032]
	mov r9, r8
	mov qword ptr [rbp + -2040], r9
	mov r8, 0
	mov qword ptr [rbp + -2048], r8
	mov r8, qword ptr [rbp + -2048]
	mov r9, r8
	mov qword ptr [rbp + -2056], r9
	_l20:
	mov r8, qword ptr [rbp + -2056]
	mov r9, qword ptr [rbp + -1816]
	cmp r8, r9
	jge _l18
	_l19:
	mov r8, 8
	mov qword ptr [rbp + -2064], r8
	mov r8, qword ptr [rbp + -2056]
	mov r9, r8
	mov qword ptr [rbp + -2072], r9
	mov r8, qword ptr [rbp + -2072]
	mov r9, qword ptr [rbp + -2064]
	imul r8, r9
	mov qword ptr [rbp + -2072], r8
	mov r8, qword ptr [rbp + -2040]
	mov r9, qword ptr [rbp + -2072]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2080], r8
	mov r8, qword ptr [rbp + -2080]
	mov r9, r8
	mov qword ptr [rbp + -2088], r9
	mov r8, 8
	mov qword ptr [rbp + -2096], r8
	mov r8, qword ptr [rbp + -2056]
	mov r9, r8
	mov qword ptr [rbp + -2104], r9
	mov r8, qword ptr [rbp + -2104]
	mov r9, qword ptr [rbp + -2096]
	imul r8, r9
	mov qword ptr [rbp + -2104], r8
	mov r8, qword ptr [rbp + -1768]
	mov r9, qword ptr [rbp + -2104]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2112], r8
	mov r8, qword ptr [rbp + -2112]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2112], r8
	mov r8, qword ptr [rbp + -2088]
	mov r9, qword ptr [rbp + -2112]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2120], r8
	mov r8, qword ptr [rbp + -2056]
	mov r9, qword ptr [rbp + -2120]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2128], r8
	mov r8, qword ptr [rbp + -2128]
	mov r9, r8
	mov qword ptr [rbp + -2056], r9
	jmp _l20
	_l18:
	mov r8, qword ptr [rbp + -1920]
	mov r9, r8
	mov qword ptr [rbp + -2136], r9
	mov r8, qword ptr [rbp + -2136]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g4[rip]
	mov qword ptr [rbp + -2144], r8
	mov r8, qword ptr [rbp + -2144]
	mov r9, r8
	mov qword ptr [rbp + -2152], r9
	mov r8, 8
	mov qword ptr [rbp + -2160], r8
	mov r8, qword ptr [rbp + -2152]
	mov r9, qword ptr [rbp + -2160]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2168], r8
	mov r8, qword ptr [rbp + -2168]
	mov r9, r8
	mov qword ptr [rbp + -2176], r9
	mov r8, qword ptr [rbp + -2176]
	mov r9, r8
	mov qword ptr [rbp + -2184], r9
	mov r8, qword ptr [rbp + -296]
	mov r9, r8
	mov qword ptr [rbp + -2192], r9
	mov r8, qword ptr [rbp + -2192]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -2200], r9
	mov r8, qword ptr [rbp + -2200]
	mov r9, r8
	mov qword ptr [rbp + -2208], r9
	mov r8, qword ptr [rbp + -2208]
	mov r9, r8
	mov qword ptr [rbp + -2216], r9
	mov r8, 8
	mov qword ptr [rbp + -2224], r8
	mov r8, qword ptr [rbp + -2184]
	mov r9, r8
	mov qword ptr [rbp + -2232], r9
	mov r8, qword ptr [rbp + -2232]
	mov r9, qword ptr [rbp + -2224]
	sub r8, r9
	mov qword ptr [rbp + -2232], r8
	mov r8, qword ptr [rbp + -2232]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2232], r8
	mov r8, qword ptr [rbp + -2232]
	mov r9, r8
	mov qword ptr [rbp + -2240], r9
	mov r8, 8
	mov qword ptr [rbp + -2248], r8
	mov r8, qword ptr [rbp + -2216]
	mov r9, r8
	mov qword ptr [rbp + -2256], r9
	mov r8, qword ptr [rbp + -2256]
	mov r9, qword ptr [rbp + -2248]
	sub r8, r9
	mov qword ptr [rbp + -2256], r8
	mov r8, qword ptr [rbp + -2256]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2256], r8
	mov r8, qword ptr [rbp + -2256]
	mov r9, r8
	mov qword ptr [rbp + -2264], r9
	mov r8, qword ptr [rbp + -2240]
	mov r9, qword ptr [rbp + -2264]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2272], r8
	mov r8, qword ptr [rbp + -2272]
	mov r9, r8
	mov qword ptr [rbp + -2280], r9
	mov r8, 8
	mov qword ptr [rbp + -2288], r8
	mov r8, 8
	mov qword ptr [rbp + -2296], r8
	mov r8, qword ptr [rbp + -2280]
	mov r9, r8
	mov qword ptr [rbp + -2304], r9
	mov r8, qword ptr [rbp + -2304]
	mov r9, qword ptr [rbp + -2296]
	imul r8, r9
	mov qword ptr [rbp + -2304], r8
	mov r8, qword ptr [rbp + -2288]
	mov r9, qword ptr [rbp + -2304]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2312], r8
	mov r8, qword ptr [rbp + -2312]
	mov r9, r8
	mov qword ptr [rbp + -2320], r9
	mov r8, qword ptr [rbp + -2320]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -2328], r9
	mov r8, qword ptr [rbp + -2328]
	mov r9, r8
	mov qword ptr [rbp + -2336], r9
	mov r8, qword ptr [rbp + -2336]
	mov r9, r8
	mov qword ptr [rbp + -2344], r9
	mov r8, qword ptr [rbp + -2344]
	mov r9, qword ptr [rbp + -2280]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -2352], r8
	mov r8, qword ptr [rbp + -2344]
	mov r9, qword ptr [rbp + -2352]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2360], r8
	mov r8, qword ptr [rbp + -2360]
	mov r9, r8
	mov qword ptr [rbp + -2368], r9
	mov r8, 0
	mov qword ptr [rbp + -2376], r8
	mov r8, qword ptr [rbp + -2376]
	mov r9, r8
	mov qword ptr [rbp + -2384], r9
	_l29:
	mov r8, qword ptr [rbp + -2384]
	mov r9, qword ptr [rbp + -2240]
	cmp r8, r9
	jge _l27
	_l28:
	mov r8, 8
	mov qword ptr [rbp + -2392], r8
	mov r8, qword ptr [rbp + -2384]
	mov r9, r8
	mov qword ptr [rbp + -2400], r9
	mov r8, qword ptr [rbp + -2400]
	mov r9, qword ptr [rbp + -2392]
	imul r8, r9
	mov qword ptr [rbp + -2400], r8
	mov r8, qword ptr [rbp + -2368]
	mov r9, qword ptr [rbp + -2400]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2408], r8
	mov r8, qword ptr [rbp + -2408]
	mov r9, r8
	mov qword ptr [rbp + -2416], r9
	mov r8, 8
	mov qword ptr [rbp + -2424], r8
	mov r8, qword ptr [rbp + -2384]
	mov r9, r8
	mov qword ptr [rbp + -2432], r9
	mov r8, qword ptr [rbp + -2432]
	mov r9, qword ptr [rbp + -2424]
	imul r8, r9
	mov qword ptr [rbp + -2432], r8
	mov r8, qword ptr [rbp + -2184]
	mov r9, qword ptr [rbp + -2432]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2440], r8
	mov r8, qword ptr [rbp + -2440]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2440], r8
	mov r8, qword ptr [rbp + -2416]
	mov r9, qword ptr [rbp + -2440]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2448], r8
	mov r8, qword ptr [rbp + -2384]
	mov r9, qword ptr [rbp + -2448]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2456], r8
	mov r8, qword ptr [rbp + -2456]
	mov r9, r8
	mov qword ptr [rbp + -2384], r9
	jmp _l29
	_l27:
	mov r8, 8
	mov qword ptr [rbp + -2464], r8
	mov r8, qword ptr [rbp + -2240]
	mov r9, r8
	mov qword ptr [rbp + -2472], r9
	mov r8, qword ptr [rbp + -2472]
	mov r9, qword ptr [rbp + -2464]
	imul r8, r9
	mov qword ptr [rbp + -2472], r8
	mov r8, qword ptr [rbp + -2368]
	mov r9, qword ptr [rbp + -2472]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2480], r8
	mov r8, qword ptr [rbp + -2480]
	mov r9, r8
	mov qword ptr [rbp + -2488], r9
	mov r8, 0
	mov qword ptr [rbp + -2496], r8
	mov r8, qword ptr [rbp + -2496]
	mov r9, r8
	mov qword ptr [rbp + -2504], r9
	_l26:
	mov r8, qword ptr [rbp + -2504]
	mov r9, qword ptr [rbp + -2264]
	cmp r8, r9
	jge _l24
	_l25:
	mov r8, 8
	mov qword ptr [rbp + -2512], r8
	mov r8, qword ptr [rbp + -2504]
	mov r9, r8
	mov qword ptr [rbp + -2520], r9
	mov r8, qword ptr [rbp + -2520]
	mov r9, qword ptr [rbp + -2512]
	imul r8, r9
	mov qword ptr [rbp + -2520], r8
	mov r8, qword ptr [rbp + -2488]
	mov r9, qword ptr [rbp + -2520]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2528], r8
	mov r8, qword ptr [rbp + -2528]
	mov r9, r8
	mov qword ptr [rbp + -2536], r9
	mov r8, 8
	mov qword ptr [rbp + -2544], r8
	mov r8, qword ptr [rbp + -2504]
	mov r9, r8
	mov qword ptr [rbp + -2552], r9
	mov r8, qword ptr [rbp + -2552]
	mov r9, qword ptr [rbp + -2544]
	imul r8, r9
	mov qword ptr [rbp + -2552], r8
	mov r8, qword ptr [rbp + -2216]
	mov r9, qword ptr [rbp + -2552]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2560], r8
	mov r8, qword ptr [rbp + -2560]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2560], r8
	mov r8, qword ptr [rbp + -2536]
	mov r9, qword ptr [rbp + -2560]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2568], r8
	mov r8, qword ptr [rbp + -2504]
	mov r9, qword ptr [rbp + -2568]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2576], r8
	mov r8, qword ptr [rbp + -2576]
	mov r9, r8
	mov qword ptr [rbp + -2504], r9
	jmp _l26
	_l24:
	mov r8, qword ptr [rbp + -2368]
	mov r9, r8
	mov qword ptr [rbp + -2584], r9
	mov r8, qword ptr [rbp + -2584]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g5[rip]
	mov qword ptr [rbp + -2592], r8
	mov r8, qword ptr [rbp + -2592]
	mov r9, r8
	mov qword ptr [rbp + -2600], r9
	mov r8, 8
	mov qword ptr [rbp + -2608], r8
	mov r8, qword ptr [rbp + -2600]
	mov r9, qword ptr [rbp + -2608]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2616], r8
	mov r8, qword ptr [rbp + -2616]
	mov r9, r8
	mov qword ptr [rbp + -2624], r9
	mov r8, qword ptr [rbp + -2624]
	mov r9, r8
	mov qword ptr [rbp + -2632], r9
	mov r8, qword ptr [rbp + -312]
	mov r9, r8
	mov qword ptr [rbp + -2640], r9
	mov r8, qword ptr [rbp + -2640]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -2648], r9
	mov r8, qword ptr [rbp + -2648]
	mov r9, r8
	mov qword ptr [rbp + -2656], r9
	mov r8, qword ptr [rbp + -2656]
	mov r9, r8
	mov qword ptr [rbp + -2664], r9
	mov r8, 8
	mov qword ptr [rbp + -2672], r8
	mov r8, qword ptr [rbp + -2632]
	mov r9, r8
	mov qword ptr [rbp + -2680], r9
	mov r8, qword ptr [rbp + -2680]
	mov r9, qword ptr [rbp + -2672]
	sub r8, r9
	mov qword ptr [rbp + -2680], r8
	mov r8, qword ptr [rbp + -2680]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2680], r8
	mov r8, qword ptr [rbp + -2680]
	mov r9, r8
	mov qword ptr [rbp + -2688], r9
	mov r8, 8
	mov qword ptr [rbp + -2696], r8
	mov r8, qword ptr [rbp + -2664]
	mov r9, r8
	mov qword ptr [rbp + -2704], r9
	mov r8, qword ptr [rbp + -2704]
	mov r9, qword ptr [rbp + -2696]
	sub r8, r9
	mov qword ptr [rbp + -2704], r8
	mov r8, qword ptr [rbp + -2704]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2704], r8
	mov r8, qword ptr [rbp + -2704]
	mov r9, r8
	mov qword ptr [rbp + -2712], r9
	mov r8, qword ptr [rbp + -2688]
	mov r9, qword ptr [rbp + -2712]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2720], r8
	mov r8, qword ptr [rbp + -2720]
	mov r9, r8
	mov qword ptr [rbp + -2728], r9
	mov r8, 8
	mov qword ptr [rbp + -2736], r8
	mov r8, 8
	mov qword ptr [rbp + -2744], r8
	mov r8, qword ptr [rbp + -2728]
	mov r9, r8
	mov qword ptr [rbp + -2752], r9
	mov r8, qword ptr [rbp + -2752]
	mov r9, qword ptr [rbp + -2744]
	imul r8, r9
	mov qword ptr [rbp + -2752], r8
	mov r8, qword ptr [rbp + -2736]
	mov r9, qword ptr [rbp + -2752]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2760], r8
	mov r8, qword ptr [rbp + -2760]
	mov r9, r8
	mov qword ptr [rbp + -2768], r9
	mov r8, qword ptr [rbp + -2768]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -2776], r9
	mov r8, qword ptr [rbp + -2776]
	mov r9, r8
	mov qword ptr [rbp + -2784], r9
	mov r8, qword ptr [rbp + -2784]
	mov r9, r8
	mov qword ptr [rbp + -2792], r9
	mov r8, qword ptr [rbp + -2792]
	mov r9, qword ptr [rbp + -2728]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -2800], r8
	mov r8, qword ptr [rbp + -2792]
	mov r9, qword ptr [rbp + -2800]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2808], r8
	mov r8, qword ptr [rbp + -2808]
	mov r9, r8
	mov qword ptr [rbp + -2816], r9
	mov r8, 0
	mov qword ptr [rbp + -2824], r8
	mov r8, qword ptr [rbp + -2824]
	mov r9, r8
	mov qword ptr [rbp + -2832], r9
	_l35:
	mov r8, qword ptr [rbp + -2832]
	mov r9, qword ptr [rbp + -2688]
	cmp r8, r9
	jge _l33
	_l34:
	mov r8, 8
	mov qword ptr [rbp + -2840], r8
	mov r8, qword ptr [rbp + -2832]
	mov r9, r8
	mov qword ptr [rbp + -2848], r9
	mov r8, qword ptr [rbp + -2848]
	mov r9, qword ptr [rbp + -2840]
	imul r8, r9
	mov qword ptr [rbp + -2848], r8
	mov r8, qword ptr [rbp + -2816]
	mov r9, qword ptr [rbp + -2848]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2856], r8
	mov r8, qword ptr [rbp + -2856]
	mov r9, r8
	mov qword ptr [rbp + -2864], r9
	mov r8, 8
	mov qword ptr [rbp + -2872], r8
	mov r8, qword ptr [rbp + -2832]
	mov r9, r8
	mov qword ptr [rbp + -2880], r9
	mov r8, qword ptr [rbp + -2880]
	mov r9, qword ptr [rbp + -2872]
	imul r8, r9
	mov qword ptr [rbp + -2880], r8
	mov r8, qword ptr [rbp + -2632]
	mov r9, qword ptr [rbp + -2880]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2888], r8
	mov r8, qword ptr [rbp + -2888]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2888], r8
	mov r8, qword ptr [rbp + -2864]
	mov r9, qword ptr [rbp + -2888]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2896], r8
	mov r8, qword ptr [rbp + -2832]
	mov r9, qword ptr [rbp + -2896]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2904], r8
	mov r8, qword ptr [rbp + -2904]
	mov r9, r8
	mov qword ptr [rbp + -2832], r9
	jmp _l35
	_l33:
	mov r8, 8
	mov qword ptr [rbp + -2912], r8
	mov r8, qword ptr [rbp + -2688]
	mov r9, r8
	mov qword ptr [rbp + -2920], r9
	mov r8, qword ptr [rbp + -2920]
	mov r9, qword ptr [rbp + -2912]
	imul r8, r9
	mov qword ptr [rbp + -2920], r8
	mov r8, qword ptr [rbp + -2816]
	mov r9, qword ptr [rbp + -2920]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2928], r8
	mov r8, qword ptr [rbp + -2928]
	mov r9, r8
	mov qword ptr [rbp + -2936], r9
	mov r8, 0
	mov qword ptr [rbp + -2944], r8
	mov r8, qword ptr [rbp + -2944]
	mov r9, r8
	mov qword ptr [rbp + -2952], r9
	_l32:
	mov r8, qword ptr [rbp + -2952]
	mov r9, qword ptr [rbp + -2712]
	cmp r8, r9
	jge _l30
	_l31:
	mov r8, 8
	mov qword ptr [rbp + -2960], r8
	mov r8, qword ptr [rbp + -2952]
	mov r9, r8
	mov qword ptr [rbp + -2968], r9
	mov r8, qword ptr [rbp + -2968]
	mov r9, qword ptr [rbp + -2960]
	imul r8, r9
	mov qword ptr [rbp + -2968], r8
	mov r8, qword ptr [rbp + -2936]
	mov r9, qword ptr [rbp + -2968]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2976], r8
	mov r8, qword ptr [rbp + -2976]
	mov r9, r8
	mov qword ptr [rbp + -2984], r9
	mov r8, 8
	mov qword ptr [rbp + -2992], r8
	mov r8, qword ptr [rbp + -2952]
	mov r9, r8
	mov qword ptr [rbp + -3000], r9
	mov r8, qword ptr [rbp + -3000]
	mov r9, qword ptr [rbp + -2992]
	imul r8, r9
	mov qword ptr [rbp + -3000], r8
	mov r8, qword ptr [rbp + -2664]
	mov r9, qword ptr [rbp + -3000]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3008], r8
	mov r8, qword ptr [rbp + -3008]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3008], r8
	mov r8, qword ptr [rbp + -2984]
	mov r9, qword ptr [rbp + -3008]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3016], r8
	mov r8, qword ptr [rbp + -2952]
	mov r9, qword ptr [rbp + -3016]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3024], r8
	mov r8, qword ptr [rbp + -3024]
	mov r9, r8
	mov qword ptr [rbp + -2952], r9
	jmp _l32
	_l30:
	mov r8, qword ptr [rbp + -2816]
	mov r9, r8
	mov qword ptr [rbp + -3032], r9
	mov r8, qword ptr [rbp + -3032]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g6[rip]
	mov qword ptr [rbp + -3040], r8
	mov r8, qword ptr [rbp + -3040]
	mov r9, r8
	mov qword ptr [rbp + -3048], r9
	mov r8, 8
	mov qword ptr [rbp + -3056], r8
	mov r8, qword ptr [rbp + -3048]
	mov r9, qword ptr [rbp + -3056]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3064], r8
	mov r8, qword ptr [rbp + -3064]
	mov r9, r8
	mov qword ptr [rbp + -3072], r9
	mov r8, qword ptr [rbp + -3072]
	mov r9, r8
	mov qword ptr [rbp + -3080], r9
	mov r8, qword ptr [rbp + -328]
	mov r9, r8
	mov qword ptr [rbp + -3088], r9
	mov r8, qword ptr [rbp + -3088]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -3096], r9
	mov r8, qword ptr [rbp + -3096]
	mov r9, r8
	mov qword ptr [rbp + -3104], r9
	mov r8, qword ptr [rbp + -3104]
	mov r9, r8
	mov qword ptr [rbp + -3112], r9
	mov r8, 8
	mov qword ptr [rbp + -3120], r8
	mov r8, qword ptr [rbp + -3080]
	mov r9, r8
	mov qword ptr [rbp + -3128], r9
	mov r8, qword ptr [rbp + -3128]
	mov r9, qword ptr [rbp + -3120]
	sub r8, r9
	mov qword ptr [rbp + -3128], r8
	mov r8, qword ptr [rbp + -3128]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3128], r8
	mov r8, qword ptr [rbp + -3128]
	mov r9, r8
	mov qword ptr [rbp + -3136], r9
	mov r8, 8
	mov qword ptr [rbp + -3144], r8
	mov r8, qword ptr [rbp + -3112]
	mov r9, r8
	mov qword ptr [rbp + -3152], r9
	mov r8, qword ptr [rbp + -3152]
	mov r9, qword ptr [rbp + -3144]
	sub r8, r9
	mov qword ptr [rbp + -3152], r8
	mov r8, qword ptr [rbp + -3152]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3152], r8
	mov r8, qword ptr [rbp + -3152]
	mov r9, r8
	mov qword ptr [rbp + -3160], r9
	mov r8, qword ptr [rbp + -3136]
	mov r9, qword ptr [rbp + -3160]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3168], r8
	mov r8, qword ptr [rbp + -3168]
	mov r9, r8
	mov qword ptr [rbp + -3176], r9
	mov r8, 8
	mov qword ptr [rbp + -3184], r8
	mov r8, 8
	mov qword ptr [rbp + -3192], r8
	mov r8, qword ptr [rbp + -3176]
	mov r9, r8
	mov qword ptr [rbp + -3200], r9
	mov r8, qword ptr [rbp + -3200]
	mov r9, qword ptr [rbp + -3192]
	imul r8, r9
	mov qword ptr [rbp + -3200], r8
	mov r8, qword ptr [rbp + -3184]
	mov r9, qword ptr [rbp + -3200]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3208], r8
	mov r8, qword ptr [rbp + -3208]
	mov r9, r8
	mov qword ptr [rbp + -3216], r9
	mov r8, qword ptr [rbp + -3216]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -3224], r9
	mov r8, qword ptr [rbp + -3224]
	mov r9, r8
	mov qword ptr [rbp + -3232], r9
	mov r8, qword ptr [rbp + -3232]
	mov r9, r8
	mov qword ptr [rbp + -3240], r9
	mov r8, qword ptr [rbp + -3240]
	mov r9, qword ptr [rbp + -3176]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -3248], r8
	mov r8, qword ptr [rbp + -3240]
	mov r9, qword ptr [rbp + -3248]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3256], r8
	mov r8, qword ptr [rbp + -3256]
	mov r9, r8
	mov qword ptr [rbp + -3264], r9
	mov r8, 0
	mov qword ptr [rbp + -3272], r8
	mov r8, qword ptr [rbp + -3272]
	mov r9, r8
	mov qword ptr [rbp + -3280], r9
	_l41:
	mov r8, qword ptr [rbp + -3280]
	mov r9, qword ptr [rbp + -3136]
	cmp r8, r9
	jge _l39
	_l40:
	mov r8, 8
	mov qword ptr [rbp + -3288], r8
	mov r8, qword ptr [rbp + -3280]
	mov r9, r8
	mov qword ptr [rbp + -3296], r9
	mov r8, qword ptr [rbp + -3296]
	mov r9, qword ptr [rbp + -3288]
	imul r8, r9
	mov qword ptr [rbp + -3296], r8
	mov r8, qword ptr [rbp + -3264]
	mov r9, qword ptr [rbp + -3296]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3304], r8
	mov r8, qword ptr [rbp + -3304]
	mov r9, r8
	mov qword ptr [rbp + -3312], r9
	mov r8, 8
	mov qword ptr [rbp + -3320], r8
	mov r8, qword ptr [rbp + -3280]
	mov r9, r8
	mov qword ptr [rbp + -3328], r9
	mov r8, qword ptr [rbp + -3328]
	mov r9, qword ptr [rbp + -3320]
	imul r8, r9
	mov qword ptr [rbp + -3328], r8
	mov r8, qword ptr [rbp + -3080]
	mov r9, qword ptr [rbp + -3328]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3336], r8
	mov r8, qword ptr [rbp + -3336]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3336], r8
	mov r8, qword ptr [rbp + -3312]
	mov r9, qword ptr [rbp + -3336]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3344], r8
	mov r8, qword ptr [rbp + -3280]
	mov r9, qword ptr [rbp + -3344]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3352], r8
	mov r8, qword ptr [rbp + -3352]
	mov r9, r8
	mov qword ptr [rbp + -3280], r9
	jmp _l41
	_l39:
	mov r8, 8
	mov qword ptr [rbp + -3360], r8
	mov r8, qword ptr [rbp + -3136]
	mov r9, r8
	mov qword ptr [rbp + -3368], r9
	mov r8, qword ptr [rbp + -3368]
	mov r9, qword ptr [rbp + -3360]
	imul r8, r9
	mov qword ptr [rbp + -3368], r8
	mov r8, qword ptr [rbp + -3264]
	mov r9, qword ptr [rbp + -3368]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3376], r8
	mov r8, qword ptr [rbp + -3376]
	mov r9, r8
	mov qword ptr [rbp + -3384], r9
	mov r8, 0
	mov qword ptr [rbp + -3392], r8
	mov r8, qword ptr [rbp + -3392]
	mov r9, r8
	mov qword ptr [rbp + -3400], r9
	_l38:
	mov r8, qword ptr [rbp + -3400]
	mov r9, qword ptr [rbp + -3160]
	cmp r8, r9
	jge _l36
	_l37:
	mov r8, 8
	mov qword ptr [rbp + -3408], r8
	mov r8, qword ptr [rbp + -3400]
	mov r9, r8
	mov qword ptr [rbp + -3416], r9
	mov r8, qword ptr [rbp + -3416]
	mov r9, qword ptr [rbp + -3408]
	imul r8, r9
	mov qword ptr [rbp + -3416], r8
	mov r8, qword ptr [rbp + -3384]
	mov r9, qword ptr [rbp + -3416]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3424], r8
	mov r8, qword ptr [rbp + -3424]
	mov r9, r8
	mov qword ptr [rbp + -3432], r9
	mov r8, 8
	mov qword ptr [rbp + -3440], r8
	mov r8, qword ptr [rbp + -3400]
	mov r9, r8
	mov qword ptr [rbp + -3448], r9
	mov r8, qword ptr [rbp + -3448]
	mov r9, qword ptr [rbp + -3440]
	imul r8, r9
	mov qword ptr [rbp + -3448], r8
	mov r8, qword ptr [rbp + -3112]
	mov r9, qword ptr [rbp + -3448]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3456], r8
	mov r8, qword ptr [rbp + -3456]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3456], r8
	mov r8, qword ptr [rbp + -3432]
	mov r9, qword ptr [rbp + -3456]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3464], r8
	mov r8, qword ptr [rbp + -3400]
	mov r9, qword ptr [rbp + -3464]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3472], r8
	mov r8, qword ptr [rbp + -3472]
	mov r9, r8
	mov qword ptr [rbp + -3400], r9
	jmp _l38
	_l36:
	mov r8, qword ptr [rbp + -3264]
	mov r9, r8
	mov qword ptr [rbp + -3480], r9
	mov r8, qword ptr [rbp + -3480]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g7[rip]
	mov qword ptr [rbp + -3488], r8
	mov r8, qword ptr [rbp + -3488]
	mov r9, r8
	mov qword ptr [rbp + -3496], r9
	mov r8, 8
	mov qword ptr [rbp + -3504], r8
	mov r8, qword ptr [rbp + -3496]
	mov r9, qword ptr [rbp + -3504]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3512], r8
	mov r8, qword ptr [rbp + -3512]
	mov r9, r8
	mov qword ptr [rbp + -3520], r9
	mov r8, qword ptr [rbp + -3520]
	mov r9, r8
	mov qword ptr [rbp + -3528], r9
	mov r8, qword ptr [rbp + -344]
	mov r9, r8
	mov qword ptr [rbp + -3536], r9
	mov r8, qword ptr [rbp + -3536]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -3544], r9
	mov r8, qword ptr [rbp + -3544]
	mov r9, r8
	mov qword ptr [rbp + -3552], r9
	mov r8, qword ptr [rbp + -3552]
	mov r9, r8
	mov qword ptr [rbp + -3560], r9
	mov r8, 8
	mov qword ptr [rbp + -3568], r8
	mov r8, qword ptr [rbp + -3528]
	mov r9, r8
	mov qword ptr [rbp + -3576], r9
	mov r8, qword ptr [rbp + -3576]
	mov r9, qword ptr [rbp + -3568]
	sub r8, r9
	mov qword ptr [rbp + -3576], r8
	mov r8, qword ptr [rbp + -3576]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3576], r8
	mov r8, qword ptr [rbp + -3576]
	mov r9, r8
	mov qword ptr [rbp + -3584], r9
	mov r8, 8
	mov qword ptr [rbp + -3592], r8
	mov r8, qword ptr [rbp + -3560]
	mov r9, r8
	mov qword ptr [rbp + -3600], r9
	mov r8, qword ptr [rbp + -3600]
	mov r9, qword ptr [rbp + -3592]
	sub r8, r9
	mov qword ptr [rbp + -3600], r8
	mov r8, qword ptr [rbp + -3600]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3600], r8
	mov r8, qword ptr [rbp + -3600]
	mov r9, r8
	mov qword ptr [rbp + -3608], r9
	mov r8, qword ptr [rbp + -3584]
	mov r9, qword ptr [rbp + -3608]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3616], r8
	mov r8, qword ptr [rbp + -3616]
	mov r9, r8
	mov qword ptr [rbp + -3624], r9
	mov r8, 8
	mov qword ptr [rbp + -3632], r8
	mov r8, 8
	mov qword ptr [rbp + -3640], r8
	mov r8, qword ptr [rbp + -3624]
	mov r9, r8
	mov qword ptr [rbp + -3648], r9
	mov r8, qword ptr [rbp + -3648]
	mov r9, qword ptr [rbp + -3640]
	imul r8, r9
	mov qword ptr [rbp + -3648], r8
	mov r8, qword ptr [rbp + -3632]
	mov r9, qword ptr [rbp + -3648]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3656], r8
	mov r8, qword ptr [rbp + -3656]
	mov r9, r8
	mov qword ptr [rbp + -3664], r9
	mov r8, qword ptr [rbp + -3664]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -160], r8
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -3672], r9
	mov r8, qword ptr [rbp + -3672]
	mov r9, r8
	mov qword ptr [rbp + -3680], r9
	mov r8, qword ptr [rbp + -3680]
	mov r9, r8
	mov qword ptr [rbp + -3688], r9
	mov r8, qword ptr [rbp + -3688]
	mov r9, qword ptr [rbp + -3624]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -3696], r8
	mov r8, qword ptr [rbp + -3688]
	mov r9, qword ptr [rbp + -3696]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3704], r8
	mov r8, qword ptr [rbp + -3704]
	mov r9, r8
	mov qword ptr [rbp + -3712], r9
	mov r8, 0
	mov qword ptr [rbp + -3720], r8
	mov r8, qword ptr [rbp + -3720]
	mov r9, r8
	mov qword ptr [rbp + -3728], r9
	_l47:
	mov r8, qword ptr [rbp + -3728]
	mov r9, qword ptr [rbp + -3584]
	cmp r8, r9
	jge _l45
	_l46:
	mov r8, 8
	mov qword ptr [rbp + -3736], r8
	mov r8, qword ptr [rbp + -3728]
	mov r9, r8
	mov qword ptr [rbp + -3744], r9
	mov r8, qword ptr [rbp + -3744]
	mov r9, qword ptr [rbp + -3736]
	imul r8, r9
	mov qword ptr [rbp + -3744], r8
	mov r8, qword ptr [rbp + -3712]
	mov r9, qword ptr [rbp + -3744]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3752], r8
	mov r8, qword ptr [rbp + -3752]
	mov r9, r8
	mov qword ptr [rbp + -3760], r9
	mov r8, 8
	mov qword ptr [rbp + -3768], r8
	mov r8, qword ptr [rbp + -3728]
	mov r9, r8
	mov qword ptr [rbp + -3776], r9
	mov r8, qword ptr [rbp + -3776]
	mov r9, qword ptr [rbp + -3768]
	imul r8, r9
	mov qword ptr [rbp + -3776], r8
	mov r8, qword ptr [rbp + -3528]
	mov r9, qword ptr [rbp + -3776]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3784], r8
	mov r8, qword ptr [rbp + -3784]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3784], r8
	mov r8, qword ptr [rbp + -3760]
	mov r9, qword ptr [rbp + -3784]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3792], r8
	mov r8, qword ptr [rbp + -3728]
	mov r9, qword ptr [rbp + -3792]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3800], r8
	mov r8, qword ptr [rbp + -3800]
	mov r9, r8
	mov qword ptr [rbp + -3728], r9
	jmp _l47
	_l45:
	mov r8, 8
	mov qword ptr [rbp + -3808], r8
	mov r8, qword ptr [rbp + -3584]
	mov r9, r8
	mov qword ptr [rbp + -3816], r9
	mov r8, qword ptr [rbp + -3816]
	mov r9, qword ptr [rbp + -3808]
	imul r8, r9
	mov qword ptr [rbp + -3816], r8
	mov r8, qword ptr [rbp + -3712]
	mov r9, qword ptr [rbp + -3816]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3824], r8
	mov r8, qword ptr [rbp + -3824]
	mov r9, r8
	mov qword ptr [rbp + -3832], r9
	mov r8, 0
	mov qword ptr [rbp + -3840], r8
	mov r8, qword ptr [rbp + -3840]
	mov r9, r8
	mov qword ptr [rbp + -3848], r9
	_l44:
	mov r8, qword ptr [rbp + -3848]
	mov r9, qword ptr [rbp + -3608]
	cmp r8, r9
	jge _l42
	_l43:
	mov r8, 8
	mov qword ptr [rbp + -3856], r8
	mov r8, qword ptr [rbp + -3848]
	mov r9, r8
	mov qword ptr [rbp + -3864], r9
	mov r8, qword ptr [rbp + -3864]
	mov r9, qword ptr [rbp + -3856]
	imul r8, r9
	mov qword ptr [rbp + -3864], r8
	mov r8, qword ptr [rbp + -3832]
	mov r9, qword ptr [rbp + -3864]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3872], r8
	mov r8, qword ptr [rbp + -3872]
	mov r9, r8
	mov qword ptr [rbp + -3880], r9
	mov r8, 8
	mov qword ptr [rbp + -3888], r8
	mov r8, qword ptr [rbp + -3848]
	mov r9, r8
	mov qword ptr [rbp + -3896], r9
	mov r8, qword ptr [rbp + -3896]
	mov r9, qword ptr [rbp + -3888]
	imul r8, r9
	mov qword ptr [rbp + -3896], r8
	mov r8, qword ptr [rbp + -3560]
	mov r9, qword ptr [rbp + -3896]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3904], r8
	mov r8, qword ptr [rbp + -3904]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3904], r8
	mov r8, qword ptr [rbp + -3880]
	mov r9, qword ptr [rbp + -3904]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3912], r8
	mov r8, qword ptr [rbp + -3848]
	mov r9, qword ptr [rbp + -3912]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3920], r8
	mov r8, qword ptr [rbp + -3920]
	mov r9, r8
	mov qword ptr [rbp + -3848], r9
	jmp _l44
	_l42:
	mov r8, qword ptr [rbp + -3712]
	mov r9, r8
	mov qword ptr [rbp + -3928], r9
	mov r8, qword ptr [rbp + -3928]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
_Ifoo_t8iiiiiiiiiiiiiiii:
	enter 3920, 0
	mov qword ptr [rbp + -8], rdi

	mov rdi, rsi
	mov qword ptr [rbp + -16], rdi

	mov rsi, rdx
	mov qword ptr [rbp + -24], rsi

	mov rdx, rcx
	mov qword ptr [rbp + -32], rdx

	mov rcx, r8
	mov qword ptr [rbp + -40], rcx

	mov r8, r9
	mov qword ptr [rbp + -48], r8
	
	pop r8
	mov qword ptr [rbp + -56], r8
	pop r8
	mov qword ptr [rbp + -64], r8
	pop r8
	mov qword ptr [rbp + -72], r8
	
	mov r8, qword ptr [rbp + -16]
	mov r9, r8
	mov qword ptr [rbp + -80], r9
	mov r8, qword ptr [rbp + -80]
	mov r9, r8
	mov qword ptr [rbp + -88], r9
	mov r8, qword ptr [rbp + -24]
	mov r9, r8
	mov qword ptr [rbp + -96], r9
	mov r8, qword ptr [rbp + -96]
	mov r9, r8
	mov qword ptr [rbp + -104], r9
	mov r8, qword ptr [rbp + -32]
	mov r9, r8
	mov qword ptr [rbp + -112], r9
	mov r8, qword ptr [rbp + -112]
	mov r9, r8
	mov qword ptr [rbp + -120], r9
	mov r8, qword ptr [rbp + -40]
	mov r9, r8
	mov qword ptr [rbp + -128], r9
	mov r8, qword ptr [rbp + -128]
	mov r9, r8
	mov qword ptr [rbp + -136], r9
	mov r8, qword ptr [rbp + -48]
	mov r9, r8
	mov qword ptr [rbp + -144], r9
	mov r8, qword ptr [rbp + -144]
	mov r9, r8
	mov qword ptr [rbp + -152], r9
	mov r8, qword ptr [rbp + -56]
	mov r9, r8
	mov qword ptr [rbp + -160], r9
	mov r8, qword ptr [rbp + -160]
	mov r9, r8
	mov qword ptr [rbp + -168], r9
	mov r8, qword ptr [rbp + -64]
	mov r9, r8
	mov qword ptr [rbp + -176], r9
	mov r8, qword ptr [rbp + -176]
	mov r9, r8
	mov qword ptr [rbp + -184], r9
	mov r8, qword ptr [rbp + -72]
	mov r9, r8
	mov qword ptr [rbp + -192], r9
	mov r8, qword ptr [rbp + -192]
	mov r9, r8
	mov qword ptr [rbp + -200], r9
	lea r8, qword ptr _g8[rip]
	mov qword ptr [rbp + -208], r8
	mov r8, qword ptr [rbp + -208]
	mov r9, r8
	mov qword ptr [rbp + -216], r9
	mov r8, 8
	mov qword ptr [rbp + -224], r8
	mov r8, qword ptr [rbp + -216]
	mov r9, qword ptr [rbp + -224]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -232], r8
	mov r8, qword ptr [rbp + -232]
	mov r9, r8
	mov qword ptr [rbp + -240], r9
	mov r8, qword ptr [rbp + -240]
	mov r9, r8
	mov qword ptr [rbp + -248], r9
	mov r8, qword ptr [rbp + -88]
	mov r9, r8
	mov qword ptr [rbp + -256], r9
	mov r8, qword ptr [rbp + -256]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -272], r9
	mov r8, qword ptr [rbp + -272]
	mov r9, r8
	mov qword ptr [rbp + -280], r9
	mov r8, qword ptr [rbp + -280]
	mov r9, r8
	mov qword ptr [rbp + -288], r9
	mov r8, 8
	mov qword ptr [rbp + -296], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, r8
	mov qword ptr [rbp + -304], r9
	mov r8, qword ptr [rbp + -304]
	mov r9, qword ptr [rbp + -296]
	sub r8, r9
	mov qword ptr [rbp + -304], r8
	mov r8, qword ptr [rbp + -304]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -304], r8
	mov r8, qword ptr [rbp + -304]
	mov r9, r8
	mov qword ptr [rbp + -312], r9
	mov r8, 8
	mov qword ptr [rbp + -320], r8
	mov r8, qword ptr [rbp + -288]
	mov r9, r8
	mov qword ptr [rbp + -328], r9
	mov r8, qword ptr [rbp + -328]
	mov r9, qword ptr [rbp + -320]
	sub r8, r9
	mov qword ptr [rbp + -328], r8
	mov r8, qword ptr [rbp + -328]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -328], r8
	mov r8, qword ptr [rbp + -328]
	mov r9, r8
	mov qword ptr [rbp + -336], r9
	mov r8, qword ptr [rbp + -312]
	mov r9, qword ptr [rbp + -336]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -344], r8
	mov r8, qword ptr [rbp + -344]
	mov r9, r8
	mov qword ptr [rbp + -352], r9
	mov r8, 8
	mov qword ptr [rbp + -360], r8
	mov r8, 8
	mov qword ptr [rbp + -368], r8
	mov r8, qword ptr [rbp + -352]
	mov r9, r8
	mov qword ptr [rbp + -376], r9
	mov r8, qword ptr [rbp + -376]
	mov r9, qword ptr [rbp + -368]
	imul r8, r9
	mov qword ptr [rbp + -376], r8
	mov r8, qword ptr [rbp + -360]
	mov r9, qword ptr [rbp + -376]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -384], r8
	mov r8, qword ptr [rbp + -384]
	mov r9, r8
	mov qword ptr [rbp + -392], r9
	mov r8, qword ptr [rbp + -392]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -400], r9
	mov r8, qword ptr [rbp + -400]
	mov r9, r8
	mov qword ptr [rbp + -408], r9
	mov r8, qword ptr [rbp + -408]
	mov r9, r8
	mov qword ptr [rbp + -416], r9
	mov r8, qword ptr [rbp + -416]
	mov r9, qword ptr [rbp + -352]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -424], r8
	mov r8, qword ptr [rbp + -416]
	mov r9, qword ptr [rbp + -424]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -432], r8
	mov r8, qword ptr [rbp + -432]
	mov r9, r8
	mov qword ptr [rbp + -440], r9
	mov r8, 0
	mov qword ptr [rbp + -448], r8
	mov r8, qword ptr [rbp + -448]
	mov r9, r8
	mov qword ptr [rbp + -456], r9
	_l53:
	mov r8, qword ptr [rbp + -456]
	mov r9, qword ptr [rbp + -312]
	cmp r8, r9
	jge _l51
	_l52:
	mov r8, 8
	mov qword ptr [rbp + -464], r8
	mov r8, qword ptr [rbp + -456]
	mov r9, r8
	mov qword ptr [rbp + -472], r9
	mov r8, qword ptr [rbp + -472]
	mov r9, qword ptr [rbp + -464]
	imul r8, r9
	mov qword ptr [rbp + -472], r8
	mov r8, qword ptr [rbp + -440]
	mov r9, qword ptr [rbp + -472]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -480], r8
	mov r8, qword ptr [rbp + -480]
	mov r9, r8
	mov qword ptr [rbp + -488], r9
	mov r8, 8
	mov qword ptr [rbp + -496], r8
	mov r8, qword ptr [rbp + -456]
	mov r9, r8
	mov qword ptr [rbp + -504], r9
	mov r8, qword ptr [rbp + -504]
	mov r9, qword ptr [rbp + -496]
	imul r8, r9
	mov qword ptr [rbp + -504], r8
	mov r8, qword ptr [rbp + -248]
	mov r9, qword ptr [rbp + -504]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -512], r8
	mov r8, qword ptr [rbp + -512]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -512], r8
	mov r8, qword ptr [rbp + -488]
	mov r9, qword ptr [rbp + -512]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -520], r8
	mov r8, qword ptr [rbp + -456]
	mov r9, qword ptr [rbp + -520]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -528], r8
	mov r8, qword ptr [rbp + -528]
	mov r9, r8
	mov qword ptr [rbp + -456], r9
	jmp _l53
	_l51:
	mov r8, 8
	mov qword ptr [rbp + -536], r8
	mov r8, qword ptr [rbp + -312]
	mov r9, r8
	mov qword ptr [rbp + -544], r9
	mov r8, qword ptr [rbp + -544]
	mov r9, qword ptr [rbp + -536]
	imul r8, r9
	mov qword ptr [rbp + -544], r8
	mov r8, qword ptr [rbp + -440]
	mov r9, qword ptr [rbp + -544]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -552], r8
	mov r8, qword ptr [rbp + -552]
	mov r9, r8
	mov qword ptr [rbp + -560], r9
	mov r8, 0
	mov qword ptr [rbp + -568], r8
	mov r8, qword ptr [rbp + -568]
	mov r9, r8
	mov qword ptr [rbp + -576], r9
	_l50:
	mov r8, qword ptr [rbp + -576]
	mov r9, qword ptr [rbp + -336]
	cmp r8, r9
	jge _l48
	_l49:
	mov r8, 8
	mov qword ptr [rbp + -584], r8
	mov r8, qword ptr [rbp + -576]
	mov r9, r8
	mov qword ptr [rbp + -592], r9
	mov r8, qword ptr [rbp + -592]
	mov r9, qword ptr [rbp + -584]
	imul r8, r9
	mov qword ptr [rbp + -592], r8
	mov r8, qword ptr [rbp + -560]
	mov r9, qword ptr [rbp + -592]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -600], r8
	mov r8, qword ptr [rbp + -600]
	mov r9, r8
	mov qword ptr [rbp + -608], r9
	mov r8, 8
	mov qword ptr [rbp + -616], r8
	mov r8, qword ptr [rbp + -576]
	mov r9, r8
	mov qword ptr [rbp + -624], r9
	mov r8, qword ptr [rbp + -624]
	mov r9, qword ptr [rbp + -616]
	imul r8, r9
	mov qword ptr [rbp + -624], r8
	mov r8, qword ptr [rbp + -288]
	mov r9, qword ptr [rbp + -624]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -632], r8
	mov r8, qword ptr [rbp + -632]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -632], r8
	mov r8, qword ptr [rbp + -608]
	mov r9, qword ptr [rbp + -632]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -640], r8
	mov r8, qword ptr [rbp + -576]
	mov r9, qword ptr [rbp + -640]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -648], r8
	mov r8, qword ptr [rbp + -648]
	mov r9, r8
	mov qword ptr [rbp + -576], r9
	jmp _l50
	_l48:
	mov r8, qword ptr [rbp + -440]
	mov r9, r8
	mov qword ptr [rbp + -656], r9
	mov r8, qword ptr [rbp + -656]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g9[rip]
	mov qword ptr [rbp + -664], r8
	mov r8, qword ptr [rbp + -664]
	mov r9, r8
	mov qword ptr [rbp + -672], r9
	mov r8, 8
	mov qword ptr [rbp + -680], r8
	mov r8, qword ptr [rbp + -672]
	mov r9, qword ptr [rbp + -680]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -688], r8
	mov r8, qword ptr [rbp + -688]
	mov r9, r8
	mov qword ptr [rbp + -696], r9
	mov r8, qword ptr [rbp + -696]
	mov r9, r8
	mov qword ptr [rbp + -704], r9
	mov r8, qword ptr [rbp + -104]
	mov r9, r8
	mov qword ptr [rbp + -712], r9
	mov r8, qword ptr [rbp + -712]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -720], r9
	mov r8, qword ptr [rbp + -720]
	mov r9, r8
	mov qword ptr [rbp + -728], r9
	mov r8, qword ptr [rbp + -728]
	mov r9, r8
	mov qword ptr [rbp + -736], r9
	mov r8, 8
	mov qword ptr [rbp + -744], r8
	mov r8, qword ptr [rbp + -704]
	mov r9, r8
	mov qword ptr [rbp + -752], r9
	mov r8, qword ptr [rbp + -752]
	mov r9, qword ptr [rbp + -744]
	sub r8, r9
	mov qword ptr [rbp + -752], r8
	mov r8, qword ptr [rbp + -752]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -752], r8
	mov r8, qword ptr [rbp + -752]
	mov r9, r8
	mov qword ptr [rbp + -760], r9
	mov r8, 8
	mov qword ptr [rbp + -768], r8
	mov r8, qword ptr [rbp + -736]
	mov r9, r8
	mov qword ptr [rbp + -776], r9
	mov r8, qword ptr [rbp + -776]
	mov r9, qword ptr [rbp + -768]
	sub r8, r9
	mov qword ptr [rbp + -776], r8
	mov r8, qword ptr [rbp + -776]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -776], r8
	mov r8, qword ptr [rbp + -776]
	mov r9, r8
	mov qword ptr [rbp + -784], r9
	mov r8, qword ptr [rbp + -760]
	mov r9, qword ptr [rbp + -784]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -792], r8
	mov r8, qword ptr [rbp + -792]
	mov r9, r8
	mov qword ptr [rbp + -800], r9
	mov r8, 8
	mov qword ptr [rbp + -808], r8
	mov r8, 8
	mov qword ptr [rbp + -816], r8
	mov r8, qword ptr [rbp + -800]
	mov r9, r8
	mov qword ptr [rbp + -824], r9
	mov r8, qword ptr [rbp + -824]
	mov r9, qword ptr [rbp + -816]
	imul r8, r9
	mov qword ptr [rbp + -824], r8
	mov r8, qword ptr [rbp + -808]
	mov r9, qword ptr [rbp + -824]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -832], r8
	mov r8, qword ptr [rbp + -832]
	mov r9, r8
	mov qword ptr [rbp + -840], r9
	mov r8, qword ptr [rbp + -840]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -848], r9
	mov r8, qword ptr [rbp + -848]
	mov r9, r8
	mov qword ptr [rbp + -856], r9
	mov r8, qword ptr [rbp + -856]
	mov r9, r8
	mov qword ptr [rbp + -864], r9
	mov r8, qword ptr [rbp + -864]
	mov r9, qword ptr [rbp + -800]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -872], r8
	mov r8, qword ptr [rbp + -864]
	mov r9, qword ptr [rbp + -872]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -880], r8
	mov r8, qword ptr [rbp + -880]
	mov r9, r8
	mov qword ptr [rbp + -888], r9
	mov r8, 0
	mov qword ptr [rbp + -896], r8
	mov r8, qword ptr [rbp + -896]
	mov r9, r8
	mov qword ptr [rbp + -904], r9
	_l59:
	mov r8, qword ptr [rbp + -904]
	mov r9, qword ptr [rbp + -760]
	cmp r8, r9
	jge _l57
	_l58:
	mov r8, 8
	mov qword ptr [rbp + -912], r8
	mov r8, qword ptr [rbp + -904]
	mov r9, r8
	mov qword ptr [rbp + -920], r9
	mov r8, qword ptr [rbp + -920]
	mov r9, qword ptr [rbp + -912]
	imul r8, r9
	mov qword ptr [rbp + -920], r8
	mov r8, qword ptr [rbp + -888]
	mov r9, qword ptr [rbp + -920]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -928], r8
	mov r8, qword ptr [rbp + -928]
	mov r9, r8
	mov qword ptr [rbp + -936], r9
	mov r8, 8
	mov qword ptr [rbp + -944], r8
	mov r8, qword ptr [rbp + -904]
	mov r9, r8
	mov qword ptr [rbp + -952], r9
	mov r8, qword ptr [rbp + -952]
	mov r9, qword ptr [rbp + -944]
	imul r8, r9
	mov qword ptr [rbp + -952], r8
	mov r8, qword ptr [rbp + -704]
	mov r9, qword ptr [rbp + -952]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -960], r8
	mov r8, qword ptr [rbp + -960]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -960], r8
	mov r8, qword ptr [rbp + -936]
	mov r9, qword ptr [rbp + -960]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -968], r8
	mov r8, qword ptr [rbp + -904]
	mov r9, qword ptr [rbp + -968]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -976], r8
	mov r8, qword ptr [rbp + -976]
	mov r9, r8
	mov qword ptr [rbp + -904], r9
	jmp _l59
	_l57:
	mov r8, 8
	mov qword ptr [rbp + -984], r8
	mov r8, qword ptr [rbp + -760]
	mov r9, r8
	mov qword ptr [rbp + -992], r9
	mov r8, qword ptr [rbp + -992]
	mov r9, qword ptr [rbp + -984]
	imul r8, r9
	mov qword ptr [rbp + -992], r8
	mov r8, qword ptr [rbp + -888]
	mov r9, qword ptr [rbp + -992]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1000], r8
	mov r8, qword ptr [rbp + -1000]
	mov r9, r8
	mov qword ptr [rbp + -1008], r9
	mov r8, 0
	mov qword ptr [rbp + -1016], r8
	mov r8, qword ptr [rbp + -1016]
	mov r9, r8
	mov qword ptr [rbp + -1024], r9
	_l56:
	mov r8, qword ptr [rbp + -1024]
	mov r9, qword ptr [rbp + -784]
	cmp r8, r9
	jge _l54
	_l55:
	mov r8, 8
	mov qword ptr [rbp + -1032], r8
	mov r8, qword ptr [rbp + -1024]
	mov r9, r8
	mov qword ptr [rbp + -1040], r9
	mov r8, qword ptr [rbp + -1040]
	mov r9, qword ptr [rbp + -1032]
	imul r8, r9
	mov qword ptr [rbp + -1040], r8
	mov r8, qword ptr [rbp + -1008]
	mov r9, qword ptr [rbp + -1040]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1048], r8
	mov r8, qword ptr [rbp + -1048]
	mov r9, r8
	mov qword ptr [rbp + -1056], r9
	mov r8, 8
	mov qword ptr [rbp + -1064], r8
	mov r8, qword ptr [rbp + -1024]
	mov r9, r8
	mov qword ptr [rbp + -1072], r9
	mov r8, qword ptr [rbp + -1072]
	mov r9, qword ptr [rbp + -1064]
	imul r8, r9
	mov qword ptr [rbp + -1072], r8
	mov r8, qword ptr [rbp + -736]
	mov r9, qword ptr [rbp + -1072]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1080], r8
	mov r8, qword ptr [rbp + -1080]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1080], r8
	mov r8, qword ptr [rbp + -1056]
	mov r9, qword ptr [rbp + -1080]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1088], r8
	mov r8, qword ptr [rbp + -1024]
	mov r9, qword ptr [rbp + -1088]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1096], r8
	mov r8, qword ptr [rbp + -1096]
	mov r9, r8
	mov qword ptr [rbp + -1024], r9
	jmp _l56
	_l54:
	mov r8, qword ptr [rbp + -888]
	mov r9, r8
	mov qword ptr [rbp + -1104], r9
	mov r8, qword ptr [rbp + -1104]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g10[rip]
	mov qword ptr [rbp + -1112], r8
	mov r8, qword ptr [rbp + -1112]
	mov r9, r8
	mov qword ptr [rbp + -1120], r9
	mov r8, 8
	mov qword ptr [rbp + -1128], r8
	mov r8, qword ptr [rbp + -1120]
	mov r9, qword ptr [rbp + -1128]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1136], r8
	mov r8, qword ptr [rbp + -1136]
	mov r9, r8
	mov qword ptr [rbp + -1144], r9
	mov r8, qword ptr [rbp + -1144]
	mov r9, r8
	mov qword ptr [rbp + -1152], r9
	mov r8, qword ptr [rbp + -120]
	mov r9, r8
	mov qword ptr [rbp + -1160], r9
	mov r8, qword ptr [rbp + -1160]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -1168], r9
	mov r8, qword ptr [rbp + -1168]
	mov r9, r8
	mov qword ptr [rbp + -1176], r9
	mov r8, qword ptr [rbp + -1176]
	mov r9, r8
	mov qword ptr [rbp + -1184], r9
	mov r8, 8
	mov qword ptr [rbp + -1192], r8
	mov r8, qword ptr [rbp + -1152]
	mov r9, r8
	mov qword ptr [rbp + -1200], r9
	mov r8, qword ptr [rbp + -1200]
	mov r9, qword ptr [rbp + -1192]
	sub r8, r9
	mov qword ptr [rbp + -1200], r8
	mov r8, qword ptr [rbp + -1200]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1200], r8
	mov r8, qword ptr [rbp + -1200]
	mov r9, r8
	mov qword ptr [rbp + -1208], r9
	mov r8, 8
	mov qword ptr [rbp + -1216], r8
	mov r8, qword ptr [rbp + -1184]
	mov r9, r8
	mov qword ptr [rbp + -1224], r9
	mov r8, qword ptr [rbp + -1224]
	mov r9, qword ptr [rbp + -1216]
	sub r8, r9
	mov qword ptr [rbp + -1224], r8
	mov r8, qword ptr [rbp + -1224]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1224], r8
	mov r8, qword ptr [rbp + -1224]
	mov r9, r8
	mov qword ptr [rbp + -1232], r9
	mov r8, qword ptr [rbp + -1208]
	mov r9, qword ptr [rbp + -1232]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1240], r8
	mov r8, qword ptr [rbp + -1240]
	mov r9, r8
	mov qword ptr [rbp + -1248], r9
	mov r8, 8
	mov qword ptr [rbp + -1256], r8
	mov r8, 8
	mov qword ptr [rbp + -1264], r8
	mov r8, qword ptr [rbp + -1248]
	mov r9, r8
	mov qword ptr [rbp + -1272], r9
	mov r8, qword ptr [rbp + -1272]
	mov r9, qword ptr [rbp + -1264]
	imul r8, r9
	mov qword ptr [rbp + -1272], r8
	mov r8, qword ptr [rbp + -1256]
	mov r9, qword ptr [rbp + -1272]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1280], r8
	mov r8, qword ptr [rbp + -1280]
	mov r9, r8
	mov qword ptr [rbp + -1288], r9
	mov r8, qword ptr [rbp + -1288]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -1296], r9
	mov r8, qword ptr [rbp + -1296]
	mov r9, r8
	mov qword ptr [rbp + -1304], r9
	mov r8, qword ptr [rbp + -1304]
	mov r9, r8
	mov qword ptr [rbp + -1312], r9
	mov r8, qword ptr [rbp + -1312]
	mov r9, qword ptr [rbp + -1248]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -1320], r8
	mov r8, qword ptr [rbp + -1312]
	mov r9, qword ptr [rbp + -1320]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1328], r8
	mov r8, qword ptr [rbp + -1328]
	mov r9, r8
	mov qword ptr [rbp + -1336], r9
	mov r8, 0
	mov qword ptr [rbp + -1344], r8
	mov r8, qword ptr [rbp + -1344]
	mov r9, r8
	mov qword ptr [rbp + -1352], r9
	_l65:
	mov r8, qword ptr [rbp + -1352]
	mov r9, qword ptr [rbp + -1208]
	cmp r8, r9
	jge _l63
	_l64:
	mov r8, 8
	mov qword ptr [rbp + -1360], r8
	mov r8, qword ptr [rbp + -1352]
	mov r9, r8
	mov qword ptr [rbp + -1368], r9
	mov r8, qword ptr [rbp + -1368]
	mov r9, qword ptr [rbp + -1360]
	imul r8, r9
	mov qword ptr [rbp + -1368], r8
	mov r8, qword ptr [rbp + -1336]
	mov r9, qword ptr [rbp + -1368]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1376], r8
	mov r8, qword ptr [rbp + -1376]
	mov r9, r8
	mov qword ptr [rbp + -1384], r9
	mov r8, 8
	mov qword ptr [rbp + -1392], r8
	mov r8, qword ptr [rbp + -1352]
	mov r9, r8
	mov qword ptr [rbp + -1400], r9
	mov r8, qword ptr [rbp + -1400]
	mov r9, qword ptr [rbp + -1392]
	imul r8, r9
	mov qword ptr [rbp + -1400], r8
	mov r8, qword ptr [rbp + -1152]
	mov r9, qword ptr [rbp + -1400]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1408], r8
	mov r8, qword ptr [rbp + -1408]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1408], r8
	mov r8, qword ptr [rbp + -1384]
	mov r9, qword ptr [rbp + -1408]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1416], r8
	mov r8, qword ptr [rbp + -1352]
	mov r9, qword ptr [rbp + -1416]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1424], r8
	mov r8, qword ptr [rbp + -1424]
	mov r9, r8
	mov qword ptr [rbp + -1352], r9
	jmp _l65
	_l63:
	mov r8, 8
	mov qword ptr [rbp + -1432], r8
	mov r8, qword ptr [rbp + -1208]
	mov r9, r8
	mov qword ptr [rbp + -1440], r9
	mov r8, qword ptr [rbp + -1440]
	mov r9, qword ptr [rbp + -1432]
	imul r8, r9
	mov qword ptr [rbp + -1440], r8
	mov r8, qword ptr [rbp + -1336]
	mov r9, qword ptr [rbp + -1440]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1448], r8
	mov r8, qword ptr [rbp + -1448]
	mov r9, r8
	mov qword ptr [rbp + -1456], r9
	mov r8, 0
	mov qword ptr [rbp + -1464], r8
	mov r8, qword ptr [rbp + -1464]
	mov r9, r8
	mov qword ptr [rbp + -1472], r9
	_l62:
	mov r8, qword ptr [rbp + -1472]
	mov r9, qword ptr [rbp + -1232]
	cmp r8, r9
	jge _l60
	_l61:
	mov r8, 8
	mov qword ptr [rbp + -1480], r8
	mov r8, qword ptr [rbp + -1472]
	mov r9, r8
	mov qword ptr [rbp + -1488], r9
	mov r8, qword ptr [rbp + -1488]
	mov r9, qword ptr [rbp + -1480]
	imul r8, r9
	mov qword ptr [rbp + -1488], r8
	mov r8, qword ptr [rbp + -1456]
	mov r9, qword ptr [rbp + -1488]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1496], r8
	mov r8, qword ptr [rbp + -1496]
	mov r9, r8
	mov qword ptr [rbp + -1504], r9
	mov r8, 8
	mov qword ptr [rbp + -1512], r8
	mov r8, qword ptr [rbp + -1472]
	mov r9, r8
	mov qword ptr [rbp + -1520], r9
	mov r8, qword ptr [rbp + -1520]
	mov r9, qword ptr [rbp + -1512]
	imul r8, r9
	mov qword ptr [rbp + -1520], r8
	mov r8, qword ptr [rbp + -1184]
	mov r9, qword ptr [rbp + -1520]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1528], r8
	mov r8, qword ptr [rbp + -1528]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1528], r8
	mov r8, qword ptr [rbp + -1504]
	mov r9, qword ptr [rbp + -1528]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1536], r8
	mov r8, qword ptr [rbp + -1472]
	mov r9, qword ptr [rbp + -1536]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1544], r8
	mov r8, qword ptr [rbp + -1544]
	mov r9, r8
	mov qword ptr [rbp + -1472], r9
	jmp _l62
	_l60:
	mov r8, qword ptr [rbp + -1336]
	mov r9, r8
	mov qword ptr [rbp + -1552], r9
	mov r8, qword ptr [rbp + -1552]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g11[rip]
	mov qword ptr [rbp + -1560], r8
	mov r8, qword ptr [rbp + -1560]
	mov r9, r8
	mov qword ptr [rbp + -1568], r9
	mov r8, 8
	mov qword ptr [rbp + -1576], r8
	mov r8, qword ptr [rbp + -1568]
	mov r9, qword ptr [rbp + -1576]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1584], r8
	mov r8, qword ptr [rbp + -1584]
	mov r9, r8
	mov qword ptr [rbp + -1592], r9
	mov r8, qword ptr [rbp + -1592]
	mov r9, r8
	mov qword ptr [rbp + -1600], r9
	mov r8, qword ptr [rbp + -136]
	mov r9, r8
	mov qword ptr [rbp + -1608], r9
	mov r8, qword ptr [rbp + -1608]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -1616], r9
	mov r8, qword ptr [rbp + -1616]
	mov r9, r8
	mov qword ptr [rbp + -1624], r9
	mov r8, qword ptr [rbp + -1624]
	mov r9, r8
	mov qword ptr [rbp + -1632], r9
	mov r8, 8
	mov qword ptr [rbp + -1640], r8
	mov r8, qword ptr [rbp + -1600]
	mov r9, r8
	mov qword ptr [rbp + -1648], r9
	mov r8, qword ptr [rbp + -1648]
	mov r9, qword ptr [rbp + -1640]
	sub r8, r9
	mov qword ptr [rbp + -1648], r8
	mov r8, qword ptr [rbp + -1648]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1648], r8
	mov r8, qword ptr [rbp + -1648]
	mov r9, r8
	mov qword ptr [rbp + -1656], r9
	mov r8, 8
	mov qword ptr [rbp + -1664], r8
	mov r8, qword ptr [rbp + -1632]
	mov r9, r8
	mov qword ptr [rbp + -1672], r9
	mov r8, qword ptr [rbp + -1672]
	mov r9, qword ptr [rbp + -1664]
	sub r8, r9
	mov qword ptr [rbp + -1672], r8
	mov r8, qword ptr [rbp + -1672]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1672], r8
	mov r8, qword ptr [rbp + -1672]
	mov r9, r8
	mov qword ptr [rbp + -1680], r9
	mov r8, qword ptr [rbp + -1656]
	mov r9, qword ptr [rbp + -1680]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1688], r8
	mov r8, qword ptr [rbp + -1688]
	mov r9, r8
	mov qword ptr [rbp + -1696], r9
	mov r8, 8
	mov qword ptr [rbp + -1704], r8
	mov r8, 8
	mov qword ptr [rbp + -1712], r8
	mov r8, qword ptr [rbp + -1696]
	mov r9, r8
	mov qword ptr [rbp + -1720], r9
	mov r8, qword ptr [rbp + -1720]
	mov r9, qword ptr [rbp + -1712]
	imul r8, r9
	mov qword ptr [rbp + -1720], r8
	mov r8, qword ptr [rbp + -1704]
	mov r9, qword ptr [rbp + -1720]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1728], r8
	mov r8, qword ptr [rbp + -1728]
	mov r9, r8
	mov qword ptr [rbp + -1736], r9
	mov r8, qword ptr [rbp + -1736]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -1744], r9
	mov r8, qword ptr [rbp + -1744]
	mov r9, r8
	mov qword ptr [rbp + -1752], r9
	mov r8, qword ptr [rbp + -1752]
	mov r9, r8
	mov qword ptr [rbp + -1760], r9
	mov r8, qword ptr [rbp + -1760]
	mov r9, qword ptr [rbp + -1696]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -1768], r8
	mov r8, qword ptr [rbp + -1760]
	mov r9, qword ptr [rbp + -1768]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1776], r8
	mov r8, qword ptr [rbp + -1776]
	mov r9, r8
	mov qword ptr [rbp + -1784], r9
	mov r8, 0
	mov qword ptr [rbp + -1792], r8
	mov r8, qword ptr [rbp + -1792]
	mov r9, r8
	mov qword ptr [rbp + -1800], r9
	_l71:
	mov r8, qword ptr [rbp + -1800]
	mov r9, qword ptr [rbp + -1656]
	cmp r8, r9
	jge _l69
	_l70:
	mov r8, 8
	mov qword ptr [rbp + -1808], r8
	mov r8, qword ptr [rbp + -1800]
	mov r9, r8
	mov qword ptr [rbp + -1816], r9
	mov r8, qword ptr [rbp + -1816]
	mov r9, qword ptr [rbp + -1808]
	imul r8, r9
	mov qword ptr [rbp + -1816], r8
	mov r8, qword ptr [rbp + -1784]
	mov r9, qword ptr [rbp + -1816]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1824], r8
	mov r8, qword ptr [rbp + -1824]
	mov r9, r8
	mov qword ptr [rbp + -1832], r9
	mov r8, 8
	mov qword ptr [rbp + -1840], r8
	mov r8, qword ptr [rbp + -1800]
	mov r9, r8
	mov qword ptr [rbp + -1848], r9
	mov r8, qword ptr [rbp + -1848]
	mov r9, qword ptr [rbp + -1840]
	imul r8, r9
	mov qword ptr [rbp + -1848], r8
	mov r8, qword ptr [rbp + -1600]
	mov r9, qword ptr [rbp + -1848]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1856], r8
	mov r8, qword ptr [rbp + -1856]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1856], r8
	mov r8, qword ptr [rbp + -1832]
	mov r9, qword ptr [rbp + -1856]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1864], r8
	mov r8, qword ptr [rbp + -1800]
	mov r9, qword ptr [rbp + -1864]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1872], r8
	mov r8, qword ptr [rbp + -1872]
	mov r9, r8
	mov qword ptr [rbp + -1800], r9
	jmp _l71
	_l69:
	mov r8, 8
	mov qword ptr [rbp + -1880], r8
	mov r8, qword ptr [rbp + -1656]
	mov r9, r8
	mov qword ptr [rbp + -1888], r9
	mov r8, qword ptr [rbp + -1888]
	mov r9, qword ptr [rbp + -1880]
	imul r8, r9
	mov qword ptr [rbp + -1888], r8
	mov r8, qword ptr [rbp + -1784]
	mov r9, qword ptr [rbp + -1888]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1896], r8
	mov r8, qword ptr [rbp + -1896]
	mov r9, r8
	mov qword ptr [rbp + -1904], r9
	mov r8, 0
	mov qword ptr [rbp + -1912], r8
	mov r8, qword ptr [rbp + -1912]
	mov r9, r8
	mov qword ptr [rbp + -1920], r9
	_l68:
	mov r8, qword ptr [rbp + -1920]
	mov r9, qword ptr [rbp + -1680]
	cmp r8, r9
	jge _l66
	_l67:
	mov r8, 8
	mov qword ptr [rbp + -1928], r8
	mov r8, qword ptr [rbp + -1920]
	mov r9, r8
	mov qword ptr [rbp + -1936], r9
	mov r8, qword ptr [rbp + -1936]
	mov r9, qword ptr [rbp + -1928]
	imul r8, r9
	mov qword ptr [rbp + -1936], r8
	mov r8, qword ptr [rbp + -1904]
	mov r9, qword ptr [rbp + -1936]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1944], r8
	mov r8, qword ptr [rbp + -1944]
	mov r9, r8
	mov qword ptr [rbp + -1952], r9
	mov r8, 8
	mov qword ptr [rbp + -1960], r8
	mov r8, qword ptr [rbp + -1920]
	mov r9, r8
	mov qword ptr [rbp + -1968], r9
	mov r8, qword ptr [rbp + -1968]
	mov r9, qword ptr [rbp + -1960]
	imul r8, r9
	mov qword ptr [rbp + -1968], r8
	mov r8, qword ptr [rbp + -1632]
	mov r9, qword ptr [rbp + -1968]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1976], r8
	mov r8, qword ptr [rbp + -1976]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -1976], r8
	mov r8, qword ptr [rbp + -1952]
	mov r9, qword ptr [rbp + -1976]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -1984], r8
	mov r8, qword ptr [rbp + -1920]
	mov r9, qword ptr [rbp + -1984]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -1992], r8
	mov r8, qword ptr [rbp + -1992]
	mov r9, r8
	mov qword ptr [rbp + -1920], r9
	jmp _l68
	_l66:
	mov r8, qword ptr [rbp + -1784]
	mov r9, r8
	mov qword ptr [rbp + -2000], r9
	mov r8, qword ptr [rbp + -2000]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g12[rip]
	mov qword ptr [rbp + -2008], r8
	mov r8, qword ptr [rbp + -2008]
	mov r9, r8
	mov qword ptr [rbp + -2016], r9
	mov r8, 8
	mov qword ptr [rbp + -2024], r8
	mov r8, qword ptr [rbp + -2016]
	mov r9, qword ptr [rbp + -2024]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2032], r8
	mov r8, qword ptr [rbp + -2032]
	mov r9, r8
	mov qword ptr [rbp + -2040], r9
	mov r8, qword ptr [rbp + -2040]
	mov r9, r8
	mov qword ptr [rbp + -2048], r9
	mov r8, qword ptr [rbp + -152]
	mov r9, r8
	mov qword ptr [rbp + -2056], r9
	mov r8, qword ptr [rbp + -2056]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -2064], r9
	mov r8, qword ptr [rbp + -2064]
	mov r9, r8
	mov qword ptr [rbp + -2072], r9
	mov r8, qword ptr [rbp + -2072]
	mov r9, r8
	mov qword ptr [rbp + -2080], r9
	mov r8, 8
	mov qword ptr [rbp + -2088], r8
	mov r8, qword ptr [rbp + -2048]
	mov r9, r8
	mov qword ptr [rbp + -2096], r9
	mov r8, qword ptr [rbp + -2096]
	mov r9, qword ptr [rbp + -2088]
	sub r8, r9
	mov qword ptr [rbp + -2096], r8
	mov r8, qword ptr [rbp + -2096]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2096], r8
	mov r8, qword ptr [rbp + -2096]
	mov r9, r8
	mov qword ptr [rbp + -2104], r9
	mov r8, 8
	mov qword ptr [rbp + -2112], r8
	mov r8, qword ptr [rbp + -2080]
	mov r9, r8
	mov qword ptr [rbp + -2120], r9
	mov r8, qword ptr [rbp + -2120]
	mov r9, qword ptr [rbp + -2112]
	sub r8, r9
	mov qword ptr [rbp + -2120], r8
	mov r8, qword ptr [rbp + -2120]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2120], r8
	mov r8, qword ptr [rbp + -2120]
	mov r9, r8
	mov qword ptr [rbp + -2128], r9
	mov r8, qword ptr [rbp + -2104]
	mov r9, qword ptr [rbp + -2128]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2136], r8
	mov r8, qword ptr [rbp + -2136]
	mov r9, r8
	mov qword ptr [rbp + -2144], r9
	mov r8, 8
	mov qword ptr [rbp + -2152], r8
	mov r8, 8
	mov qword ptr [rbp + -2160], r8
	mov r8, qword ptr [rbp + -2144]
	mov r9, r8
	mov qword ptr [rbp + -2168], r9
	mov r8, qword ptr [rbp + -2168]
	mov r9, qword ptr [rbp + -2160]
	imul r8, r9
	mov qword ptr [rbp + -2168], r8
	mov r8, qword ptr [rbp + -2152]
	mov r9, qword ptr [rbp + -2168]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2176], r8
	mov r8, qword ptr [rbp + -2176]
	mov r9, r8
	mov qword ptr [rbp + -2184], r9
	mov r8, qword ptr [rbp + -2184]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -2192], r9
	mov r8, qword ptr [rbp + -2192]
	mov r9, r8
	mov qword ptr [rbp + -2200], r9
	mov r8, qword ptr [rbp + -2200]
	mov r9, r8
	mov qword ptr [rbp + -2208], r9
	mov r8, qword ptr [rbp + -2208]
	mov r9, qword ptr [rbp + -2144]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -2216], r8
	mov r8, qword ptr [rbp + -2208]
	mov r9, qword ptr [rbp + -2216]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2224], r8
	mov r8, qword ptr [rbp + -2224]
	mov r9, r8
	mov qword ptr [rbp + -2232], r9
	mov r8, 0
	mov qword ptr [rbp + -2240], r8
	mov r8, qword ptr [rbp + -2240]
	mov r9, r8
	mov qword ptr [rbp + -2248], r9
	_l77:
	mov r8, qword ptr [rbp + -2248]
	mov r9, qword ptr [rbp + -2104]
	cmp r8, r9
	jge _l75
	_l76:
	mov r8, 8
	mov qword ptr [rbp + -2256], r8
	mov r8, qword ptr [rbp + -2248]
	mov r9, r8
	mov qword ptr [rbp + -2264], r9
	mov r8, qword ptr [rbp + -2264]
	mov r9, qword ptr [rbp + -2256]
	imul r8, r9
	mov qword ptr [rbp + -2264], r8
	mov r8, qword ptr [rbp + -2232]
	mov r9, qword ptr [rbp + -2264]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2272], r8
	mov r8, qword ptr [rbp + -2272]
	mov r9, r8
	mov qword ptr [rbp + -2280], r9
	mov r8, 8
	mov qword ptr [rbp + -2288], r8
	mov r8, qword ptr [rbp + -2248]
	mov r9, r8
	mov qword ptr [rbp + -2296], r9
	mov r8, qword ptr [rbp + -2296]
	mov r9, qword ptr [rbp + -2288]
	imul r8, r9
	mov qword ptr [rbp + -2296], r8
	mov r8, qword ptr [rbp + -2048]
	mov r9, qword ptr [rbp + -2296]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2304], r8
	mov r8, qword ptr [rbp + -2304]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2304], r8
	mov r8, qword ptr [rbp + -2280]
	mov r9, qword ptr [rbp + -2304]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2312], r8
	mov r8, qword ptr [rbp + -2248]
	mov r9, qword ptr [rbp + -2312]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2320], r8
	mov r8, qword ptr [rbp + -2320]
	mov r9, r8
	mov qword ptr [rbp + -2248], r9
	jmp _l77
	_l75:
	mov r8, 8
	mov qword ptr [rbp + -2328], r8
	mov r8, qword ptr [rbp + -2104]
	mov r9, r8
	mov qword ptr [rbp + -2336], r9
	mov r8, qword ptr [rbp + -2336]
	mov r9, qword ptr [rbp + -2328]
	imul r8, r9
	mov qword ptr [rbp + -2336], r8
	mov r8, qword ptr [rbp + -2232]
	mov r9, qword ptr [rbp + -2336]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2344], r8
	mov r8, qword ptr [rbp + -2344]
	mov r9, r8
	mov qword ptr [rbp + -2352], r9
	mov r8, 0
	mov qword ptr [rbp + -2360], r8
	mov r8, qword ptr [rbp + -2360]
	mov r9, r8
	mov qword ptr [rbp + -2368], r9
	_l74:
	mov r8, qword ptr [rbp + -2368]
	mov r9, qword ptr [rbp + -2128]
	cmp r8, r9
	jge _l72
	_l73:
	mov r8, 8
	mov qword ptr [rbp + -2376], r8
	mov r8, qword ptr [rbp + -2368]
	mov r9, r8
	mov qword ptr [rbp + -2384], r9
	mov r8, qword ptr [rbp + -2384]
	mov r9, qword ptr [rbp + -2376]
	imul r8, r9
	mov qword ptr [rbp + -2384], r8
	mov r8, qword ptr [rbp + -2352]
	mov r9, qword ptr [rbp + -2384]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2392], r8
	mov r8, qword ptr [rbp + -2392]
	mov r9, r8
	mov qword ptr [rbp + -2400], r9
	mov r8, 8
	mov qword ptr [rbp + -2408], r8
	mov r8, qword ptr [rbp + -2368]
	mov r9, r8
	mov qword ptr [rbp + -2416], r9
	mov r8, qword ptr [rbp + -2416]
	mov r9, qword ptr [rbp + -2408]
	imul r8, r9
	mov qword ptr [rbp + -2416], r8
	mov r8, qword ptr [rbp + -2080]
	mov r9, qword ptr [rbp + -2416]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2424], r8
	mov r8, qword ptr [rbp + -2424]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2424], r8
	mov r8, qword ptr [rbp + -2400]
	mov r9, qword ptr [rbp + -2424]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2432], r8
	mov r8, qword ptr [rbp + -2368]
	mov r9, qword ptr [rbp + -2432]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2440], r8
	mov r8, qword ptr [rbp + -2440]
	mov r9, r8
	mov qword ptr [rbp + -2368], r9
	jmp _l74
	_l72:
	mov r8, qword ptr [rbp + -2232]
	mov r9, r8
	mov qword ptr [rbp + -2448], r9
	mov r8, qword ptr [rbp + -2448]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g13[rip]
	mov qword ptr [rbp + -2456], r8
	mov r8, qword ptr [rbp + -2456]
	mov r9, r8
	mov qword ptr [rbp + -2464], r9
	mov r8, 8
	mov qword ptr [rbp + -2472], r8
	mov r8, qword ptr [rbp + -2464]
	mov r9, qword ptr [rbp + -2472]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2480], r8
	mov r8, qword ptr [rbp + -2480]
	mov r9, r8
	mov qword ptr [rbp + -2488], r9
	mov r8, qword ptr [rbp + -2488]
	mov r9, r8
	mov qword ptr [rbp + -2496], r9
	mov r8, qword ptr [rbp + -168]
	mov r9, r8
	mov qword ptr [rbp + -2504], r9
	mov r8, qword ptr [rbp + -2504]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -2512], r9
	mov r8, qword ptr [rbp + -2512]
	mov r9, r8
	mov qword ptr [rbp + -2520], r9
	mov r8, qword ptr [rbp + -2520]
	mov r9, r8
	mov qword ptr [rbp + -2528], r9
	mov r8, 8
	mov qword ptr [rbp + -2536], r8
	mov r8, qword ptr [rbp + -2496]
	mov r9, r8
	mov qword ptr [rbp + -2544], r9
	mov r8, qword ptr [rbp + -2544]
	mov r9, qword ptr [rbp + -2536]
	sub r8, r9
	mov qword ptr [rbp + -2544], r8
	mov r8, qword ptr [rbp + -2544]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2544], r8
	mov r8, qword ptr [rbp + -2544]
	mov r9, r8
	mov qword ptr [rbp + -2552], r9
	mov r8, 8
	mov qword ptr [rbp + -2560], r8
	mov r8, qword ptr [rbp + -2528]
	mov r9, r8
	mov qword ptr [rbp + -2568], r9
	mov r8, qword ptr [rbp + -2568]
	mov r9, qword ptr [rbp + -2560]
	sub r8, r9
	mov qword ptr [rbp + -2568], r8
	mov r8, qword ptr [rbp + -2568]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2568], r8
	mov r8, qword ptr [rbp + -2568]
	mov r9, r8
	mov qword ptr [rbp + -2576], r9
	mov r8, qword ptr [rbp + -2552]
	mov r9, qword ptr [rbp + -2576]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2584], r8
	mov r8, qword ptr [rbp + -2584]
	mov r9, r8
	mov qword ptr [rbp + -2592], r9
	mov r8, 8
	mov qword ptr [rbp + -2600], r8
	mov r8, 8
	mov qword ptr [rbp + -2608], r8
	mov r8, qword ptr [rbp + -2592]
	mov r9, r8
	mov qword ptr [rbp + -2616], r9
	mov r8, qword ptr [rbp + -2616]
	mov r9, qword ptr [rbp + -2608]
	imul r8, r9
	mov qword ptr [rbp + -2616], r8
	mov r8, qword ptr [rbp + -2600]
	mov r9, qword ptr [rbp + -2616]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2624], r8
	mov r8, qword ptr [rbp + -2624]
	mov r9, r8
	mov qword ptr [rbp + -2632], r9
	mov r8, qword ptr [rbp + -2632]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -2640], r9
	mov r8, qword ptr [rbp + -2640]
	mov r9, r8
	mov qword ptr [rbp + -2648], r9
	mov r8, qword ptr [rbp + -2648]
	mov r9, r8
	mov qword ptr [rbp + -2656], r9
	mov r8, qword ptr [rbp + -2656]
	mov r9, qword ptr [rbp + -2592]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -2664], r8
	mov r8, qword ptr [rbp + -2656]
	mov r9, qword ptr [rbp + -2664]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2672], r8
	mov r8, qword ptr [rbp + -2672]
	mov r9, r8
	mov qword ptr [rbp + -2680], r9
	mov r8, 0
	mov qword ptr [rbp + -2688], r8
	mov r8, qword ptr [rbp + -2688]
	mov r9, r8
	mov qword ptr [rbp + -2696], r9
	_l83:
	mov r8, qword ptr [rbp + -2696]
	mov r9, qword ptr [rbp + -2552]
	cmp r8, r9
	jge _l81
	_l82:
	mov r8, 8
	mov qword ptr [rbp + -2704], r8
	mov r8, qword ptr [rbp + -2696]
	mov r9, r8
	mov qword ptr [rbp + -2712], r9
	mov r8, qword ptr [rbp + -2712]
	mov r9, qword ptr [rbp + -2704]
	imul r8, r9
	mov qword ptr [rbp + -2712], r8
	mov r8, qword ptr [rbp + -2680]
	mov r9, qword ptr [rbp + -2712]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2720], r8
	mov r8, qword ptr [rbp + -2720]
	mov r9, r8
	mov qword ptr [rbp + -2728], r9
	mov r8, 8
	mov qword ptr [rbp + -2736], r8
	mov r8, qword ptr [rbp + -2696]
	mov r9, r8
	mov qword ptr [rbp + -2744], r9
	mov r8, qword ptr [rbp + -2744]
	mov r9, qword ptr [rbp + -2736]
	imul r8, r9
	mov qword ptr [rbp + -2744], r8
	mov r8, qword ptr [rbp + -2496]
	mov r9, qword ptr [rbp + -2744]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2752], r8
	mov r8, qword ptr [rbp + -2752]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2752], r8
	mov r8, qword ptr [rbp + -2728]
	mov r9, qword ptr [rbp + -2752]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2760], r8
	mov r8, qword ptr [rbp + -2696]
	mov r9, qword ptr [rbp + -2760]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2768], r8
	mov r8, qword ptr [rbp + -2768]
	mov r9, r8
	mov qword ptr [rbp + -2696], r9
	jmp _l83
	_l81:
	mov r8, 8
	mov qword ptr [rbp + -2776], r8
	mov r8, qword ptr [rbp + -2552]
	mov r9, r8
	mov qword ptr [rbp + -2784], r9
	mov r8, qword ptr [rbp + -2784]
	mov r9, qword ptr [rbp + -2776]
	imul r8, r9
	mov qword ptr [rbp + -2784], r8
	mov r8, qword ptr [rbp + -2680]
	mov r9, qword ptr [rbp + -2784]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2792], r8
	mov r8, qword ptr [rbp + -2792]
	mov r9, r8
	mov qword ptr [rbp + -2800], r9
	mov r8, 0
	mov qword ptr [rbp + -2808], r8
	mov r8, qword ptr [rbp + -2808]
	mov r9, r8
	mov qword ptr [rbp + -2816], r9
	_l80:
	mov r8, qword ptr [rbp + -2816]
	mov r9, qword ptr [rbp + -2576]
	cmp r8, r9
	jge _l78
	_l79:
	mov r8, 8
	mov qword ptr [rbp + -2824], r8
	mov r8, qword ptr [rbp + -2816]
	mov r9, r8
	mov qword ptr [rbp + -2832], r9
	mov r8, qword ptr [rbp + -2832]
	mov r9, qword ptr [rbp + -2824]
	imul r8, r9
	mov qword ptr [rbp + -2832], r8
	mov r8, qword ptr [rbp + -2800]
	mov r9, qword ptr [rbp + -2832]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2840], r8
	mov r8, qword ptr [rbp + -2840]
	mov r9, r8
	mov qword ptr [rbp + -2848], r9
	mov r8, 8
	mov qword ptr [rbp + -2856], r8
	mov r8, qword ptr [rbp + -2816]
	mov r9, r8
	mov qword ptr [rbp + -2864], r9
	mov r8, qword ptr [rbp + -2864]
	mov r9, qword ptr [rbp + -2856]
	imul r8, r9
	mov qword ptr [rbp + -2864], r8
	mov r8, qword ptr [rbp + -2528]
	mov r9, qword ptr [rbp + -2864]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2872], r8
	mov r8, qword ptr [rbp + -2872]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2872], r8
	mov r8, qword ptr [rbp + -2848]
	mov r9, qword ptr [rbp + -2872]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -2880], r8
	mov r8, qword ptr [rbp + -2816]
	mov r9, qword ptr [rbp + -2880]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2888], r8
	mov r8, qword ptr [rbp + -2888]
	mov r9, r8
	mov qword ptr [rbp + -2816], r9
	jmp _l80
	_l78:
	mov r8, qword ptr [rbp + -2680]
	mov r9, r8
	mov qword ptr [rbp + -2896], r9
	mov r8, qword ptr [rbp + -2896]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g14[rip]
	mov qword ptr [rbp + -2904], r8
	mov r8, qword ptr [rbp + -2904]
	mov r9, r8
	mov qword ptr [rbp + -2912], r9
	mov r8, 8
	mov qword ptr [rbp + -2920], r8
	mov r8, qword ptr [rbp + -2912]
	mov r9, qword ptr [rbp + -2920]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -2928], r8
	mov r8, qword ptr [rbp + -2928]
	mov r9, r8
	mov qword ptr [rbp + -2936], r9
	mov r8, qword ptr [rbp + -2936]
	mov r9, r8
	mov qword ptr [rbp + -2944], r9
	mov r8, qword ptr [rbp + -184]
	mov r9, r8
	mov qword ptr [rbp + -2952], r9
	mov r8, qword ptr [rbp + -2952]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -2960], r9
	mov r8, qword ptr [rbp + -2960]
	mov r9, r8
	mov qword ptr [rbp + -2968], r9
	mov r8, qword ptr [rbp + -2968]
	mov r9, r8
	mov qword ptr [rbp + -2976], r9
	mov r8, 8
	mov qword ptr [rbp + -2984], r8
	mov r8, qword ptr [rbp + -2944]
	mov r9, r8
	mov qword ptr [rbp + -2992], r9
	mov r8, qword ptr [rbp + -2992]
	mov r9, qword ptr [rbp + -2984]
	sub r8, r9
	mov qword ptr [rbp + -2992], r8
	mov r8, qword ptr [rbp + -2992]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -2992], r8
	mov r8, qword ptr [rbp + -2992]
	mov r9, r8
	mov qword ptr [rbp + -3000], r9
	mov r8, 8
	mov qword ptr [rbp + -3008], r8
	mov r8, qword ptr [rbp + -2976]
	mov r9, r8
	mov qword ptr [rbp + -3016], r9
	mov r8, qword ptr [rbp + -3016]
	mov r9, qword ptr [rbp + -3008]
	sub r8, r9
	mov qword ptr [rbp + -3016], r8
	mov r8, qword ptr [rbp + -3016]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3016], r8
	mov r8, qword ptr [rbp + -3016]
	mov r9, r8
	mov qword ptr [rbp + -3024], r9
	mov r8, qword ptr [rbp + -3000]
	mov r9, qword ptr [rbp + -3024]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3032], r8
	mov r8, qword ptr [rbp + -3032]
	mov r9, r8
	mov qword ptr [rbp + -3040], r9
	mov r8, 8
	mov qword ptr [rbp + -3048], r8
	mov r8, 8
	mov qword ptr [rbp + -3056], r8
	mov r8, qword ptr [rbp + -3040]
	mov r9, r8
	mov qword ptr [rbp + -3064], r9
	mov r8, qword ptr [rbp + -3064]
	mov r9, qword ptr [rbp + -3056]
	imul r8, r9
	mov qword ptr [rbp + -3064], r8
	mov r8, qword ptr [rbp + -3048]
	mov r9, qword ptr [rbp + -3064]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3072], r8
	mov r8, qword ptr [rbp + -3072]
	mov r9, r8
	mov qword ptr [rbp + -3080], r9
	mov r8, qword ptr [rbp + -3080]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -3088], r9
	mov r8, qword ptr [rbp + -3088]
	mov r9, r8
	mov qword ptr [rbp + -3096], r9
	mov r8, qword ptr [rbp + -3096]
	mov r9, r8
	mov qword ptr [rbp + -3104], r9
	mov r8, qword ptr [rbp + -3104]
	mov r9, qword ptr [rbp + -3040]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -3112], r8
	mov r8, qword ptr [rbp + -3104]
	mov r9, qword ptr [rbp + -3112]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3120], r8
	mov r8, qword ptr [rbp + -3120]
	mov r9, r8
	mov qword ptr [rbp + -3128], r9
	mov r8, 0
	mov qword ptr [rbp + -3136], r8
	mov r8, qword ptr [rbp + -3136]
	mov r9, r8
	mov qword ptr [rbp + -3144], r9
	_l89:
	mov r8, qword ptr [rbp + -3144]
	mov r9, qword ptr [rbp + -3000]
	cmp r8, r9
	jge _l87
	_l88:
	mov r8, 8
	mov qword ptr [rbp + -3152], r8
	mov r8, qword ptr [rbp + -3144]
	mov r9, r8
	mov qword ptr [rbp + -3160], r9
	mov r8, qword ptr [rbp + -3160]
	mov r9, qword ptr [rbp + -3152]
	imul r8, r9
	mov qword ptr [rbp + -3160], r8
	mov r8, qword ptr [rbp + -3128]
	mov r9, qword ptr [rbp + -3160]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3168], r8
	mov r8, qword ptr [rbp + -3168]
	mov r9, r8
	mov qword ptr [rbp + -3176], r9
	mov r8, 8
	mov qword ptr [rbp + -3184], r8
	mov r8, qword ptr [rbp + -3144]
	mov r9, r8
	mov qword ptr [rbp + -3192], r9
	mov r8, qword ptr [rbp + -3192]
	mov r9, qword ptr [rbp + -3184]
	imul r8, r9
	mov qword ptr [rbp + -3192], r8
	mov r8, qword ptr [rbp + -2944]
	mov r9, qword ptr [rbp + -3192]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3200], r8
	mov r8, qword ptr [rbp + -3200]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3200], r8
	mov r8, qword ptr [rbp + -3176]
	mov r9, qword ptr [rbp + -3200]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3208], r8
	mov r8, qword ptr [rbp + -3144]
	mov r9, qword ptr [rbp + -3208]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3216], r8
	mov r8, qword ptr [rbp + -3216]
	mov r9, r8
	mov qword ptr [rbp + -3144], r9
	jmp _l89
	_l87:
	mov r8, 8
	mov qword ptr [rbp + -3224], r8
	mov r8, qword ptr [rbp + -3000]
	mov r9, r8
	mov qword ptr [rbp + -3232], r9
	mov r8, qword ptr [rbp + -3232]
	mov r9, qword ptr [rbp + -3224]
	imul r8, r9
	mov qword ptr [rbp + -3232], r8
	mov r8, qword ptr [rbp + -3128]
	mov r9, qword ptr [rbp + -3232]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3240], r8
	mov r8, qword ptr [rbp + -3240]
	mov r9, r8
	mov qword ptr [rbp + -3248], r9
	mov r8, 0
	mov qword ptr [rbp + -3256], r8
	mov r8, qword ptr [rbp + -3256]
	mov r9, r8
	mov qword ptr [rbp + -3264], r9
	_l86:
	mov r8, qword ptr [rbp + -3264]
	mov r9, qword ptr [rbp + -3024]
	cmp r8, r9
	jge _l84
	_l85:
	mov r8, 8
	mov qword ptr [rbp + -3272], r8
	mov r8, qword ptr [rbp + -3264]
	mov r9, r8
	mov qword ptr [rbp + -3280], r9
	mov r8, qword ptr [rbp + -3280]
	mov r9, qword ptr [rbp + -3272]
	imul r8, r9
	mov qword ptr [rbp + -3280], r8
	mov r8, qword ptr [rbp + -3248]
	mov r9, qword ptr [rbp + -3280]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3288], r8
	mov r8, qword ptr [rbp + -3288]
	mov r9, r8
	mov qword ptr [rbp + -3296], r9
	mov r8, 8
	mov qword ptr [rbp + -3304], r8
	mov r8, qword ptr [rbp + -3264]
	mov r9, r8
	mov qword ptr [rbp + -3312], r9
	mov r8, qword ptr [rbp + -3312]
	mov r9, qword ptr [rbp + -3304]
	imul r8, r9
	mov qword ptr [rbp + -3312], r8
	mov r8, qword ptr [rbp + -2976]
	mov r9, qword ptr [rbp + -3312]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3320], r8
	mov r8, qword ptr [rbp + -3320]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3320], r8
	mov r8, qword ptr [rbp + -3296]
	mov r9, qword ptr [rbp + -3320]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3328], r8
	mov r8, qword ptr [rbp + -3264]
	mov r9, qword ptr [rbp + -3328]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3336], r8
	mov r8, qword ptr [rbp + -3336]
	mov r9, r8
	mov qword ptr [rbp + -3264], r9
	jmp _l86
	_l84:
	mov r8, qword ptr [rbp + -3128]
	mov r9, r8
	mov qword ptr [rbp + -3344], r9
	mov r8, qword ptr [rbp + -3344]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	lea r8, qword ptr _g15[rip]
	mov qword ptr [rbp + -3352], r8
	mov r8, qword ptr [rbp + -3352]
	mov r9, r8
	mov qword ptr [rbp + -3360], r9
	mov r8, 8
	mov qword ptr [rbp + -3368], r8
	mov r8, qword ptr [rbp + -3360]
	mov r9, qword ptr [rbp + -3368]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3376], r8
	mov r8, qword ptr [rbp + -3376]
	mov r9, r8
	mov qword ptr [rbp + -3384], r9
	mov r8, qword ptr [rbp + -3384]
	mov r9, r8
	mov qword ptr [rbp + -3392], r9
	mov r8, qword ptr [rbp + -200]
	mov r9, r8
	mov qword ptr [rbp + -3400], r9
	mov r8, qword ptr [rbp + -3400]
	mov rdi, r8
	and rsp, -16
	call _IunparseInt_aii
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -3408], r9
	mov r8, qword ptr [rbp + -3408]
	mov r9, r8
	mov qword ptr [rbp + -3416], r9
	mov r8, qword ptr [rbp + -3416]
	mov r9, r8
	mov qword ptr [rbp + -3424], r9
	mov r8, 8
	mov qword ptr [rbp + -3432], r8
	mov r8, qword ptr [rbp + -3392]
	mov r9, r8
	mov qword ptr [rbp + -3440], r9
	mov r8, qword ptr [rbp + -3440]
	mov r9, qword ptr [rbp + -3432]
	sub r8, r9
	mov qword ptr [rbp + -3440], r8
	mov r8, qword ptr [rbp + -3440]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3440], r8
	mov r8, qword ptr [rbp + -3440]
	mov r9, r8
	mov qword ptr [rbp + -3448], r9
	mov r8, 8
	mov qword ptr [rbp + -3456], r8
	mov r8, qword ptr [rbp + -3424]
	mov r9, r8
	mov qword ptr [rbp + -3464], r9
	mov r8, qword ptr [rbp + -3464]
	mov r9, qword ptr [rbp + -3456]
	sub r8, r9
	mov qword ptr [rbp + -3464], r8
	mov r8, qword ptr [rbp + -3464]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3464], r8
	mov r8, qword ptr [rbp + -3464]
	mov r9, r8
	mov qword ptr [rbp + -3472], r9
	mov r8, qword ptr [rbp + -3448]
	mov r9, qword ptr [rbp + -3472]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3480], r8
	mov r8, qword ptr [rbp + -3480]
	mov r9, r8
	mov qword ptr [rbp + -3488], r9
	mov r8, 8
	mov qword ptr [rbp + -3496], r8
	mov r8, 8
	mov qword ptr [rbp + -3504], r8
	mov r8, qword ptr [rbp + -3488]
	mov r9, r8
	mov qword ptr [rbp + -3512], r9
	mov r8, qword ptr [rbp + -3512]
	mov r9, qword ptr [rbp + -3504]
	imul r8, r9
	mov qword ptr [rbp + -3512], r8
	mov r8, qword ptr [rbp + -3496]
	mov r9, qword ptr [rbp + -3512]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3520], r8
	mov r8, qword ptr [rbp + -3520]
	mov r9, r8
	mov qword ptr [rbp + -3528], r9
	mov r8, qword ptr [rbp + -3528]
	mov rdi, r8
	and rsp, -16
	call _xi_alloc
	mov r8, rax
	mov qword ptr [rbp + -264], r8
	mov r8, qword ptr [rbp + -264]
	mov r9, r8
	mov qword ptr [rbp + -3536], r9
	mov r8, qword ptr [rbp + -3536]
	mov r9, r8
	mov qword ptr [rbp + -3544], r9
	mov r8, qword ptr [rbp + -3544]
	mov r9, r8
	mov qword ptr [rbp + -3552], r9
	mov r8, qword ptr [rbp + -3552]
	mov r9, qword ptr [rbp + -3488]
	mov qword ptr [r8], r9
	mov r8, 8
	mov qword ptr [rbp + -3560], r8
	mov r8, qword ptr [rbp + -3552]
	mov r9, qword ptr [rbp + -3560]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3568], r8
	mov r8, qword ptr [rbp + -3568]
	mov r9, r8
	mov qword ptr [rbp + -3576], r9
	mov r8, 0
	mov qword ptr [rbp + -3584], r8
	mov r8, qword ptr [rbp + -3584]
	mov r9, r8
	mov qword ptr [rbp + -3592], r9
	_l95:
	mov r8, qword ptr [rbp + -3592]
	mov r9, qword ptr [rbp + -3448]
	cmp r8, r9
	jge _l93
	_l94:
	mov r8, 8
	mov qword ptr [rbp + -3600], r8
	mov r8, qword ptr [rbp + -3592]
	mov r9, r8
	mov qword ptr [rbp + -3608], r9
	mov r8, qword ptr [rbp + -3608]
	mov r9, qword ptr [rbp + -3600]
	imul r8, r9
	mov qword ptr [rbp + -3608], r8
	mov r8, qword ptr [rbp + -3576]
	mov r9, qword ptr [rbp + -3608]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3616], r8
	mov r8, qword ptr [rbp + -3616]
	mov r9, r8
	mov qword ptr [rbp + -3624], r9
	mov r8, 8
	mov qword ptr [rbp + -3632], r8
	mov r8, qword ptr [rbp + -3592]
	mov r9, r8
	mov qword ptr [rbp + -3640], r9
	mov r8, qword ptr [rbp + -3640]
	mov r9, qword ptr [rbp + -3632]
	imul r8, r9
	mov qword ptr [rbp + -3640], r8
	mov r8, qword ptr [rbp + -3392]
	mov r9, qword ptr [rbp + -3640]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3648], r8
	mov r8, qword ptr [rbp + -3648]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3648], r8
	mov r8, qword ptr [rbp + -3624]
	mov r9, qword ptr [rbp + -3648]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3656], r8
	mov r8, qword ptr [rbp + -3592]
	mov r9, qword ptr [rbp + -3656]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3664], r8
	mov r8, qword ptr [rbp + -3664]
	mov r9, r8
	mov qword ptr [rbp + -3592], r9
	jmp _l95
	_l93:
	mov r8, 8
	mov qword ptr [rbp + -3672], r8
	mov r8, qword ptr [rbp + -3448]
	mov r9, r8
	mov qword ptr [rbp + -3680], r9
	mov r8, qword ptr [rbp + -3680]
	mov r9, qword ptr [rbp + -3672]
	imul r8, r9
	mov qword ptr [rbp + -3680], r8
	mov r8, qword ptr [rbp + -3576]
	mov r9, qword ptr [rbp + -3680]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3688], r8
	mov r8, qword ptr [rbp + -3688]
	mov r9, r8
	mov qword ptr [rbp + -3696], r9
	mov r8, 0
	mov qword ptr [rbp + -3704], r8
	mov r8, qword ptr [rbp + -3704]
	mov r9, r8
	mov qword ptr [rbp + -3712], r9
	_l92:
	mov r8, qword ptr [rbp + -3712]
	mov r9, qword ptr [rbp + -3472]
	cmp r8, r9
	jge _l90
	_l91:
	mov r8, 8
	mov qword ptr [rbp + -3720], r8
	mov r8, qword ptr [rbp + -3712]
	mov r9, r8
	mov qword ptr [rbp + -3728], r9
	mov r8, qword ptr [rbp + -3728]
	mov r9, qword ptr [rbp + -3720]
	imul r8, r9
	mov qword ptr [rbp + -3728], r8
	mov r8, qword ptr [rbp + -3696]
	mov r9, qword ptr [rbp + -3728]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3736], r8
	mov r8, qword ptr [rbp + -3736]
	mov r9, r8
	mov qword ptr [rbp + -3744], r9
	mov r8, 8
	mov qword ptr [rbp + -3752], r8
	mov r8, qword ptr [rbp + -3712]
	mov r9, r8
	mov qword ptr [rbp + -3760], r9
	mov r8, qword ptr [rbp + -3760]
	mov r9, qword ptr [rbp + -3752]
	imul r8, r9
	mov qword ptr [rbp + -3760], r8
	mov r8, qword ptr [rbp + -3424]
	mov r9, qword ptr [rbp + -3760]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3768], r8
	mov r8, qword ptr [rbp + -3768]
	mov r8, qword ptr [r8]
	mov qword ptr [rbp + -3768], r8
	mov r8, qword ptr [rbp + -3744]
	mov r9, qword ptr [rbp + -3768]
	mov qword ptr [r8], r9
	mov r8, 1
	mov qword ptr [rbp + -3776], r8
	mov r8, qword ptr [rbp + -3712]
	mov r9, qword ptr [rbp + -3776]
	lea r8, qword ptr [r8 + r9]
	mov qword ptr [rbp + -3784], r8
	mov r8, qword ptr [rbp + -3784]
	mov r9, r8
	mov qword ptr [rbp + -3712], r9
	jmp _l92
	_l90:
	mov r8, qword ptr [rbp + -3576]
	mov r9, r8
	mov qword ptr [rbp + -3792], r9
	mov r8, qword ptr [rbp + -3792]
	mov rdi, r8
	and rsp, -16
	call _Iprintln_pai
	mov r8, 22
	mov qword ptr [rbp + -3800], r8
	mov r8, qword ptr [rbp + -3800]
	mov r9, r8
	mov qword ptr [rbp + -3808], r9
	mov r8, 24
	mov qword ptr [rbp + -3816], r8
	mov r8, qword ptr [rbp + -3816]
	mov r9, r8
	mov qword ptr [rbp + -3824], r9
	mov r8, 3
	mov qword ptr [rbp + -3832], r8
	mov r8, qword ptr [rbp + -3832]
	mov r9, r8
	mov qword ptr [rbp + -3840], r9
	mov r8, 4
	mov qword ptr [rbp + -3848], r8
	mov r8, qword ptr [rbp + -3848]
	mov r9, r8
	mov qword ptr [rbp + -3856], r9
	mov r8, 5
	mov qword ptr [rbp + -3864], r8
	mov r8, qword ptr [rbp + -3864]
	mov r9, r8
	mov qword ptr [rbp + -3872], r9
	mov r8, 6
	mov qword ptr [rbp + -3880], r8
	mov r8, qword ptr [rbp + -3880]
	mov r9, r8
	mov qword ptr [rbp + -3888], r9
	mov r8, 7
	mov qword ptr [rbp + -3896], r8
	mov r8, qword ptr [rbp + -3896]
	mov r9, r8
	mov qword ptr [rbp + -3904], r9
	mov r8, 28
	mov qword ptr [rbp + -3912], r8
	mov r8, qword ptr [rbp + -3912]
	mov r9, r8
	mov qword ptr [rbp + -3920], r9
	mov r8, qword ptr [rbp + -3808]
	mov rax, r8
	mov r8, qword ptr [rbp + -3824]
	mov rdx, r8
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -3840]
	mov qword ptr [r8 + 0], r9
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -3856]
	mov qword ptr [r8 + 8], r9
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -3872]
	mov qword ptr [r8 + 16], r9
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -3888]
	mov qword ptr [r8 + 24], r9
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -3904]
	mov qword ptr [r8 + 32], r9
	mov r8, qword ptr [rbp + -8]
	mov r9, qword ptr [rbp + -3920]
	mov qword ptr [r8 + 40], r9
	leave
	ret
