	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0	sdk_version 11, 3
	.intel_syntax noprefix
	.globl	_Ifactorial                     ## -- Begin function Ifactorial
	.p2align	4, 0x90
_Ifactorial:                            ## @Ifactorial
## %bb.0:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 32
	mov	qword ptr [rbp - 16], rdi
	cmp	qword ptr [rbp - 16], 1
	jg	LBB0_2
## %bb.1:
	mov	qword ptr [rbp - 8], 1
	jmp	LBB0_3
LBB0_2:
	mov	rax, qword ptr [rbp - 16]
	mov	rcx, qword ptr [rbp - 16]
	sub	rcx, 1
	mov	rdi, rcx
	mov	qword ptr [rbp - 24], rax       ## 8-byte Spill
	call	_Ifactorial
	mov	rcx, qword ptr [rbp - 24]       ## 8-byte Reload
	imul	rcx, rax
	mov	qword ptr [rbp - 8], rcx
LBB0_3:
	mov	rax, qword ptr [rbp - 8]
	add	rsp, 32
	pop	rbp
	ret
                                        ## -- End function
	.globl	_Imain_paai                     ## -- Begin function Imain_paai
	.p2align	4, 0x90
_Imain_paai:                            ## @Imain_paai
## %bb.0:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 48
	mov	qword ptr [rbp - 8], rdi
LBB1_1:                                 ## =>This Inner Loop Header: Depth=1
	call	_Ieof_b
	cmp	rax, 0
	setne	cl
	xor	cl, -1
	test	cl, 1
	jne	LBB1_2
	jmp	LBB1_5
LBB1_2:                                 ##   in Loop: Header=BB1_1 Depth=1
	lea	rax, [rip + _prompt]
	add	rax, 8
	mov	rdi, rax
	call	_Iprint_pai
	call	_Ireadln_ai
	mov	qword ptr [rbp - 16], rax
	mov	rdi, qword ptr [rbp - 16]
	call	_IparseInt_t2ibai
	mov	qword ptr [rbp - 40], rax
	mov	rax, qword ptr [rbp - 32]
	mov	rax, rdx
	mov	qword ptr [rbp - 32], rax
	cmp	qword ptr [rbp - 32], 0
	je	LBB1_4
## %bb.3:                               ##   in Loop: Header=BB1_1 Depth=1
	mov	rdi, qword ptr [rbp - 40]
	call	_Ifactorial
	mov	qword ptr [rbp - 48], rax
	mov	rdi, qword ptr [rbp - 40]
	call	_IunparseInt_aii
	lea	rcx, [rip + _is]
	add	rcx, 8
	mov	qword ptr [rbp - 24], rax
	mov	rdi, rcx
	call	_Iprint_pai
	mov	rdi, qword ptr [rbp - 48]
	call	_IunparseInt_aii
	mov	qword ptr [rbp - 24], rax
	mov	rdi, qword ptr [rbp - 24]
	call	_Iprintln_pai
LBB1_4:                                 ##   in Loop: Header=BB1_1 Depth=1
	jmp	LBB1_1
LBB1_5:
	add	rsp, 48
	pop	rbp
	ret
                                        ## -- End function
	.section	__DATA,__data
	.globl	_prompt                         ## @prompt
	.p2align	4
_prompt:
	.quad	7                               ## 0x7
	.quad	78                              ## 0x4e
	.quad	117                             ## 0x75
	.quad	109                             ## 0x6d
	.quad	98                              ## 0x62
	.quad	101                             ## 0x65
	.quad	114                             ## 0x72
	.quad	63                              ## 0x3f

	.globl	_is                             ## @is
	.p2align	4
_is:
	.quad	5                               ## 0x5
	.quad	33                              ## 0x21
	.quad	32                              ## 0x20
	.quad	105                             ## 0x69
	.quad	115                             ## 0x73
	.quad	32                              ## 0x20

.subsections_via_symbols
