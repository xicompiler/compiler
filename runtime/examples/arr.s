	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0	sdk_version 11, 3
	.intel_syntax noprefix
	.globl	_Imain_paai                     ## -- Begin function Imain_paai
	.p2align	4, 0x90
_Imain_paai:                            ## @Imain_paai
## %bb.0:
	push	rbp
	mov	rbp, rsp
	sub	rsp, 32
	mov	qword ptr [rbp - 8], rdi
	mov	rax, qword ptr [rbp - 8]
	mov	rax, qword ptr [rax - 8]
	mov	qword ptr [rbp - 16], rax
	mov	qword ptr [rbp - 24], 0
LBB0_1:                                 ## =>This Inner Loop Header: Depth=1
	mov	rax, qword ptr [rbp - 24]
	cmp	rax, qword ptr [rbp - 16]
	jge	LBB0_4
## %bb.2:                               ##   in Loop: Header=BB0_1 Depth=1
	mov	rax, qword ptr [rbp - 8]
	mov	rcx, qword ptr [rbp - 24]
	mov	rdi, qword ptr [rax + 8*rcx]
	call	_Iprintln_pai
## %bb.3:                               ##   in Loop: Header=BB0_1 Depth=1
	mov	rax, qword ptr [rbp - 24]
	add	rax, 1
	mov	qword ptr [rbp - 24], rax
	jmp	LBB0_1
LBB0_4:
	add	rsp, 32
	pop	rbp
	ret
                                        ## -- End function
.subsections_via_symbols
