	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 11, 0	sdk_version 11, 3
	.intel_syntax noprefix
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset rbp, -16
	mov	rbp, rsp
	.cfi_def_cfa_register rbp
	xor	eax, eax
	mov	dword ptr [rbp - 4], 0
	lea	rcx, [rip + _x]
	mov	qword ptr [rbp - 16], rcx
	pop	rbp
	ret
	.cfi_endproc
                                        ## -- End function
	.section	__DATA,__data
	.globl	_x                              ## @x
	.p2align	4
_x:
	.quad	1                               ## 0x1
	.quad	2                               ## 0x2
	.quad	3                               ## 0x3
	.quad	4                               ## 0x4

.subsections_via_symbols
