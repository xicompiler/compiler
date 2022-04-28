.intel_syntax noprefix
.data
.globl _ItoLower_ii, _ImakeInverse_paiai, _ImkMatrix_aaii, _ImakeRotor_t3aaiaaiiai, _IrotorEncryptForward_iaaiaaiii, _IrotorEncryptBack_iaaiaaiii, _ImakeReflector_aiai, _IreflectorEncrypt_iaii, _Imain_paai
.text
_ItoLower_ii:
	mov _ARG1, rdi
	mov _t205, _ARG1
	mov c, _t205
	mov _t206, 65
	cmp c, _t206
	jl _l0
	_l2:
	mov _t207, 90
	cmp c, _t207
	jg _l0
	_l1:
	mov _t208, 65
	mov _t209, c
	sub _t209, _t208
	lea _t210, qword ptr [_t209 + 97]
	mov _t99, _t210
	mov rax, _t99
	leave
	ret
	_l0:
	mov _t100, c
	mov rax, _t100
	leave
	ret
_ImakeInverse_paiai:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t212, _ARG1
	mov base, _t212
	mov _t213, _ARG2
	mov inv, _t213
	mov _t214, 0
	mov c, _t214
	_l5:
	mov _t215, 26
	cmp c, _t215
	jge _l3
	_l4:
	mov _t1, inv
	mov _t3, base
	mov _t2, c
	mov _t216, 8
	mov _t217, _t3
	sub _t217, _t216
	mov _t218, qword ptr [_t217]
	cmp _t2, _t218
	setb _t219
	mov _t220, 1
	mov _t221, _t219
	xor _t221, _t220
	test _t221, _t221
	jnz _l8
	_l9:
	mov _t222, qword ptr [_t3 + _t2 * 8]
	mov _t0, _t222
	mov _t223, 8
	mov _t224, _t1
	sub _t224, _t223
	mov _t225, qword ptr [_t224]
	cmp _t0, _t225
	setb _t226
	mov _t227, 1
	mov _t228, _t226
	xor _t228, _t227
	test _t228, _t228
	jnz _l6
	_l7:
	lea _t229, qword ptr [_t1 + _t0 * 8]
	mov qword ptr [_t229], c
	lea _t230, qword ptr [c + 1]
	mov c, _t230
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
	mov _ARG1, rdi
	mov _t232, _ARG1
	mov n, _t232
	mov _t4, n
	mov _t5, n
	mov _t233, 8
	lea _t234, qword ptr [_t233 + _t4 * 8]
	mov _t102, _t234
	mov rdi, _t102
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t235, _RV1
	mov _t101, _t235
	mov _t7, _t101
	mov qword ptr [_t7], _t4
	lea _t236, qword ptr [_t7 + 8]
	mov _t6, _t236
	mov a, _t6
	mov _t237, 0
	mov _t9, _t237
	_l15:
	cmp _t9, _t4
	jge _l13
	_l14:
	mov _t238, 8
	lea _t239, qword ptr [_t238 + _t5 * 8]
	mov _t104, _t239
	mov rdi, _t104
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t240, _RV1
	mov _t103, _t240
	mov _t11, _t103
	mov qword ptr [_t11], _t5
	lea _t241, qword ptr [_t11 + 8]
	mov _t10, _t241
	mov _t8, _t10
	mov _t242, 0
	mov _t13, _t242
	_l12:
	cmp _t13, _t5
	jge _l10
	_l11:
	lea _t243, qword ptr [_t10 + _t13 * 8]
	mov qword ptr [_t243], _t12
	lea _t244, qword ptr [_t13 + 1]
	mov _t13, _t244
	jmp _l12
	_l10:
	lea _t245, qword ptr [_t6 + _t9 * 8]
	mov qword ptr [_t245], _t8
	lea _t246, qword ptr [_t9 + 1]
	mov _t9, _t246
	jmp _l15
	_l13:
	mov _t105, a
	mov rax, _t105
	leave
	ret
_ImakeRotor_t3aaiaaiiai:
	mov _t247, rdi
	mov _ARG1, rsi
	mov _t248, _ARG1
	mov sig, _t248
	mov _t249, 26
	mov _t107, _t249
	mov rdi, _t107
	and rsp, -16
	call _ImkMatrix_aaii
	mov _RV1, rax
	mov _t250, _RV1
	mov _t106, _t250
	mov forward, _t106
	mov _t251, 26
	mov _t109, _t251
	mov rdi, _t109
	and rsp, -16
	call _ImkMatrix_aaii
	mov _RV1, rax
	mov _t252, _RV1
	mov _t108, _t252
	mov backward, _t108
	mov _t253, 216
	mov _t111, _t253
	mov rdi, _t111
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t254, _RV1
	mov _t110, _t254
	mov _t15, _t110
	mov _t255, 26
	mov qword ptr [_t15], _t255
	lea _t256, qword ptr [_t15 + 8]
	mov _t14, _t256
	mov base, _t14
	mov _t257, 0
	mov _t17, _t257
	_l18:
	mov _t258, 26
	cmp _t17, _t258
	jge _l16
	_l17:
	lea _t259, qword ptr [_t14 + _t17 * 8]
	mov qword ptr [_t259], _t16
	lea _t260, qword ptr [_t17 + 1]
	mov _t17, _t260
	jmp _l18
	_l16:
	mov _t261, 0
	mov c, _t261
	_l21:
	mov _t262, 26
	cmp c, _t262
	jge _l19
	_l20:
	mov _t19, sig
	mov _t18, c
	mov _t263, 8
	mov _t264, _t19
	sub _t264, _t263
	mov _t265, qword ptr [_t264]
	cmp _t18, _t265
	setb _t266
	mov _t267, 1
	mov _t268, _t266
	xor _t268, _t267
	test _t268, _t268
	jnz _l22
	_l23:
	mov _t269, qword ptr [_t19 + _t18 * 8]
	mov _t113, _t269
	mov rdi, _t113
	and rsp, -16
	call _ItoLower_ii
	mov _RV1, rax
	mov _t270, _RV1
	mov _t112, _t270
	mov _t271, 97
	mov _t272, _t112
	sub _t272, _t271
	mov cd, _t272
	mov _t21, base
	mov _t20, c
	mov _t273, 8
	mov _t274, _t21
	sub _t274, _t273
	mov _t275, qword ptr [_t274]
	cmp _t20, _t275
	setb _t276
	mov _t277, 1
	mov _t278, _t276
	xor _t278, _t277
	test _t278, _t278
	jnz _l24
	_l25:
	lea _t279, qword ptr [_t21 + _t20 * 8]
	mov qword ptr [_t279], cd
	lea _t280, qword ptr [c + 1]
	mov c, _t280
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
	mov _t281, 216
	mov _t115, _t281
	mov rdi, _t115
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t282, _RV1
	mov _t114, _t282
	mov _t23, _t114
	mov _t283, 26
	mov qword ptr [_t23], _t283
	lea _t284, qword ptr [_t23 + 8]
	mov _t22, _t284
	mov inv, _t22
	mov _t285, 0
	mov _t25, _t285
	_l28:
	mov _t286, 26
	cmp _t25, _t286
	jge _l26
	_l27:
	lea _t287, qword ptr [_t22 + _t25 * 8]
	mov qword ptr [_t287], _t24
	lea _t288, qword ptr [_t25 + 1]
	mov _t25, _t288
	jmp _l28
	_l26:
	mov _t289, 0
	mov rot, _t289
	_l31:
	mov _t290, 26
	cmp rot, _t290
	jge _l29
	_l30:
	mov _t291, 0
	mov c, _t291
	_l34:
	mov _t292, 26
	cmp c, _t292
	jge _l32
	_l33:
	mov _t31, forward
	mov _t30, rot
	mov _t293, 8
	mov _t294, _t31
	sub _t294, _t293
	mov _t295, qword ptr [_t294]
	cmp _t30, _t295
	setb _t296
	mov _t297, 1
	mov _t298, _t296
	xor _t298, _t297
	test _t298, _t298
	jnz _l39
	_l40:
	mov _t299, qword ptr [_t31 + _t30 * 8]
	mov _t27, _t299
	mov _t26, c
	mov _t300, 8
	mov _t301, _t27
	sub _t301, _t300
	mov _t302, qword ptr [_t301]
	cmp _t26, _t302
	setb _t303
	mov _t304, 1
	mov _t305, _t303
	xor _t305, _t304
	test _t305, _t305
	jnz _l37
	_l38:
	lea _t306, qword ptr [_t27 + _t26 * 8]
	mov _t116, _t306
	mov _t29, base
	mov _t28, c
	mov _t307, 8
	mov _t308, _t29
	sub _t308, _t307
	mov _t309, qword ptr [_t308]
	cmp _t28, _t309
	setb _t310
	mov _t311, 1
	mov _t312, _t310
	xor _t312, _t311
	test _t312, _t312
	jnz _l35
	_l36:
	mov _t313, qword ptr [_t29 + _t28 * 8]
	mov qword ptr [_t116], _t313
	lea _t314, qword ptr [c + 1]
	mov c, _t314
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
	mov _t117, base
	mov _t118, inv
	mov rsi, _t118
	mov rdi, _t117
	and rsp, -16
	call _ImakeInverse_paiai
	mov _t315, 0
	mov c, _t315
	_l43:
	mov _t316, 26
	cmp c, _t316
	jge _l41
	_l42:
	mov _t37, backward
	mov _t36, rot
	mov _t317, 8
	mov _t318, _t37
	sub _t318, _t317
	mov _t319, qword ptr [_t318]
	cmp _t36, _t319
	setb _t320
	mov _t321, 1
	mov _t322, _t320
	xor _t322, _t321
	test _t322, _t322
	jnz _l48
	_l49:
	mov _t323, qword ptr [_t37 + _t36 * 8]
	mov _t33, _t323
	mov _t32, c
	mov _t324, 8
	mov _t325, _t33
	sub _t325, _t324
	mov _t326, qword ptr [_t325]
	cmp _t32, _t326
	setb _t327
	mov _t328, 1
	mov _t329, _t327
	xor _t329, _t328
	test _t329, _t329
	jnz _l46
	_l47:
	lea _t330, qword ptr [_t33 + _t32 * 8]
	mov _t119, _t330
	mov _t35, inv
	mov _t34, c
	mov _t331, 8
	mov _t332, _t35
	sub _t332, _t331
	mov _t333, qword ptr [_t332]
	cmp _t34, _t333
	setb _t334
	mov _t335, 1
	mov _t336, _t334
	xor _t336, _t335
	test _t336, _t336
	jnz _l44
	_l45:
	mov _t337, qword ptr [_t35 + _t34 * 8]
	mov qword ptr [_t119], _t337
	lea _t338, qword ptr [c + 1]
	mov c, _t338
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
	mov _t39, base
	mov _t339, 0
	mov _t38, _t339
	mov _t340, 8
	mov _t341, _t39
	sub _t341, _t340
	mov _t342, qword ptr [_t341]
	cmp _t38, _t342
	setb _t343
	mov _t344, 1
	mov _t345, _t343
	xor _t345, _t344
	test _t345, _t345
	jnz _l50
	_l51:
	mov _t346, 1
	mov _t347, qword ptr [_t39 + _t38 * 8]
	sub _t347, _t346
	lea _t348, qword ptr [_t347 + 26]
	mov _t349, 26
	mov rax, _t348
	xor rdx, rdx
	idiv _t349
	mov _t348, rdx
	mov first, _t348
	mov _t350, 1
	mov pos, _t350
	_l54:
	mov _t351, 26
	cmp pos, _t351
	jge _l52
	_l53:
	mov _t41, base
	mov _t352, 1
	mov _t353, pos
	sub _t353, _t352
	mov _t40, _t353
	mov _t354, 8
	mov _t355, _t41
	sub _t355, _t354
	mov _t356, qword ptr [_t355]
	cmp _t40, _t356
	setb _t357
	mov _t358, 1
	mov _t359, _t357
	xor _t359, _t358
	test _t359, _t359
	jnz _l57
	_l58:
	lea _t360, qword ptr [_t41 + _t40 * 8]
	mov _t120, _t360
	mov _t43, base
	mov _t42, pos
	mov _t361, 8
	mov _t362, _t43
	sub _t362, _t361
	mov _t363, qword ptr [_t362]
	cmp _t42, _t363
	setb _t364
	mov _t365, 1
	mov _t366, _t364
	xor _t366, _t365
	test _t366, _t366
	jnz _l55
	_l56:
	mov _t367, 1
	mov _t368, qword ptr [_t43 + _t42 * 8]
	sub _t368, _t367
	lea _t369, qword ptr [_t368 + 26]
	mov _t370, 26
	mov rax, _t369
	xor rdx, rdx
	idiv _t370
	mov _t369, rdx
	mov qword ptr [_t120], _t369
	lea _t371, qword ptr [pos + 1]
	mov pos, _t371
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
	mov _t45, base
	mov _t372, 25
	mov _t44, _t372
	mov _t373, 8
	mov _t374, _t45
	sub _t374, _t373
	mov _t375, qword ptr [_t374]
	cmp _t44, _t375
	setb _t376
	mov _t377, 1
	mov _t378, _t376
	xor _t378, _t377
	test _t378, _t378
	jnz _l59
	_l60:
	lea _t379, qword ptr [_t45 + _t44 * 8]
	mov qword ptr [_t379], first
	lea _t380, qword ptr [rot + 1]
	mov rot, _t380
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
	mov _t121, forward
	mov _t122, backward
	mov _t381, 0
	mov _t123, _t381
	mov rax, _t121
	mov rdx, _t122
	mov qword ptr [_t247 + 0], _t123
	leave
	ret
_IrotorEncryptForward_iaaiaaiii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _ARG3, rdx
	mov _ARG4, rcx
	mov _t383, _ARG1
	mov forward, _t383
	mov _t384, _ARG2
	mov backward, _t384
	mov _t385, _ARG3
	mov pos, _t385
	mov _t386, _ARG4
	mov letter, _t386
	mov _t49, forward
	mov _t48, pos
	mov _t387, 8
	mov _t388, _t49
	sub _t388, _t387
	mov _t389, qword ptr [_t388]
	cmp _t48, _t389
	setb _t390
	mov _t391, 1
	mov _t392, _t390
	xor _t392, _t391
	test _t392, _t392
	jnz _l61
	_l62:
	mov _t393, qword ptr [_t49 + _t48 * 8]
	mov _t47, _t393
	mov _t46, letter
	mov _t394, 8
	mov _t395, _t47
	sub _t395, _t394
	mov _t396, qword ptr [_t395]
	cmp _t46, _t396
	setb _t397
	mov _t398, 1
	mov _t399, _t397
	xor _t399, _t398
	test _t399, _t399
	jnz _l63
	_l64:
	mov _t400, qword ptr [_t47 + _t46 * 8]
	mov _t124, _t400
	mov rax, _t124
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
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _ARG3, rdx
	mov _ARG4, rcx
	mov _t402, _ARG1
	mov forward, _t402
	mov _t403, _ARG2
	mov backward, _t403
	mov _t404, _ARG3
	mov pos, _t404
	mov _t405, _ARG4
	mov letter, _t405
	mov _t53, backward
	mov _t52, pos
	mov _t406, 8
	mov _t407, _t53
	sub _t407, _t406
	mov _t408, qword ptr [_t407]
	cmp _t52, _t408
	setb _t409
	mov _t410, 1
	mov _t411, _t409
	xor _t411, _t410
	test _t411, _t411
	jnz _l65
	_l66:
	mov _t412, qword ptr [_t53 + _t52 * 8]
	mov _t51, _t412
	mov _t50, letter
	mov _t413, 8
	mov _t414, _t51
	sub _t414, _t413
	mov _t415, qword ptr [_t414]
	cmp _t50, _t415
	setb _t416
	mov _t417, 1
	mov _t418, _t416
	xor _t418, _t417
	test _t418, _t418
	jnz _l67
	_l68:
	mov _t419, qword ptr [_t51 + _t50 * 8]
	mov _t125, _t419
	mov rax, _t125
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
	mov _ARG1, rdi
	mov _t421, _ARG1
	mov encoding, _t421
	mov _t422, 216
	mov _t127, _t422
	mov rdi, _t127
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t423, _RV1
	mov _t126, _t423
	mov _t55, _t126
	mov _t424, 26
	mov qword ptr [_t55], _t424
	lea _t425, qword ptr [_t55 + 8]
	mov _t54, _t425
	mov perm, _t54
	mov _t426, 0
	mov _t57, _t426
	_l71:
	mov _t427, 26
	cmp _t57, _t427
	jge _l69
	_l70:
	lea _t428, qword ptr [_t54 + _t57 * 8]
	mov qword ptr [_t428], _t56
	lea _t429, qword ptr [_t57 + 1]
	mov _t57, _t429
	jmp _l71
	_l69:
	mov _t430, 0
	mov c, _t430
	_l74:
	mov _t431, 26
	cmp c, _t431
	jge _l72
	_l73:
	mov _t59, encoding
	mov _t58, c
	mov _t432, 8
	mov _t433, _t59
	sub _t433, _t432
	mov _t434, qword ptr [_t433]
	cmp _t58, _t434
	setb _t435
	mov _t436, 1
	mov _t437, _t435
	xor _t437, _t436
	test _t437, _t437
	jnz _l75
	_l76:
	mov _t438, qword ptr [_t59 + _t58 * 8]
	mov _t129, _t438
	mov rdi, _t129
	and rsp, -16
	call _ItoLower_ii
	mov _RV1, rax
	mov _t439, _RV1
	mov _t128, _t439
	mov _t440, 97
	mov _t441, _t128
	sub _t441, _t440
	mov cd, _t441
	mov _t61, perm
	mov _t60, c
	mov _t442, 8
	mov _t443, _t61
	sub _t443, _t442
	mov _t444, qword ptr [_t443]
	cmp _t60, _t444
	setb _t445
	mov _t446, 1
	mov _t447, _t445
	xor _t447, _t446
	test _t447, _t447
	jnz _l77
	_l78:
	lea _t448, qword ptr [_t61 + _t60 * 8]
	mov qword ptr [_t448], cd
	lea _t449, qword ptr [c + 1]
	mov c, _t449
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
	mov _t130, perm
	mov rax, _t130
	leave
	ret
_IreflectorEncrypt_iaii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _t451, _ARG1
	mov perm, _t451
	mov _t452, _ARG2
	mov l, _t452
	mov _t63, perm
	mov _t62, l
	mov _t453, 8
	mov _t454, _t63
	sub _t454, _t453
	mov _t455, qword ptr [_t454]
	cmp _t62, _t455
	setb _t456
	mov _t457, 1
	mov _t458, _t456
	xor _t458, _t457
	test _t458, _t458
	jnz _l79
	_l80:
	mov _t459, qword ptr [_t63 + _t62 * 8]
	mov _t131, _t459
	mov rax, _t131
	leave
	ret
	_l79:
	and rsp, -16
	call _xi_out_of_bounds
	jmp _l80
_Imain_paai:
	mov _ARG1, rdi
	mov _t461, _ARG1
	mov a, _t461
	mov _t462, 40
	mov _t133, _t462
	mov rdi, _t133
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t463, _RV1
	mov _t132, _t463
	mov _t68, _t132
	mov _t464, 4
	mov qword ptr [_t68], _t464
	lea _t465, qword ptr [_t68 + 8]
	mov _t134, _t465
	mov _t466, 72
	mov _t136, _t466
	mov rdi, _t136
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t467, _RV1
	mov _t135, _t467
	mov _t64, _t135
	mov _t468, 8
	mov qword ptr [_t64], _t468
	lea _t469, qword ptr [_t64 + 8]
	mov _t470, 12
	mov qword ptr [_t469], _t470
	lea _t471, qword ptr [_t64 + 16]
	mov _t472, 27
	mov qword ptr [_t471], _t472
	lea _t473, qword ptr [_t64 + 24]
	mov _t474, 6
	mov qword ptr [_t473], _t474
	lea _t475, qword ptr [_t64 + 32]
	mov _t476, 57
	mov qword ptr [_t475], _t476
	lea _t477, qword ptr [_t64 + 40]
	mov _t478, 25
	mov qword ptr [_t477], _t478
	lea _t479, qword ptr [_t64 + 48]
	mov _t480, 51
	mov qword ptr [_t479], _t480
	lea _t481, qword ptr [_t64 + 56]
	mov _t482, 52
	mov qword ptr [_t481], _t482
	lea _t483, qword ptr [_t64 + 64]
	mov _t484, -1
	mov qword ptr [_t483], _t484
	lea _t485, qword ptr [_t64 + 8]
	mov qword ptr [_t134], _t485
	lea _t486, qword ptr [_t68 + 16]
	mov _t137, _t486
	mov _t487, 72
	mov _t139, _t487
	mov rdi, _t139
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t488, _RV1
	mov _t138, _t488
	mov _t65, _t138
	mov _t489, 8
	mov qword ptr [_t65], _t489
	lea _t490, qword ptr [_t65 + 8]
	mov _t491, 12
	mov qword ptr [_t490], _t491
	lea _t492, qword ptr [_t65 + 16]
	mov _t493, 27
	mov qword ptr [_t492], _t493
	lea _t494, qword ptr [_t65 + 24]
	mov _t495, 6
	mov qword ptr [_t494], _t495
	lea _t496, qword ptr [_t65 + 32]
	mov _t497, 55
	mov qword ptr [_t496], _t497
	lea _t498, qword ptr [_t65 + 40]
	mov _t499, 25
	mov qword ptr [_t498], _t499
	lea _t500, qword ptr [_t65 + 48]
	mov _t501, 51
	mov qword ptr [_t500], _t501
	lea _t502, qword ptr [_t65 + 56]
	mov _t503, 52
	mov qword ptr [_t502], _t503
	lea _t504, qword ptr [_t65 + 64]
	mov _t505, -1
	mov qword ptr [_t504], _t505
	lea _t506, qword ptr [_t65 + 8]
	mov qword ptr [_t137], _t506
	lea _t507, qword ptr [_t68 + 24]
	mov _t140, _t507
	mov _t508, 40
	mov _t142, _t508
	mov rdi, _t142
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t509, _RV1
	mov _t141, _t509
	mov _t66, _t141
	mov _t510, 4
	mov qword ptr [_t66], _t510
	lea _t511, qword ptr [_t66 + 8]
	mov _t512, 12
	mov qword ptr [_t511], _t512
	lea _t513, qword ptr [_t66 + 16]
	mov _t514, 46
	mov qword ptr [_t513], _t514
	lea _t515, qword ptr [_t66 + 24]
	mov _t516, 47
	mov qword ptr [_t515], _t516
	lea _t517, qword ptr [_t66 + 32]
	mov _t518, -1
	mov qword ptr [_t517], _t518
	lea _t519, qword ptr [_t66 + 8]
	mov qword ptr [_t140], _t519
	lea _t520, qword ptr [_t68 + 32]
	mov _t143, _t520
	mov _t521, 64
	mov _t145, _t521
	mov rdi, _t145
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t522, _RV1
	mov _t144, _t522
	mov _t67, _t144
	mov _t523, 7
	mov qword ptr [_t67], _t523
	lea _t524, qword ptr [_t67 + 8]
	mov _t525, 12
	mov qword ptr [_t524], _t525
	lea _t526, qword ptr [_t67 + 16]
	mov _t527, 27
	mov qword ptr [_t526], _t527
	lea _t528, qword ptr [_t67 + 24]
	mov _t529, 6
	mov qword ptr [_t528], _t529
	lea _t530, qword ptr [_t67 + 32]
	mov _t531, 16
	mov qword ptr [_t530], _t531
	lea _t532, qword ptr [_t67 + 40]
	mov _t533, 11
	mov qword ptr [_t532], _t533
	lea _t534, qword ptr [_t67 + 48]
	mov _t535, 52
	mov qword ptr [_t534], _t535
	lea _t536, qword ptr [_t67 + 56]
	mov _t537, -1
	mov qword ptr [_t536], _t537
	lea _t538, qword ptr [_t67 + 8]
	mov qword ptr [_t143], _t538
	lea _t539, qword ptr [_t68 + 8]
	mov loops, _t539
	mov _t540, 216
	mov _t148, _t540
	mov rdi, _t148
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t541, _RV1
	mov _t147, _t541
	mov _t69, _t147
	mov _t542, 26
	mov qword ptr [_t69], _t542
	lea _t543, qword ptr [_t69 + 8]
	mov _t544, 69
	mov qword ptr [_t543], _t544
	lea _t545, qword ptr [_t69 + 16]
	mov _t546, 75
	mov qword ptr [_t545], _t546
	lea _t547, qword ptr [_t69 + 24]
	mov _t548, 77
	mov qword ptr [_t547], _t548
	lea _t549, qword ptr [_t69 + 32]
	mov _t550, 70
	mov qword ptr [_t549], _t550
	lea _t551, qword ptr [_t69 + 40]
	mov _t552, 76
	mov qword ptr [_t551], _t552
	lea _t553, qword ptr [_t69 + 48]
	mov _t554, 71
	mov qword ptr [_t553], _t554
	lea _t555, qword ptr [_t69 + 56]
	mov _t556, 68
	mov qword ptr [_t555], _t556
	lea _t557, qword ptr [_t69 + 64]
	mov _t558, 81
	mov qword ptr [_t557], _t558
	lea _t559, qword ptr [_t69 + 72]
	mov _t560, 86
	mov qword ptr [_t559], _t560
	lea _t561, qword ptr [_t69 + 80]
	mov _t562, 90
	mov qword ptr [_t561], _t562
	lea _t563, qword ptr [_t69 + 88]
	mov _t564, 78
	mov qword ptr [_t563], _t564
	lea _t565, qword ptr [_t69 + 96]
	mov _t566, 84
	mov qword ptr [_t565], _t566
	lea _t567, qword ptr [_t69 + 104]
	mov _t568, 79
	mov qword ptr [_t567], _t568
	lea _t569, qword ptr [_t69 + 112]
	mov _t570, 87
	mov qword ptr [_t569], _t570
	lea _t571, qword ptr [_t69 + 120]
	mov _t572, 89
	mov qword ptr [_t571], _t572
	lea _t573, qword ptr [_t69 + 128]
	mov _t574, 72
	mov qword ptr [_t573], _t574
	lea _t575, qword ptr [_t69 + 136]
	mov _t576, 88
	mov qword ptr [_t575], _t576
	lea _t577, qword ptr [_t69 + 144]
	mov _t578, 85
	mov qword ptr [_t577], _t578
	lea _t579, qword ptr [_t69 + 152]
	mov _t580, 83
	mov qword ptr [_t579], _t580
	lea _t581, qword ptr [_t69 + 160]
	mov _t582, 80
	mov qword ptr [_t581], _t582
	lea _t583, qword ptr [_t69 + 168]
	mov _t584, 65
	mov qword ptr [_t583], _t584
	lea _t585, qword ptr [_t69 + 176]
	mov _t586, 73
	mov qword ptr [_t585], _t586
	lea _t587, qword ptr [_t69 + 184]
	mov _t588, 66
	mov qword ptr [_t587], _t588
	lea _t589, qword ptr [_t69 + 192]
	mov _t590, 82
	mov qword ptr [_t589], _t590
	lea _t591, qword ptr [_t69 + 200]
	mov _t592, 67
	mov qword ptr [_t591], _t592
	lea _t593, qword ptr [_t69 + 208]
	mov _t594, 74
	mov qword ptr [_t593], _t594
	lea _t595, qword ptr [_t69 + 8]
	mov _t146, _t595
	sub rsp, 8
	mov rdi, rsp
	mov rsi, _t146
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov _RV1, rax
	mov _RV2, rdx
	pop _RV3
	mov _t596, _RV1
	mov r1f, _t596
	mov _t597, _RV2
	mov r1b, _t597
	mov _t598, _RV3
	mov r1p, _t598
	mov _t599, 216
	mov _t151, _t599
	mov rdi, _t151
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t600, _RV1
	mov _t150, _t600
	mov _t70, _t150
	mov _t601, 26
	mov qword ptr [_t70], _t601
	lea _t602, qword ptr [_t70 + 8]
	mov _t603, 65
	mov qword ptr [_t602], _t603
	lea _t604, qword ptr [_t70 + 16]
	mov _t605, 74
	mov qword ptr [_t604], _t605
	lea _t606, qword ptr [_t70 + 24]
	mov _t607, 68
	mov qword ptr [_t606], _t607
	lea _t608, qword ptr [_t70 + 32]
	mov _t609, 75
	mov qword ptr [_t608], _t609
	lea _t610, qword ptr [_t70 + 40]
	mov _t611, 83
	mov qword ptr [_t610], _t611
	lea _t612, qword ptr [_t70 + 48]
	mov _t613, 73
	mov qword ptr [_t612], _t613
	lea _t614, qword ptr [_t70 + 56]
	mov _t615, 82
	mov qword ptr [_t614], _t615
	lea _t616, qword ptr [_t70 + 64]
	mov _t617, 85
	mov qword ptr [_t616], _t617
	lea _t618, qword ptr [_t70 + 72]
	mov _t619, 88
	mov qword ptr [_t618], _t619
	lea _t620, qword ptr [_t70 + 80]
	mov _t621, 66
	mov qword ptr [_t620], _t621
	lea _t622, qword ptr [_t70 + 88]
	mov _t623, 76
	mov qword ptr [_t622], _t623
	lea _t624, qword ptr [_t70 + 96]
	mov _t625, 72
	mov qword ptr [_t624], _t625
	lea _t626, qword ptr [_t70 + 104]
	mov _t627, 87
	mov qword ptr [_t626], _t627
	lea _t628, qword ptr [_t70 + 112]
	mov _t629, 84
	mov qword ptr [_t628], _t629
	lea _t630, qword ptr [_t70 + 120]
	mov _t631, 77
	mov qword ptr [_t630], _t631
	lea _t632, qword ptr [_t70 + 128]
	mov _t633, 67
	mov qword ptr [_t632], _t633
	lea _t634, qword ptr [_t70 + 136]
	mov _t635, 81
	mov qword ptr [_t634], _t635
	lea _t636, qword ptr [_t70 + 144]
	mov _t637, 71
	mov qword ptr [_t636], _t637
	lea _t638, qword ptr [_t70 + 152]
	mov _t639, 90
	mov qword ptr [_t638], _t639
	lea _t640, qword ptr [_t70 + 160]
	mov _t641, 78
	mov qword ptr [_t640], _t641
	lea _t642, qword ptr [_t70 + 168]
	mov _t643, 80
	mov qword ptr [_t642], _t643
	lea _t644, qword ptr [_t70 + 176]
	mov _t645, 89
	mov qword ptr [_t644], _t645
	lea _t646, qword ptr [_t70 + 184]
	mov _t647, 70
	mov qword ptr [_t646], _t647
	lea _t648, qword ptr [_t70 + 192]
	mov _t649, 86
	mov qword ptr [_t648], _t649
	lea _t650, qword ptr [_t70 + 200]
	mov _t651, 79
	mov qword ptr [_t650], _t651
	lea _t652, qword ptr [_t70 + 208]
	mov _t653, 69
	mov qword ptr [_t652], _t653
	lea _t654, qword ptr [_t70 + 8]
	mov _t149, _t654
	sub rsp, 8
	mov rdi, rsp
	mov rsi, _t149
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov _RV1, rax
	mov _RV2, rdx
	pop _RV3
	mov _t655, _RV1
	mov r2f, _t655
	mov _t656, _RV2
	mov r2b, _t656
	mov _t657, _RV3
	mov r2p, _t657
	mov _t658, 216
	mov _t154, _t658
	mov rdi, _t154
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t659, _RV1
	mov _t153, _t659
	mov _t71, _t153
	mov _t660, 26
	mov qword ptr [_t71], _t660
	lea _t661, qword ptr [_t71 + 8]
	mov _t662, 66
	mov qword ptr [_t661], _t662
	lea _t663, qword ptr [_t71 + 16]
	mov _t664, 68
	mov qword ptr [_t663], _t664
	lea _t665, qword ptr [_t71 + 24]
	mov _t666, 70
	mov qword ptr [_t665], _t666
	lea _t667, qword ptr [_t71 + 32]
	mov _t668, 72
	mov qword ptr [_t667], _t668
	lea _t669, qword ptr [_t71 + 40]
	mov _t670, 74
	mov qword ptr [_t669], _t670
	lea _t671, qword ptr [_t71 + 48]
	mov _t672, 76
	mov qword ptr [_t671], _t672
	lea _t673, qword ptr [_t71 + 56]
	mov _t674, 67
	mov qword ptr [_t673], _t674
	lea _t675, qword ptr [_t71 + 64]
	mov _t676, 80
	mov qword ptr [_t675], _t676
	lea _t677, qword ptr [_t71 + 72]
	mov _t678, 82
	mov qword ptr [_t677], _t678
	lea _t679, qword ptr [_t71 + 80]
	mov _t680, 84
	mov qword ptr [_t679], _t680
	lea _t681, qword ptr [_t71 + 88]
	mov _t682, 88
	mov qword ptr [_t681], _t682
	lea _t683, qword ptr [_t71 + 96]
	mov _t684, 86
	mov qword ptr [_t683], _t684
	lea _t685, qword ptr [_t71 + 104]
	mov _t686, 90
	mov qword ptr [_t685], _t686
	lea _t687, qword ptr [_t71 + 112]
	mov _t688, 78
	mov qword ptr [_t687], _t688
	lea _t689, qword ptr [_t71 + 120]
	mov _t690, 89
	mov qword ptr [_t689], _t690
	lea _t691, qword ptr [_t71 + 128]
	mov _t692, 69
	mov qword ptr [_t691], _t692
	lea _t693, qword ptr [_t71 + 136]
	mov _t694, 73
	mov qword ptr [_t693], _t694
	lea _t695, qword ptr [_t71 + 144]
	mov _t696, 87
	mov qword ptr [_t695], _t696
	lea _t697, qword ptr [_t71 + 152]
	mov _t698, 71
	mov qword ptr [_t697], _t698
	lea _t699, qword ptr [_t71 + 160]
	mov _t700, 65
	mov qword ptr [_t699], _t700
	lea _t701, qword ptr [_t71 + 168]
	mov _t702, 75
	mov qword ptr [_t701], _t702
	lea _t703, qword ptr [_t71 + 176]
	mov _t704, 77
	mov qword ptr [_t703], _t704
	lea _t705, qword ptr [_t71 + 184]
	mov _t706, 85
	mov qword ptr [_t705], _t706
	lea _t707, qword ptr [_t71 + 192]
	mov _t708, 83
	mov qword ptr [_t707], _t708
	lea _t709, qword ptr [_t71 + 200]
	mov _t710, 81
	mov qword ptr [_t709], _t710
	lea _t711, qword ptr [_t71 + 208]
	mov _t712, 79
	mov qword ptr [_t711], _t712
	lea _t713, qword ptr [_t71 + 8]
	mov _t152, _t713
	sub rsp, 8
	mov rdi, rsp
	mov rsi, _t152
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	mov _RV1, rax
	mov _RV2, rdx
	pop _RV3
	mov _t714, _RV1
	mov r3f, _t714
	mov _t715, _RV2
	mov r3b, _t715
	mov _t716, _RV3
	mov r3p, _t716
	mov _t717, 216
	mov _t158, _t717
	mov rdi, _t158
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t718, _RV1
	mov _t157, _t718
	mov _t72, _t157
	mov _t719, 26
	mov qword ptr [_t72], _t719
	lea _t720, qword ptr [_t72 + 8]
	mov _t721, 89
	mov qword ptr [_t720], _t721
	lea _t722, qword ptr [_t72 + 16]
	mov _t723, 82
	mov qword ptr [_t722], _t723
	lea _t724, qword ptr [_t72 + 24]
	mov _t725, 85
	mov qword ptr [_t724], _t725
	lea _t726, qword ptr [_t72 + 32]
	mov _t727, 72
	mov qword ptr [_t726], _t727
	lea _t728, qword ptr [_t72 + 40]
	mov _t729, 81
	mov qword ptr [_t728], _t729
	lea _t730, qword ptr [_t72 + 48]
	mov _t731, 83
	mov qword ptr [_t730], _t731
	lea _t732, qword ptr [_t72 + 56]
	mov _t733, 76
	mov qword ptr [_t732], _t733
	lea _t734, qword ptr [_t72 + 64]
	mov _t735, 68
	mov qword ptr [_t734], _t735
	lea _t736, qword ptr [_t72 + 72]
	mov _t737, 80
	mov qword ptr [_t736], _t737
	lea _t738, qword ptr [_t72 + 80]
	mov _t739, 88
	mov qword ptr [_t738], _t739
	lea _t740, qword ptr [_t72 + 88]
	mov _t741, 78
	mov qword ptr [_t740], _t741
	lea _t742, qword ptr [_t72 + 96]
	mov _t743, 71
	mov qword ptr [_t742], _t743
	lea _t744, qword ptr [_t72 + 104]
	mov _t745, 79
	mov qword ptr [_t744], _t745
	lea _t746, qword ptr [_t72 + 112]
	mov _t747, 75
	mov qword ptr [_t746], _t747
	lea _t748, qword ptr [_t72 + 120]
	mov _t749, 77
	mov qword ptr [_t748], _t749
	lea _t750, qword ptr [_t72 + 128]
	mov _t751, 73
	mov qword ptr [_t750], _t751
	lea _t752, qword ptr [_t72 + 136]
	mov _t753, 69
	mov qword ptr [_t752], _t753
	lea _t754, qword ptr [_t72 + 144]
	mov _t755, 66
	mov qword ptr [_t754], _t755
	lea _t756, qword ptr [_t72 + 152]
	mov _t757, 70
	mov qword ptr [_t756], _t757
	lea _t758, qword ptr [_t72 + 160]
	mov _t759, 90
	mov qword ptr [_t758], _t759
	lea _t760, qword ptr [_t72 + 168]
	mov _t761, 67
	mov qword ptr [_t760], _t761
	lea _t762, qword ptr [_t72 + 176]
	mov _t763, 87
	mov qword ptr [_t762], _t763
	lea _t764, qword ptr [_t72 + 184]
	mov _t765, 86
	mov qword ptr [_t764], _t765
	lea _t766, qword ptr [_t72 + 192]
	mov _t767, 74
	mov qword ptr [_t766], _t767
	lea _t768, qword ptr [_t72 + 200]
	mov _t769, 65
	mov qword ptr [_t768], _t769
	lea _t770, qword ptr [_t72 + 208]
	mov _t771, 84
	mov qword ptr [_t770], _t771
	lea _t772, qword ptr [_t72 + 8]
	mov _t156, _t772
	mov rdi, _t156
	and rsp, -16
	call _ImakeReflector_aiai
	mov _RV1, rax
	mov _t773, _RV1
	mov _t155, _t773
	mov mb, _t155
	mov _t774, 0
	mov pos, _t774
	_l83:
	mov _t775, 17576
	cmp pos, _t775
	jge _l81
	_l82:
	mov _t776, 0
	mov guess, _t776
	_l86:
	mov _t777, 26
	cmp guess, _t777
	jge _l84
	_l85:
	mov _t778, 1
	mov allMatch, _t778
	mov _t779, 0
	mov loop, _t779
	_l89:
	mov _t780, 8
	mov _t781, loops
	sub _t781, _t780
	mov _t782, qword ptr [_t781]
	cmp loop, _t782
	jge _l87
	_l88:
	mov l, guess
	mov _t783, 0
	mov loopPos, _t783
	_l92:
	mov _t76, loops
	mov _t75, loop
	mov _t784, 8
	mov _t785, _t76
	sub _t785, _t784
	mov _t786, qword ptr [_t785]
	cmp _t75, _t786
	setb _t787
	mov _t788, 1
	mov _t789, _t787
	xor _t789, _t788
	test _t789, _t789
	jnz _l93
	_l94:
	mov _t790, qword ptr [_t76 + _t75 * 8]
	mov _t74, _t790
	mov _t73, loopPos
	mov _t791, 8
	mov _t792, _t74
	sub _t792, _t791
	mov _t793, qword ptr [_t792]
	cmp _t73, _t793
	setb _t794
	mov _t795, 1
	mov _t796, _t794
	xor _t796, _t795
	test _t796, _t796
	jnz _l95
	_l96:
	mov _t797, qword ptr [_t74 + _t73 * 8]
	mov _t798, -1
	cmp _t797, _t798
	je _l90
	_l91:
	mov _t80, loops
	mov _t79, loop
	mov _t799, 8
	mov _t800, _t80
	sub _t800, _t799
	mov _t801, qword ptr [_t800]
	cmp _t79, _t801
	setb _t802
	mov _t803, 1
	mov _t804, _t802
	xor _t804, _t803
	test _t804, _t804
	jnz _l97
	_l98:
	mov _t805, qword ptr [_t80 + _t79 * 8]
	mov _t78, _t805
	mov _t77, loopPos
	mov _t806, 8
	mov _t807, _t78
	sub _t807, _t806
	mov _t808, qword ptr [_t807]
	cmp _t77, _t808
	setb _t809
	mov _t810, 1
	mov _t811, _t809
	xor _t811, _t810
	test _t811, _t811
	jnz _l99
	_l100:
	mov _t812, qword ptr [_t78 + _t77 * 8]
	lea _t813, qword ptr [pos + _t812]
	mov epos, _t813
	mov _t814, 26
	mov rax, epos
	xor rdx, rdx
	idiv _t814
	mov epos, rdx
	mov r1p, epos
	mov _t815, 26
	mov rax, epos
	xor rdx, rdx
	idiv _t815
	mov epos, rax
	mov _t816, 26
	mov rax, epos
	xor rdx, rdx
	idiv _t816
	mov epos, rdx
	mov r2p, epos
	mov _t817, 676
	mov rax, epos
	xor rdx, rdx
	idiv _t817
	mov epos, rax
	mov _t818, 26
	mov rax, epos
	xor rdx, rdx
	idiv _t818
	mov epos, rdx
	mov r3p, epos
	mov _t160, r1f
	mov _t161, r1b
	mov _t162, r1p
	mov _t163, l
	mov rcx, _t163
	mov rdx, _t162
	mov rsi, _t161
	mov rdi, _t160
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov _RV1, rax
	mov _t819, _RV1
	mov _t159, _t819
	mov l, _t159
	mov _t165, r2f
	mov _t166, r2b
	mov _t167, r2p
	mov _t168, l
	mov rcx, _t168
	mov rdx, _t167
	mov rsi, _t166
	mov rdi, _t165
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov _RV1, rax
	mov _t820, _RV1
	mov _t164, _t820
	mov l, _t164
	mov _t170, r3f
	mov _t171, r3b
	mov _t172, r3p
	mov _t173, l
	mov rcx, _t173
	mov rdx, _t172
	mov rsi, _t171
	mov rdi, _t170
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov _RV1, rax
	mov _t821, _RV1
	mov _t169, _t821
	mov l, _t169
	mov _t175, mb
	mov _t176, l
	mov rsi, _t176
	mov rdi, _t175
	and rsp, -16
	call _IreflectorEncrypt_iaii
	mov _RV1, rax
	mov _t822, _RV1
	mov _t174, _t822
	mov l, _t174
	mov _t178, r3f
	mov _t179, r3b
	mov _t180, r3p
	mov _t181, l
	mov rcx, _t181
	mov rdx, _t180
	mov rsi, _t179
	mov rdi, _t178
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov _RV1, rax
	mov _t823, _RV1
	mov _t177, _t823
	mov l, _t177
	mov _t183, r2f
	mov _t184, r2b
	mov _t185, r2p
	mov _t186, l
	mov rcx, _t186
	mov rdx, _t185
	mov rsi, _t184
	mov rdi, _t183
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov _RV1, rax
	mov _t824, _RV1
	mov _t182, _t824
	mov l, _t182
	mov _t188, r1f
	mov _t189, r1b
	mov _t190, r1p
	mov _t191, l
	mov rcx, _t191
	mov rdx, _t190
	mov rsi, _t189
	mov rdi, _t188
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov _RV1, rax
	mov _t825, _RV1
	mov _t187, _t825
	mov l, _t187
	lea _t826, qword ptr [loopPos + 1]
	mov loopPos, _t826
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
	cmp l, guess
	je _l101
	_l102:
	mov _t827, 0
	mov allMatch, _t827
	_l101:
	lea _t828, qword ptr [loop + 1]
	mov loop, _t828
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
	mov _t829, 1
	mov _t830, allMatch
	xor _t830, _t829
	test _t830, _t830
	jnz _l103
	_l104:
	mov _t831, 32
	mov _t193, _t831
	mov rdi, _t193
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t832, _RV1
	mov _t192, _t832
	mov _t82, _t192
	mov _t833, 3
	mov qword ptr [_t82], _t833
	lea _t834, qword ptr [_t82 + 8]
	mov _t81, _t834
	mov posStr, _t81
	mov _t835, 0
	mov _t84, _t835
	_l107:
	mov _t836, 3
	cmp _t84, _t836
	jge _l105
	_l106:
	lea _t837, qword ptr [_t81 + _t84 * 8]
	mov qword ptr [_t837], _t83
	lea _t838, qword ptr [_t84 + 1]
	mov _t84, _t838
	jmp _l107
	_l105:
	mov _t86, posStr
	mov _t839, 0
	mov _t85, _t839
	mov _t840, 8
	mov _t841, _t86
	sub _t841, _t840
	mov _t842, qword ptr [_t841]
	cmp _t85, _t842
	setb _t843
	mov _t844, 1
	mov _t845, _t843
	xor _t845, _t844
	test _t845, _t845
	jnz _l108
	_l109:
	lea _t846, qword ptr [_t86 + _t85 * 8]
	mov _t847, 26
	mov rax, pos
	xor rdx, rdx
	idiv _t847
	mov pos, rdx
	lea _t848, qword ptr [pos + 65]
	mov qword ptr [_t846], _t848
	mov _t88, posStr
	mov _t849, 1
	mov _t87, _t849
	mov _t850, 8
	mov _t851, _t88
	sub _t851, _t850
	mov _t852, qword ptr [_t851]
	cmp _t87, _t852
	setb _t853
	mov _t854, 1
	mov _t855, _t853
	xor _t855, _t854
	test _t855, _t855
	jnz _l110
	_l111:
	lea _t856, qword ptr [_t88 + _t87 * 8]
	mov _t857, 26
	mov rax, pos
	xor rdx, rdx
	idiv _t857
	mov pos, rax
	mov _t858, 26
	mov rax, pos
	xor rdx, rdx
	idiv _t858
	mov pos, rdx
	lea _t859, qword ptr [pos + 65]
	mov qword ptr [_t856], _t859
	mov _t90, posStr
	mov _t860, 2
	mov _t89, _t860
	mov _t861, 8
	mov _t862, _t90
	sub _t862, _t861
	mov _t863, qword ptr [_t862]
	cmp _t89, _t863
	setb _t864
	mov _t865, 1
	mov _t866, _t864
	xor _t866, _t865
	test _t866, _t866
	jnz _l112
	_l113:
	lea _t867, qword ptr [_t90 + _t89 * 8]
	mov _t868, 676
	mov rax, pos
	xor rdx, rdx
	idiv _t868
	mov pos, rax
	mov _t869, 26
	mov rax, pos
	xor rdx, rdx
	idiv _t869
	mov pos, rdx
	lea _t870, qword ptr [pos + 65]
	mov qword ptr [_t867], _t870
	mov _t871, 160
	mov _t196, _t871
	mov rdi, _t196
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t872, _RV1
	mov _t195, _t872
	mov _t91, _t195
	mov _t873, 19
	mov qword ptr [_t91], _t873
	lea _t874, qword ptr [_t91 + 8]
	mov _t875, 77
	mov qword ptr [_t874], _t875
	lea _t876, qword ptr [_t91 + 16]
	mov _t877, 65
	mov qword ptr [_t876], _t877
	lea _t878, qword ptr [_t91 + 24]
	mov _t879, 84
	mov qword ptr [_t878], _t879
	lea _t880, qword ptr [_t91 + 32]
	mov _t881, 67
	mov qword ptr [_t880], _t881
	lea _t882, qword ptr [_t91 + 40]
	mov _t883, 72
	mov qword ptr [_t882], _t883
	lea _t884, qword ptr [_t91 + 48]
	mov _t885, 32
	mov qword ptr [_t884], _t885
	lea _t886, qword ptr [_t91 + 56]
	mov _t887, 65
	mov qword ptr [_t886], _t887
	lea _t888, qword ptr [_t91 + 64]
	mov _t889, 116
	mov qword ptr [_t888], _t889
	lea _t890, qword ptr [_t91 + 72]
	mov _t891, 32
	mov qword ptr [_t890], _t891
	lea _t892, qword ptr [_t91 + 80]
	mov _t893, 114
	mov qword ptr [_t892], _t893
	lea _t894, qword ptr [_t91 + 88]
	mov _t895, 111
	mov qword ptr [_t894], _t895
	lea _t896, qword ptr [_t91 + 96]
	mov _t897, 116
	mov qword ptr [_t896], _t897
	lea _t898, qword ptr [_t91 + 104]
	mov _t899, 111
	mov qword ptr [_t898], _t899
	lea _t900, qword ptr [_t91 + 112]
	mov _t901, 114
	mov qword ptr [_t900], _t901
	lea _t902, qword ptr [_t91 + 120]
	mov _t903, 32
	mov qword ptr [_t902], _t903
	lea _t904, qword ptr [_t91 + 128]
	mov _t905, 112
	mov qword ptr [_t904], _t905
	lea _t906, qword ptr [_t91 + 136]
	mov _t907, 111
	mov qword ptr [_t906], _t907
	lea _t908, qword ptr [_t91 + 144]
	mov _t909, 115
	mov qword ptr [_t908], _t909
	lea _t910, qword ptr [_t91 + 152]
	mov _t911, 58
	mov qword ptr [_t910], _t911
	lea _t912, qword ptr [_t91 + 8]
	mov _t194, _t912
	mov rdi, _t194
	and rsp, -16
	call _Iprint_pai
	mov _t197, posStr
	mov rdi, _t197
	and rsp, -16
	call _Iprint_pai
	mov _t913, 176
	mov _t200, _t913
	mov rdi, _t200
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t914, _RV1
	mov _t199, _t914
	mov _t92, _t199
	mov _t915, 21
	mov qword ptr [_t92], _t915
	lea _t916, qword ptr [_t92 + 8]
	mov _t917, 32
	mov qword ptr [_t916], _t917
	lea _t918, qword ptr [_t92 + 16]
	mov _t919, 102
	mov qword ptr [_t918], _t919
	lea _t920, qword ptr [_t92 + 24]
	mov _t921, 105
	mov qword ptr [_t920], _t921
	lea _t922, qword ptr [_t92 + 32]
	mov _t923, 114
	mov qword ptr [_t922], _t923
	lea _t924, qword ptr [_t92 + 40]
	mov _t925, 115
	mov qword ptr [_t924], _t925
	lea _t926, qword ptr [_t92 + 48]
	mov _t927, 116
	mov qword ptr [_t926], _t927
	lea _t928, qword ptr [_t92 + 56]
	mov _t929, 32
	mov qword ptr [_t928], _t929
	lea _t930, qword ptr [_t92 + 64]
	mov _t931, 99
	mov qword ptr [_t930], _t931
	lea _t932, qword ptr [_t92 + 72]
	mov _t933, 111
	mov qword ptr [_t932], _t933
	lea _t934, qword ptr [_t92 + 80]
	mov _t935, 109
	mov qword ptr [_t934], _t935
	lea _t936, qword ptr [_t92 + 88]
	mov _t937, 101
	mov qword ptr [_t936], _t937
	lea _t938, qword ptr [_t92 + 96]
	mov _t939, 115
	mov qword ptr [_t938], _t939
	lea _t940, qword ptr [_t92 + 104]
	mov _t941, 32
	mov qword ptr [_t940], _t941
	lea _t942, qword ptr [_t92 + 112]
	mov _t943, 105
	mov qword ptr [_t942], _t943
	lea _t944, qword ptr [_t92 + 120]
	mov _t945, 110
	mov qword ptr [_t944], _t945
	lea _t946, qword ptr [_t92 + 128]
	mov _t947, 32
	mov qword ptr [_t946], _t947
	lea _t948, qword ptr [_t92 + 136]
	mov _t949, 102
	mov qword ptr [_t948], _t949
	lea _t950, qword ptr [_t92 + 144]
	mov _t951, 114
	mov qword ptr [_t950], _t951
	lea _t952, qword ptr [_t92 + 152]
	mov _t953, 111
	mov qword ptr [_t952], _t953
	lea _t954, qword ptr [_t92 + 160]
	mov _t955, 109
	mov qword ptr [_t954], _t955
	lea _t956, qword ptr [_t92 + 168]
	mov _t957, 58
	mov qword ptr [_t956], _t957
	lea _t958, qword ptr [_t92 + 8]
	mov _t198, _t958
	mov rdi, _t198
	and rsp, -16
	call _Iprint_pai
	mov _t959, 16
	mov _t202, _t959
	mov rdi, _t202
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t960, _RV1
	mov _t201, _t960
	mov _t94, _t201
	mov _t961, 1
	mov qword ptr [_t94], _t961
	lea _t962, qword ptr [_t94 + 8]
	mov _t93, _t962
	mov guessStr, _t93
	mov _t963, 0
	mov _t96, _t963
	_l116:
	mov _t964, 1
	cmp _t96, _t964
	jge _l114
	_l115:
	lea _t965, qword ptr [_t93 + _t96 * 8]
	mov qword ptr [_t965], _t95
	lea _t966, qword ptr [_t96 + 1]
	mov _t96, _t966
	jmp _l116
	_l114:
	mov _t98, guessStr
	mov _t967, 0
	mov _t97, _t967
	mov _t968, 8
	mov _t969, _t98
	sub _t969, _t968
	mov _t970, qword ptr [_t969]
	cmp _t97, _t970
	setb _t971
	mov _t972, 1
	mov _t973, _t971
	xor _t973, _t972
	test _t973, _t973
	jnz _l117
	_l118:
	lea _t974, qword ptr [_t98 + _t97 * 8]
	lea _t975, qword ptr [guess + 65]
	mov qword ptr [_t974], _t975
	mov _t203, guessStr
	mov rdi, _t203
	and rsp, -16
	call _Iprintln_pai
	_l103:
	lea _t976, qword ptr [guess + 1]
	mov guess, _t976
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
	lea _t977, qword ptr [pos + 1]
	mov pos, _t977
	jmp _l83
	_l81:
	leave
	ret
