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
	mov _ARG1, rdi
	mov t198, _ARG1
	mov c, t198
	mov t199, 65
	cmp c, t199
	jl l0
	l2:
	mov t200, 90
	cmp c, t200
	jg l0
	l1:
	mov t201, 65
	mov t202, c
	sub t202, t201
	mov t203, 97
	lea t204, qword ptr [t202 + t203]
	mov t105, t204
	mov rax, t105
	leave
	ret
	l0:
	mov t106, c
	mov rax, t106
	leave
	ret
_ImakeInverse_paiai:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov t205, _ARG1
	mov base, t205
	mov t206, _ARG2
	mov inv, t206
	mov t207, 0
	mov c, t207
	l5:
	mov t208, 26
	cmp c, t208
	jge l3
	l4:
	mov t1, inv
	mov t3, base
	mov t2, c
	mov t209, 8
	mov t210, t3
	sub t210, t209
	mov t210, qword ptr [t210]
	cmp t2, t210
	setb t211
	mov t212, 1
	mov t213, t211
	xor t213, t212
	test t213, t213
	jnz l8
	l9:
	mov t214, 8
	mov t215, t2
	imul t215, t214
	lea t216, qword ptr [t3 + t215]
	mov t216, qword ptr [t216]
	mov t0, t216
	mov t217, 8
	mov t218, t1
	sub t218, t217
	mov t218, qword ptr [t218]
	cmp t0, t218
	setb t219
	mov t220, 1
	mov t221, t219
	xor t221, t220
	test t221, t221
	jnz l6
	l7:
	mov t222, 8
	mov t223, t0
	imul t223, t222
	lea t224, qword ptr [t1 + t223]
	mov qword ptr [t224], c
	mov t225, 1
	lea t226, qword ptr [c + t225]
	mov c, t226
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
	mov _ARG1, rdi
	mov t227, _ARG1
	mov n, t227
	mov t4, n
	mov t5, n
	mov t228, 8
	mov t229, 8
	mov t230, t4
	imul t230, t229
	lea t231, qword ptr [t228 + t230]
	mov t108, t231
	mov rdi, t108
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t232, _RV1
	mov t107, t232
	mov t7, t107
	mov qword ptr [t7], t4
	mov t233, 8
	lea t234, qword ptr [t7 + t233]
	mov t6, t234
	mov a, t6
	mov t235, 0
	mov t9, t235
	l15:
	cmp t9, t4
	jge l13
	l14:
	mov t236, 8
	mov t237, 8
	mov t238, t5
	imul t238, t237
	lea t239, qword ptr [t236 + t238]
	mov t110, t239
	mov rdi, t110
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t240, _RV1
	mov t109, t240
	mov t11, t109
	mov qword ptr [t11], t5
	mov t241, 8
	lea t242, qword ptr [t11 + t241]
	mov t10, t242
	mov t8, t10
	mov t243, 0
	mov t13, t243
	l12:
	cmp t13, t5
	jge l10
	l11:
	mov t244, 8
	mov t245, t13
	imul t245, t244
	lea t246, qword ptr [t10 + t245]
	mov qword ptr [t246], t12
	mov t247, 1
	lea t248, qword ptr [t13 + t247]
	mov t13, t248
	jmp l12
	l10:
	mov t249, 8
	mov t250, t9
	imul t250, t249
	lea t251, qword ptr [t6 + t250]
	mov qword ptr [t251], t8
	mov t252, 1
	lea t253, qword ptr [t9 + t252]
	mov t9, t253
	jmp l15
	l13:
	mov t111, a
	mov rax, t111
	leave
	ret
_ImakeRotor_t3aaiaaiiai:
	mov _ARG1, rsi
	mov t254, _ARG1
	mov sig, t254
	mov t255, 26
	mov t113, t255
	mov rdi, t113
	and rsp, -16
	call _ImkMatrix_aaii
	mov _RV1, rax
	mov t256, _RV1
	mov t112, t256
	mov forward, t112
	mov t257, 26
	mov t115, t257
	mov rdi, t115
	and rsp, -16
	call _ImkMatrix_aaii
	mov _RV1, rax
	mov t258, _RV1
	mov t114, t258
	mov backward, t114
	mov t259, 216
	mov t117, t259
	mov rdi, t117
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t260, _RV1
	mov t116, t260
	mov t15, t116
	mov t261, 26
	mov qword ptr [t15], t261
	mov t262, 8
	lea t263, qword ptr [t15 + t262]
	mov t14, t263
	mov base, t14
	mov t264, 0
	mov t17, t264
	l18:
	mov t265, 26
	cmp t17, t265
	jge l16
	l17:
	mov t266, 8
	mov t267, t17
	imul t267, t266
	lea t268, qword ptr [t14 + t267]
	mov qword ptr [t268], t16
	mov t269, 1
	lea t270, qword ptr [t17 + t269]
	mov t17, t270
	jmp l18
	l16:
	mov t271, 0
	mov c, t271
	l21:
	mov t272, 26
	cmp c, t272
	jge l19
	l20:
	mov t19, sig
	mov t18, c
	mov t273, 8
	mov t274, t19
	sub t274, t273
	mov t274, qword ptr [t274]
	cmp t18, t274
	setb t275
	mov t276, 1
	mov t277, t275
	xor t277, t276
	test t277, t277
	jnz l22
	l23:
	mov t278, 8
	mov t279, t18
	imul t279, t278
	lea t280, qword ptr [t19 + t279]
	mov t280, qword ptr [t280]
	mov t119, t280
	mov rdi, t119
	and rsp, -16
	call _ItoLower_ii
	mov _RV1, rax
	mov t281, _RV1
	mov t118, t281
	mov t282, 97
	mov t283, t118
	sub t283, t282
	mov cd, t283
	mov t21, base
	mov t20, c
	mov t284, 8
	mov t285, t21
	sub t285, t284
	mov t285, qword ptr [t285]
	cmp t20, t285
	setb t286
	mov t287, 1
	mov t288, t286
	xor t288, t287
	test t288, t288
	jnz l24
	l25:
	mov t289, 8
	mov t290, t20
	imul t290, t289
	lea t291, qword ptr [t21 + t290]
	mov qword ptr [t291], cd
	mov t292, 1
	lea t293, qword ptr [c + t292]
	mov c, t293
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
	mov t294, 216
	mov t121, t294
	mov rdi, t121
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t295, _RV1
	mov t120, t295
	mov t23, t120
	mov t296, 26
	mov qword ptr [t23], t296
	mov t297, 8
	lea t298, qword ptr [t23 + t297]
	mov t22, t298
	mov inv, t22
	mov t299, 0
	mov t25, t299
	l28:
	mov t300, 26
	cmp t25, t300
	jge l26
	l27:
	mov t301, 8
	mov t302, t25
	imul t302, t301
	lea t303, qword ptr [t22 + t302]
	mov qword ptr [t303], t24
	mov t304, 1
	lea t305, qword ptr [t25 + t304]
	mov t25, t305
	jmp l28
	l26:
	mov t306, 0
	mov rot, t306
	l31:
	mov t307, 26
	cmp rot, t307
	jge l29
	l30:
	mov t308, 0
	mov c, t308
	l34:
	mov t309, 26
	cmp c, t309
	jge l32
	l33:
	mov t31, forward
	mov t30, rot
	mov t310, 8
	mov t311, t31
	sub t311, t310
	mov t311, qword ptr [t311]
	cmp t30, t311
	setb t312
	mov t313, 1
	mov t314, t312
	xor t314, t313
	test t314, t314
	jnz l39
	l40:
	mov t315, 8
	mov t316, t30
	imul t316, t315
	lea t317, qword ptr [t31 + t316]
	mov t317, qword ptr [t317]
	mov t27, t317
	mov t26, c
	mov t318, 8
	mov t319, t27
	sub t319, t318
	mov t319, qword ptr [t319]
	cmp t26, t319
	setb t320
	mov t321, 1
	mov t322, t320
	xor t322, t321
	test t322, t322
	jnz l37
	l38:
	mov t323, 8
	mov t324, t26
	imul t324, t323
	lea t325, qword ptr [t27 + t324]
	mov t122, t325
	mov t29, base
	mov t28, c
	mov t326, 8
	mov t327, t29
	sub t327, t326
	mov t327, qword ptr [t327]
	cmp t28, t327
	setb t328
	mov t329, 1
	mov t330, t328
	xor t330, t329
	test t330, t330
	jnz l35
	l36:
	mov t331, 8
	mov t332, t28
	imul t332, t331
	lea t333, qword ptr [t29 + t332]
	mov t333, qword ptr [t333]
	mov qword ptr [t122], t333
	mov t334, 1
	lea t335, qword ptr [c + t334]
	mov c, t335
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
	mov t123, base
	mov t124, inv
	mov rdi, t124
	mov rsi, t123
	and rsp, -16
	call _ImakeInverse_paiai
	mov t336, 0
	mov c, t336
	l43:
	mov t337, 26
	cmp c, t337
	jge l41
	l42:
	mov t37, backward
	mov t36, rot
	mov t338, 8
	mov t339, t37
	sub t339, t338
	mov t339, qword ptr [t339]
	cmp t36, t339
	setb t340
	mov t341, 1
	mov t342, t340
	xor t342, t341
	test t342, t342
	jnz l48
	l49:
	mov t343, 8
	mov t344, t36
	imul t344, t343
	lea t345, qword ptr [t37 + t344]
	mov t345, qword ptr [t345]
	mov t33, t345
	mov t32, c
	mov t346, 8
	mov t347, t33
	sub t347, t346
	mov t347, qword ptr [t347]
	cmp t32, t347
	setb t348
	mov t349, 1
	mov t350, t348
	xor t350, t349
	test t350, t350
	jnz l46
	l47:
	mov t351, 8
	mov t352, t32
	imul t352, t351
	lea t353, qword ptr [t33 + t352]
	mov t125, t353
	mov t35, inv
	mov t34, c
	mov t354, 8
	mov t355, t35
	sub t355, t354
	mov t355, qword ptr [t355]
	cmp t34, t355
	setb t356
	mov t357, 1
	mov t358, t356
	xor t358, t357
	test t358, t358
	jnz l44
	l45:
	mov t359, 8
	mov t360, t34
	imul t360, t359
	lea t361, qword ptr [t35 + t360]
	mov t361, qword ptr [t361]
	mov qword ptr [t125], t361
	mov t362, 1
	lea t363, qword ptr [c + t362]
	mov c, t363
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
	mov t39, base
	mov t364, 0
	mov t38, t364
	mov t365, 8
	mov t366, t39
	sub t366, t365
	mov t366, qword ptr [t366]
	cmp t38, t366
	setb t367
	mov t368, 1
	mov t369, t367
	xor t369, t368
	test t369, t369
	jnz l50
	l51:
	mov t370, 8
	mov t371, t38
	imul t371, t370
	lea t372, qword ptr [t39 + t371]
	mov t372, qword ptr [t372]
	mov t373, 1
	mov t374, t372
	sub t374, t373
	mov t375, 26
	lea t376, qword ptr [t374 + t375]
	mov t377, 26
	mov rax, t376
	xor rdx, rdx
	idiv t377
	mov t376, rdx
	mov first, t376
	mov t378, 1
	mov pos, t378
	l54:
	mov t379, 26
	cmp pos, t379
	jge l52
	l53:
	mov t41, base
	mov t380, 1
	mov t381, pos
	sub t381, t380
	mov t40, t381
	mov t382, 8
	mov t383, t41
	sub t383, t382
	mov t383, qword ptr [t383]
	cmp t40, t383
	setb t384
	mov t385, 1
	mov t386, t384
	xor t386, t385
	test t386, t386
	jnz l57
	l58:
	mov t387, 8
	mov t388, t40
	imul t388, t387
	lea t389, qword ptr [t41 + t388]
	mov t126, t389
	mov t43, base
	mov t42, pos
	mov t390, 8
	mov t391, t43
	sub t391, t390
	mov t391, qword ptr [t391]
	cmp t42, t391
	setb t392
	mov t393, 1
	mov t394, t392
	xor t394, t393
	test t394, t394
	jnz l55
	l56:
	mov t395, 8
	mov t396, t42
	imul t396, t395
	lea t397, qword ptr [t43 + t396]
	mov t397, qword ptr [t397]
	mov t398, 1
	mov t399, t397
	sub t399, t398
	mov t400, 26
	lea t401, qword ptr [t399 + t400]
	mov t402, 26
	mov rax, t401
	xor rdx, rdx
	idiv t402
	mov t401, rdx
	mov qword ptr [t126], t401
	mov t403, 1
	lea t404, qword ptr [pos + t403]
	mov pos, t404
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
	mov t45, base
	mov t405, 25
	mov t44, t405
	mov t406, 8
	mov t407, t45
	sub t407, t406
	mov t407, qword ptr [t407]
	cmp t44, t407
	setb t408
	mov t409, 1
	mov t410, t408
	xor t410, t409
	test t410, t410
	jnz l59
	l60:
	mov t411, 8
	mov t412, t44
	imul t412, t411
	lea t413, qword ptr [t45 + t412]
	mov qword ptr [t413], first
	mov t414, 1
	lea t415, qword ptr [rot + t414]
	mov rot, t415
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
	mov t127, forward
	mov t128, backward
	mov t416, 0
	mov t129, t416
	mov rax, t129
	mov rdx, t128
	push t127
	leave
	ret
_IrotorEncryptForward_iaaiaaiii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _ARG3, rdx
	mov _ARG4, rcx
	mov t417, _ARG1
	mov forward, t417
	mov t418, _ARG2
	mov backward, t418
	mov t419, _ARG3
	mov pos, t419
	mov t420, _ARG4
	mov letter, t420
	mov t49, forward
	mov t48, pos
	mov t421, 8
	mov t422, t49
	sub t422, t421
	mov t422, qword ptr [t422]
	cmp t48, t422
	setb t423
	mov t424, 1
	mov t425, t423
	xor t425, t424
	test t425, t425
	jnz l61
	l62:
	mov t426, 8
	mov t427, t48
	imul t427, t426
	lea t428, qword ptr [t49 + t427]
	mov t428, qword ptr [t428]
	mov t47, t428
	mov t46, letter
	mov t429, 8
	mov t430, t47
	sub t430, t429
	mov t430, qword ptr [t430]
	cmp t46, t430
	setb t431
	mov t432, 1
	mov t433, t431
	xor t433, t432
	test t433, t433
	jnz l63
	l64:
	mov t434, 8
	mov t435, t46
	imul t435, t434
	lea t436, qword ptr [t47 + t435]
	mov t436, qword ptr [t436]
	mov t130, t436
	mov rax, t130
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
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov _ARG3, rdx
	mov _ARG4, rcx
	mov t437, _ARG1
	mov forward, t437
	mov t438, _ARG2
	mov backward, t438
	mov t439, _ARG3
	mov pos, t439
	mov t440, _ARG4
	mov letter, t440
	mov t53, backward
	mov t52, pos
	mov t441, 8
	mov t442, t53
	sub t442, t441
	mov t442, qword ptr [t442]
	cmp t52, t442
	setb t443
	mov t444, 1
	mov t445, t443
	xor t445, t444
	test t445, t445
	jnz l65
	l66:
	mov t446, 8
	mov t447, t52
	imul t447, t446
	lea t448, qword ptr [t53 + t447]
	mov t448, qword ptr [t448]
	mov t51, t448
	mov t50, letter
	mov t449, 8
	mov t450, t51
	sub t450, t449
	mov t450, qword ptr [t450]
	cmp t50, t450
	setb t451
	mov t452, 1
	mov t453, t451
	xor t453, t452
	test t453, t453
	jnz l67
	l68:
	mov t454, 8
	mov t455, t50
	imul t455, t454
	lea t456, qword ptr [t51 + t455]
	mov t456, qword ptr [t456]
	mov t131, t456
	mov rax, t131
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
	mov _ARG1, rdi
	mov t457, _ARG1
	mov encoding, t457
	mov t458, 216
	mov t133, t458
	mov rdi, t133
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t459, _RV1
	mov t132, t459
	mov t55, t132
	mov t460, 26
	mov qword ptr [t55], t460
	mov t461, 8
	lea t462, qword ptr [t55 + t461]
	mov t54, t462
	mov perm, t54
	mov t463, 0
	mov t57, t463
	l71:
	mov t464, 26
	cmp t57, t464
	jge l69
	l70:
	mov t465, 8
	mov t466, t57
	imul t466, t465
	lea t467, qword ptr [t54 + t466]
	mov qword ptr [t467], t56
	mov t468, 1
	lea t469, qword ptr [t57 + t468]
	mov t57, t469
	jmp l71
	l69:
	mov t470, 0
	mov c, t470
	l74:
	mov t471, 26
	cmp c, t471
	jge l72
	l73:
	mov t59, encoding
	mov t58, c
	mov t472, 8
	mov t473, t59
	sub t473, t472
	mov t473, qword ptr [t473]
	cmp t58, t473
	setb t474
	mov t475, 1
	mov t476, t474
	xor t476, t475
	test t476, t476
	jnz l75
	l76:
	mov t477, 8
	mov t478, t58
	imul t478, t477
	lea t479, qword ptr [t59 + t478]
	mov t479, qword ptr [t479]
	mov t135, t479
	mov rdi, t135
	and rsp, -16
	call _ItoLower_ii
	mov _RV1, rax
	mov t480, _RV1
	mov t134, t480
	mov t481, 97
	mov t482, t134
	sub t482, t481
	mov cd, t482
	mov t61, perm
	mov t60, c
	mov t483, 8
	mov t484, t61
	sub t484, t483
	mov t484, qword ptr [t484]
	cmp t60, t484
	setb t485
	mov t486, 1
	mov t487, t485
	xor t487, t486
	test t487, t487
	jnz l77
	l78:
	mov t488, 8
	mov t489, t60
	imul t489, t488
	lea t490, qword ptr [t61 + t489]
	mov qword ptr [t490], cd
	mov t491, 1
	lea t492, qword ptr [c + t491]
	mov c, t492
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
	mov t136, perm
	mov rax, t136
	leave
	ret
_IreflectorEncrypt_iaii:
	mov _ARG1, rdi
	mov _ARG2, rsi
	mov t493, _ARG1
	mov perm, t493
	mov t494, _ARG2
	mov l, t494
	mov t63, perm
	mov t62, l
	mov t495, 8
	mov t496, t63
	sub t496, t495
	mov t496, qword ptr [t496]
	cmp t62, t496
	setb t497
	mov t498, 1
	mov t499, t497
	xor t499, t498
	test t499, t499
	jnz l79
	l80:
	mov t500, 8
	mov t501, t62
	imul t501, t500
	lea t502, qword ptr [t63 + t501]
	mov t502, qword ptr [t502]
	mov t137, t502
	mov rax, t137
	leave
	ret
	l79:
	and rsp, -16
	call _xi_out_of_bounds
	jmp l80
_Imain_paai:
	mov _ARG1, rdi
	mov t503, _ARG1
	mov a, t503
	mov t504, 40
	mov t139, t504
	mov rdi, t139
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t505, _RV1
	mov t138, t505
	mov t68, t138
	mov t506, 4
	mov qword ptr [t68], t506
	mov t507, 8
	lea t508, qword ptr [t68 + t507]
	mov t140, t508
	mov t509, 72
	mov t142, t509
	mov rdi, t142
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t510, _RV1
	mov t141, t510
	mov t64, t141
	mov t511, 8
	mov qword ptr [t64], t511
	mov t512, 8
	lea t513, qword ptr [t64 + t512]
	mov t514, 12
	mov qword ptr [t513], t514
	mov t515, 16
	lea t516, qword ptr [t64 + t515]
	mov t517, 27
	mov qword ptr [t516], t517
	mov t518, 24
	lea t519, qword ptr [t64 + t518]
	mov t520, 6
	mov qword ptr [t519], t520
	mov t521, 32
	lea t522, qword ptr [t64 + t521]
	mov t523, 57
	mov qword ptr [t522], t523
	mov t524, 40
	lea t525, qword ptr [t64 + t524]
	mov t526, 25
	mov qword ptr [t525], t526
	mov t527, 48
	lea t528, qword ptr [t64 + t527]
	mov t529, 51
	mov qword ptr [t528], t529
	mov t530, 56
	lea t531, qword ptr [t64 + t530]
	mov t532, 52
	mov qword ptr [t531], t532
	mov t533, 64
	lea t534, qword ptr [t64 + t533]
	mov t535, -1
	mov qword ptr [t534], t535
	mov t536, 8
	lea t537, qword ptr [t64 + t536]
	mov qword ptr [t140], t537
	mov t538, 16
	lea t539, qword ptr [t68 + t538]
	mov t143, t539
	mov t540, 72
	mov t145, t540
	mov rdi, t145
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t541, _RV1
	mov t144, t541
	mov t65, t144
	mov t542, 8
	mov qword ptr [t65], t542
	mov t543, 8
	lea t544, qword ptr [t65 + t543]
	mov t545, 12
	mov qword ptr [t544], t545
	mov t546, 16
	lea t547, qword ptr [t65 + t546]
	mov t548, 27
	mov qword ptr [t547], t548
	mov t549, 24
	lea t550, qword ptr [t65 + t549]
	mov t551, 6
	mov qword ptr [t550], t551
	mov t552, 32
	lea t553, qword ptr [t65 + t552]
	mov t554, 55
	mov qword ptr [t553], t554
	mov t555, 40
	lea t556, qword ptr [t65 + t555]
	mov t557, 25
	mov qword ptr [t556], t557
	mov t558, 48
	lea t559, qword ptr [t65 + t558]
	mov t560, 51
	mov qword ptr [t559], t560
	mov t561, 56
	lea t562, qword ptr [t65 + t561]
	mov t563, 52
	mov qword ptr [t562], t563
	mov t564, 64
	lea t565, qword ptr [t65 + t564]
	mov t566, -1
	mov qword ptr [t565], t566
	mov t567, 8
	lea t568, qword ptr [t65 + t567]
	mov qword ptr [t143], t568
	mov t569, 24
	lea t570, qword ptr [t68 + t569]
	mov t146, t570
	mov t571, 40
	mov t148, t571
	mov rdi, t148
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t572, _RV1
	mov t147, t572
	mov t66, t147
	mov t573, 4
	mov qword ptr [t66], t573
	mov t574, 8
	lea t575, qword ptr [t66 + t574]
	mov t576, 12
	mov qword ptr [t575], t576
	mov t577, 16
	lea t578, qword ptr [t66 + t577]
	mov t579, 46
	mov qword ptr [t578], t579
	mov t580, 24
	lea t581, qword ptr [t66 + t580]
	mov t582, 47
	mov qword ptr [t581], t582
	mov t583, 32
	lea t584, qword ptr [t66 + t583]
	mov t585, -1
	mov qword ptr [t584], t585
	mov t586, 8
	lea t587, qword ptr [t66 + t586]
	mov qword ptr [t146], t587
	mov t588, 32
	lea t589, qword ptr [t68 + t588]
	mov t149, t589
	mov t590, 64
	mov t151, t590
	mov rdi, t151
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t591, _RV1
	mov t150, t591
	mov t67, t150
	mov t592, 7
	mov qword ptr [t67], t592
	mov t593, 8
	lea t594, qword ptr [t67 + t593]
	mov t595, 12
	mov qword ptr [t594], t595
	mov t596, 16
	lea t597, qword ptr [t67 + t596]
	mov t598, 27
	mov qword ptr [t597], t598
	mov t599, 24
	lea t600, qword ptr [t67 + t599]
	mov t601, 6
	mov qword ptr [t600], t601
	mov t602, 32
	lea t603, qword ptr [t67 + t602]
	mov t604, 16
	mov qword ptr [t603], t604
	mov t605, 40
	lea t606, qword ptr [t67 + t605]
	mov t607, 11
	mov qword ptr [t606], t607
	mov t608, 48
	lea t609, qword ptr [t67 + t608]
	mov t610, 52
	mov qword ptr [t609], t610
	mov t611, 56
	lea t612, qword ptr [t67 + t611]
	mov t613, -1
	mov qword ptr [t612], t613
	mov t614, 8
	lea t615, qword ptr [t67 + t614]
	mov qword ptr [t149], t615
	mov t616, 8
	lea t617, qword ptr [t68 + t616]
	mov loops, t617
	lea t618, qword ptr g0[rip]
	mov t69, t618
	mov t619, 8
	lea t620, qword ptr [t69 + t619]
	mov t70, t620
	mov t152, t70
	sub rsp, 8
	mov rdi, rsp
	mov rsi, t152
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	pop _RV3
	mov _RV2, rdx
	mov _RV1, rax
	mov t621, _RV1
	mov r1f, t621
	mov t622, _RV2
	mov r1b, t622
	mov t623, _RV3
	mov r1p, t623
	lea t624, qword ptr g1[rip]
	mov t71, t624
	mov t625, 8
	lea t626, qword ptr [t71 + t625]
	mov t72, t626
	mov t153, t72
	sub rsp, 8
	mov rdi, rsp
	mov rsi, t153
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	pop _RV3
	mov _RV2, rdx
	mov _RV1, rax
	mov t627, _RV1
	mov r2f, t627
	mov t628, _RV2
	mov r2b, t628
	mov t629, _RV3
	mov r2p, t629
	lea t630, qword ptr g2[rip]
	mov t73, t630
	mov t631, 8
	lea t632, qword ptr [t73 + t631]
	mov t74, t632
	mov t154, t74
	sub rsp, 8
	mov rdi, rsp
	mov rsi, t154
	and rsp, -16
	call _ImakeRotor_t3aaiaaiiai
	pop _RV3
	mov _RV2, rdx
	mov _RV1, rax
	mov t633, _RV1
	mov r3f, t633
	mov t634, _RV2
	mov r3b, t634
	mov t635, _RV3
	mov r3p, t635
	lea t636, qword ptr g3[rip]
	mov t75, t636
	mov t637, 8
	lea t638, qword ptr [t75 + t637]
	mov t76, t638
	mov t156, t76
	mov rdi, t156
	and rsp, -16
	call _ImakeReflector_aiai
	mov _RV1, rax
	mov t639, _RV1
	mov t155, t639
	mov mb, t155
	mov t640, 0
	mov pos, t640
	l83:
	mov t641, 17576
	cmp pos, t641
	jge l81
	l82:
	mov t642, 0
	mov guess, t642
	l86:
	mov t643, 26
	cmp guess, t643
	jge l84
	l85:
	mov t644, 1
	mov allMatch, t644
	mov t645, 0
	mov loop, t645
	l89:
	mov t646, 8
	mov t647, loops
	sub t647, t646
	mov t647, qword ptr [t647]
	cmp loop, t647
	jge l87
	l88:
	mov l, guess
	mov t648, 0
	mov loopPos, t648
	l92:
	mov t80, loops
	mov t79, loop
	mov t649, 8
	mov t650, t80
	sub t650, t649
	mov t650, qword ptr [t650]
	cmp t79, t650
	setb t651
	mov t652, 1
	mov t653, t651
	xor t653, t652
	test t653, t653
	jnz l93
	l94:
	mov t654, 8
	mov t655, t79
	imul t655, t654
	lea t656, qword ptr [t80 + t655]
	mov t656, qword ptr [t656]
	mov t78, t656
	mov t77, loopPos
	mov t657, 8
	mov t658, t78
	sub t658, t657
	mov t658, qword ptr [t658]
	cmp t77, t658
	setb t659
	mov t660, 1
	mov t661, t659
	xor t661, t660
	test t661, t661
	jnz l95
	l96:
	mov t662, 8
	mov t663, t77
	imul t663, t662
	lea t664, qword ptr [t78 + t663]
	mov t664, qword ptr [t664]
	mov t665, -1
	cmp t664, t665
	je l90
	l91:
	mov t84, loops
	mov t83, loop
	mov t666, 8
	mov t667, t84
	sub t667, t666
	mov t667, qword ptr [t667]
	cmp t83, t667
	setb t668
	mov t669, 1
	mov t670, t668
	xor t670, t669
	test t670, t670
	jnz l97
	l98:
	mov t671, 8
	mov t672, t83
	imul t672, t671
	lea t673, qword ptr [t84 + t672]
	mov t673, qword ptr [t673]
	mov t82, t673
	mov t81, loopPos
	mov t674, 8
	mov t675, t82
	sub t675, t674
	mov t675, qword ptr [t675]
	cmp t81, t675
	setb t676
	mov t677, 1
	mov t678, t676
	xor t678, t677
	test t678, t678
	jnz l99
	l100:
	mov t679, 8
	mov t680, t81
	imul t680, t679
	lea t681, qword ptr [t82 + t680]
	mov t681, qword ptr [t681]
	lea t682, qword ptr [pos + t681]
	mov epos, t682
	mov t683, 26
	mov rax, epos
	xor rdx, rdx
	idiv t683
	mov epos, rdx
	mov r1p, epos
	mov t684, 26
	mov rax, epos
	xor rdx, rdx
	idiv t684
	mov epos, rax
	mov t685, 26
	mov rax, epos
	xor rdx, rdx
	idiv t685
	mov epos, rdx
	mov r2p, epos
	mov t686, 676
	mov rax, epos
	xor rdx, rdx
	idiv t686
	mov epos, rax
	mov t687, 26
	mov rax, epos
	xor rdx, rdx
	idiv t687
	mov epos, rdx
	mov r3p, epos
	mov t158, r1f
	mov t159, r1b
	mov t160, r1p
	mov t161, l
	mov rdi, t161
	mov rsi, t160
	mov rdx, t159
	mov rcx, t158
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov _RV1, rax
	mov t688, _RV1
	mov t157, t688
	mov l, t157
	mov t163, r2f
	mov t164, r2b
	mov t165, r2p
	mov t166, l
	mov rdi, t166
	mov rsi, t165
	mov rdx, t164
	mov rcx, t163
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov _RV1, rax
	mov t689, _RV1
	mov t162, t689
	mov l, t162
	mov t168, r3f
	mov t169, r3b
	mov t170, r3p
	mov t171, l
	mov rdi, t171
	mov rsi, t170
	mov rdx, t169
	mov rcx, t168
	and rsp, -16
	call _IrotorEncryptForward_iaaiaaiii
	mov _RV1, rax
	mov t690, _RV1
	mov t167, t690
	mov l, t167
	mov t173, mb
	mov t174, l
	mov rdi, t174
	mov rsi, t173
	and rsp, -16
	call _IreflectorEncrypt_iaii
	mov _RV1, rax
	mov t691, _RV1
	mov t172, t691
	mov l, t172
	mov t176, r3f
	mov t177, r3b
	mov t178, r3p
	mov t179, l
	mov rdi, t179
	mov rsi, t178
	mov rdx, t177
	mov rcx, t176
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov _RV1, rax
	mov t692, _RV1
	mov t175, t692
	mov l, t175
	mov t181, r2f
	mov t182, r2b
	mov t183, r2p
	mov t184, l
	mov rdi, t184
	mov rsi, t183
	mov rdx, t182
	mov rcx, t181
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov _RV1, rax
	mov t693, _RV1
	mov t180, t693
	mov l, t180
	mov t186, r1f
	mov t187, r1b
	mov t188, r1p
	mov t189, l
	mov rdi, t189
	mov rsi, t188
	mov rdx, t187
	mov rcx, t186
	and rsp, -16
	call _IrotorEncryptBack_iaaiaaiii
	mov _RV1, rax
	mov t694, _RV1
	mov t185, t694
	mov l, t185
	mov t695, 1
	lea t696, qword ptr [loopPos + t695]
	mov loopPos, t696
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
	cmp l, guess
	je l101
	l102:
	mov t697, 0
	mov allMatch, t697
	l101:
	mov t698, 1
	lea t699, qword ptr [loop + t698]
	mov loop, t699
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
	mov t700, 1
	mov t701, allMatch
	xor t701, t700
	test t701, t701
	jnz l103
	l104:
	mov t702, 32
	mov t191, t702
	mov rdi, t191
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t703, _RV1
	mov t190, t703
	mov t86, t190
	mov t704, 3
	mov qword ptr [t86], t704
	mov t705, 8
	lea t706, qword ptr [t86 + t705]
	mov t85, t706
	mov posStr, t85
	mov t707, 0
	mov t88, t707
	l107:
	mov t708, 3
	cmp t88, t708
	jge l105
	l106:
	mov t709, 8
	mov t710, t88
	imul t710, t709
	lea t711, qword ptr [t85 + t710]
	mov qword ptr [t711], t87
	mov t712, 1
	lea t713, qword ptr [t88 + t712]
	mov t88, t713
	jmp l107
	l105:
	mov t90, posStr
	mov t714, 0
	mov t89, t714
	mov t715, 8
	mov t716, t90
	sub t716, t715
	mov t716, qword ptr [t716]
	cmp t89, t716
	setb t717
	mov t718, 1
	mov t719, t717
	xor t719, t718
	test t719, t719
	jnz l108
	l109:
	mov t720, 8
	mov t721, t89
	imul t721, t720
	lea t722, qword ptr [t90 + t721]
	mov t723, 26
	mov rax, pos
	xor rdx, rdx
	idiv t723
	mov pos, rdx
	mov t724, 65
	lea t725, qword ptr [pos + t724]
	mov qword ptr [t722], t725
	mov t92, posStr
	mov t726, 1
	mov t91, t726
	mov t727, 8
	mov t728, t92
	sub t728, t727
	mov t728, qword ptr [t728]
	cmp t91, t728
	setb t729
	mov t730, 1
	mov t731, t729
	xor t731, t730
	test t731, t731
	jnz l110
	l111:
	mov t732, 8
	mov t733, t91
	imul t733, t732
	lea t734, qword ptr [t92 + t733]
	mov t735, 26
	mov rax, pos
	xor rdx, rdx
	idiv t735
	mov pos, rax
	mov t736, 26
	mov rax, pos
	xor rdx, rdx
	idiv t736
	mov pos, rdx
	mov t737, 65
	lea t738, qword ptr [pos + t737]
	mov qword ptr [t734], t738
	mov t94, posStr
	mov t739, 2
	mov t93, t739
	mov t740, 8
	mov t741, t94
	sub t741, t740
	mov t741, qword ptr [t741]
	cmp t93, t741
	setb t742
	mov t743, 1
	mov t744, t742
	xor t744, t743
	test t744, t744
	jnz l112
	l113:
	mov t745, 8
	mov t746, t93
	imul t746, t745
	lea t747, qword ptr [t94 + t746]
	mov t748, 676
	mov rax, pos
	xor rdx, rdx
	idiv t748
	mov pos, rax
	mov t749, 26
	mov rax, pos
	xor rdx, rdx
	idiv t749
	mov pos, rdx
	mov t750, 65
	lea t751, qword ptr [pos + t750]
	mov qword ptr [t747], t751
	lea t752, qword ptr g4[rip]
	mov t95, t752
	mov t753, 8
	lea t754, qword ptr [t95 + t753]
	mov t96, t754
	mov t192, t96
	mov rdi, t192
	and rsp, -16
	call _Iprint_pai
	mov t193, posStr
	mov rdi, t193
	and rsp, -16
	call _Iprint_pai
	lea t755, qword ptr g5[rip]
	mov t97, t755
	mov t756, 8
	lea t757, qword ptr [t97 + t756]
	mov t98, t757
	mov t194, t98
	mov rdi, t194
	and rsp, -16
	call _Iprint_pai
	mov t758, 16
	mov t196, t758
	mov rdi, t196
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov t759, _RV1
	mov t195, t759
	mov t100, t195
	mov t760, 1
	mov qword ptr [t100], t760
	mov t761, 8
	lea t762, qword ptr [t100 + t761]
	mov t99, t762
	mov guessStr, t99
	mov t763, 0
	mov t102, t763
	l116:
	mov t764, 1
	cmp t102, t764
	jge l114
	l115:
	mov t765, 8
	mov t766, t102
	imul t766, t765
	lea t767, qword ptr [t99 + t766]
	mov qword ptr [t767], t101
	mov t768, 1
	lea t769, qword ptr [t102 + t768]
	mov t102, t769
	jmp l116
	l114:
	mov t104, guessStr
	mov t770, 0
	mov t103, t770
	mov t771, 8
	mov t772, t104
	sub t772, t771
	mov t772, qword ptr [t772]
	cmp t103, t772
	setb t773
	mov t774, 1
	mov t775, t773
	xor t775, t774
	test t775, t775
	jnz l117
	l118:
	mov t776, 8
	mov t777, t103
	imul t777, t776
	lea t778, qword ptr [t104 + t777]
	mov t779, 65
	lea t780, qword ptr [guess + t779]
	mov qword ptr [t778], t780
	mov t197, guessStr
	mov rdi, t197
	and rsp, -16
	call _Iprintln_pai
	l103:
	mov t781, 1
	lea t782, qword ptr [guess + t781]
	mov guess, t782
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
	mov t783, 1
	lea t784, qword ptr [pos + t783]
	mov pos, t784
	jmp l83
	l81:
	leave
	ret
