.intel_syntax noprefix
.data
.globl _Imain_paai, _Ifoo_t8iiiiiiiiiiiiiiii
.text
_Imain_paai:
	mov _ARG1, rdi
	mov _t337, _ARG1
	mov args, _t337
	mov _t338, 32
	mov _t176, _t338
	mov _t339, 34
	mov _t177, _t339
	mov _t340, 13
	mov _t178, _t340
	mov _t341, 14
	mov _t179, _t341
	mov _t342, 15
	mov _t180, _t342
	mov _t343, 16
	mov _t181, _t343
	mov _t344, 17
	mov _t182, _t344
	mov _t345, 38
	mov _t183, _t345
	sub rsp, 48
	mov rdi, rsp
	push _t183
	push _t182
	push _t181
	mov r9, _t180
	mov r8, _t179
	mov rcx, _t178
	mov rdx, _t177
	mov rsi, _t176
	and rsp, -16
	call _Ifoo_t8iiiiiiiiiiiiiiii
	add rsp, 24
	mov _RV1, rax
	mov _RV2, rdx
	pop _RV3
	pop _RV4
	pop _RV5
	pop _RV6
	pop _RV7
	pop _RV8
	mov _t346, _RV1
	mov n, _t346
	mov _t347, _RV2
	mov m, _t347
	mov _t348, _RV3
	mov o, _t348
	mov _t349, _RV4
	mov p, _t349
	mov _t350, _RV5
	mov q, _t350
	mov _t351, _RV6
	mov r, _t351
	mov _t352, _RV7
	mov s, _t352
	mov _t353, _RV8
	mov t, _t353
	mov _t354, 24
	mov _t186, _t354
	mov rdi, _t186
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t355, _RV1
	mov _t185, _t355
	mov _t10, _t185
	mov _t356, 2
	mov qword ptr [_t10], _t356
	lea _t357, qword ptr [_t10 + 8]
	mov _t358, 110
	mov qword ptr [_t357], _t358
	lea _t359, qword ptr [_t10 + 16]
	mov _t360, 32
	mov qword ptr [_t359], _t360
	lea _t361, qword ptr [_t10 + 8]
	mov _t2, _t361
	mov _t188, n
	mov rdi, _t188
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t362, _RV1
	mov _t187, _t362
	mov _t5, _t187
	mov _t363, 8
	mov _t364, _t2
	sub _t364, _t363
	mov _t365, qword ptr [_t364]
	mov _t1, _t365
	mov _t366, 8
	mov _t367, _t5
	sub _t367, _t366
	mov _t368, qword ptr [_t367]
	mov _t4, _t368
	lea _t369, qword ptr [_t1 + _t4]
	mov _t8, _t369
	mov _t370, 8
	lea _t371, qword ptr [_t370 + _t8 * 8]
	mov _t190, _t371
	mov rdi, _t190
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t372, _RV1
	mov _t189, _t372
	mov _t7, _t189
	mov qword ptr [_t7], _t8
	lea _t373, qword ptr [_t7 + 8]
	mov _t6, _t373
	mov _t374, 0
	mov _t0, _t374
	_l5:
	cmp _t0, _t1
	jge _l3
	_l4:
	lea _t375, qword ptr [_t6 + _t0 * 8]
	mov _t191, _t375
	mov _t376, qword ptr [_t2 + _t0 * 8]
	mov qword ptr [_t191], _t376
	lea _t377, qword ptr [_t0 + 1]
	mov _t0, _t377
	jmp _l5
	_l3:
	lea _t378, qword ptr [_t6 + _t1 * 8]
	mov _t9, _t378
	mov _t379, 0
	mov _t3, _t379
	_l2:
	cmp _t3, _t4
	jge _l0
	_l1:
	lea _t380, qword ptr [_t9 + _t3 * 8]
	mov _t192, _t380
	mov _t381, qword ptr [_t5 + _t3 * 8]
	mov qword ptr [_t192], _t381
	lea _t382, qword ptr [_t3 + 1]
	mov _t3, _t382
	jmp _l2
	_l0:
	mov _t184, _t6
	mov rdi, _t184
	and rsp, -16
	call _Iprintln_pai
	mov _t383, 24
	mov _t195, _t383
	mov rdi, _t195
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t384, _RV1
	mov _t194, _t384
	mov _t21, _t194
	mov _t385, 2
	mov qword ptr [_t21], _t385
	lea _t386, qword ptr [_t21 + 8]
	mov _t387, 109
	mov qword ptr [_t386], _t387
	lea _t388, qword ptr [_t21 + 16]
	mov _t389, 32
	mov qword ptr [_t388], _t389
	lea _t390, qword ptr [_t21 + 8]
	mov _t13, _t390
	mov _t197, m
	mov rdi, _t197
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t391, _RV1
	mov _t196, _t391
	mov _t16, _t196
	mov _t392, 8
	mov _t393, _t13
	sub _t393, _t392
	mov _t394, qword ptr [_t393]
	mov _t12, _t394
	mov _t395, 8
	mov _t396, _t16
	sub _t396, _t395
	mov _t397, qword ptr [_t396]
	mov _t15, _t397
	lea _t398, qword ptr [_t12 + _t15]
	mov _t19, _t398
	mov _t399, 8
	lea _t400, qword ptr [_t399 + _t19 * 8]
	mov _t199, _t400
	mov rdi, _t199
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t401, _RV1
	mov _t198, _t401
	mov _t18, _t198
	mov qword ptr [_t18], _t19
	lea _t402, qword ptr [_t18 + 8]
	mov _t17, _t402
	mov _t403, 0
	mov _t11, _t403
	_l11:
	cmp _t11, _t12
	jge _l9
	_l10:
	lea _t404, qword ptr [_t17 + _t11 * 8]
	mov _t200, _t404
	mov _t405, qword ptr [_t13 + _t11 * 8]
	mov qword ptr [_t200], _t405
	lea _t406, qword ptr [_t11 + 1]
	mov _t11, _t406
	jmp _l11
	_l9:
	lea _t407, qword ptr [_t17 + _t12 * 8]
	mov _t20, _t407
	mov _t408, 0
	mov _t14, _t408
	_l8:
	cmp _t14, _t15
	jge _l6
	_l7:
	lea _t409, qword ptr [_t20 + _t14 * 8]
	mov _t201, _t409
	mov _t410, qword ptr [_t16 + _t14 * 8]
	mov qword ptr [_t201], _t410
	lea _t411, qword ptr [_t14 + 1]
	mov _t14, _t411
	jmp _l8
	_l6:
	mov _t193, _t17
	mov rdi, _t193
	and rsp, -16
	call _Iprintln_pai
	mov _t412, 24
	mov _t204, _t412
	mov rdi, _t204
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t413, _RV1
	mov _t203, _t413
	mov _t32, _t203
	mov _t414, 2
	mov qword ptr [_t32], _t414
	lea _t415, qword ptr [_t32 + 8]
	mov _t416, 111
	mov qword ptr [_t415], _t416
	lea _t417, qword ptr [_t32 + 16]
	mov _t418, 32
	mov qword ptr [_t417], _t418
	lea _t419, qword ptr [_t32 + 8]
	mov _t24, _t419
	mov _t206, o
	mov rdi, _t206
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t420, _RV1
	mov _t205, _t420
	mov _t27, _t205
	mov _t421, 8
	mov _t422, _t24
	sub _t422, _t421
	mov _t423, qword ptr [_t422]
	mov _t23, _t423
	mov _t424, 8
	mov _t425, _t27
	sub _t425, _t424
	mov _t426, qword ptr [_t425]
	mov _t26, _t426
	lea _t427, qword ptr [_t23 + _t26]
	mov _t30, _t427
	mov _t428, 8
	lea _t429, qword ptr [_t428 + _t30 * 8]
	mov _t208, _t429
	mov rdi, _t208
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t430, _RV1
	mov _t207, _t430
	mov _t29, _t207
	mov qword ptr [_t29], _t30
	lea _t431, qword ptr [_t29 + 8]
	mov _t28, _t431
	mov _t432, 0
	mov _t22, _t432
	_l17:
	cmp _t22, _t23
	jge _l15
	_l16:
	lea _t433, qword ptr [_t28 + _t22 * 8]
	mov _t209, _t433
	mov _t434, qword ptr [_t24 + _t22 * 8]
	mov qword ptr [_t209], _t434
	lea _t435, qword ptr [_t22 + 1]
	mov _t22, _t435
	jmp _l17
	_l15:
	lea _t436, qword ptr [_t28 + _t23 * 8]
	mov _t31, _t436
	mov _t437, 0
	mov _t25, _t437
	_l14:
	cmp _t25, _t26
	jge _l12
	_l13:
	lea _t438, qword ptr [_t31 + _t25 * 8]
	mov _t210, _t438
	mov _t439, qword ptr [_t27 + _t25 * 8]
	mov qword ptr [_t210], _t439
	lea _t440, qword ptr [_t25 + 1]
	mov _t25, _t440
	jmp _l14
	_l12:
	mov _t202, _t28
	mov rdi, _t202
	and rsp, -16
	call _Iprintln_pai
	mov _t441, 24
	mov _t213, _t441
	mov rdi, _t213
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t442, _RV1
	mov _t212, _t442
	mov _t43, _t212
	mov _t443, 2
	mov qword ptr [_t43], _t443
	lea _t444, qword ptr [_t43 + 8]
	mov _t445, 112
	mov qword ptr [_t444], _t445
	lea _t446, qword ptr [_t43 + 16]
	mov _t447, 32
	mov qword ptr [_t446], _t447
	lea _t448, qword ptr [_t43 + 8]
	mov _t35, _t448
	mov _t215, p
	mov rdi, _t215
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t449, _RV1
	mov _t214, _t449
	mov _t38, _t214
	mov _t450, 8
	mov _t451, _t35
	sub _t451, _t450
	mov _t452, qword ptr [_t451]
	mov _t34, _t452
	mov _t453, 8
	mov _t454, _t38
	sub _t454, _t453
	mov _t455, qword ptr [_t454]
	mov _t37, _t455
	lea _t456, qword ptr [_t34 + _t37]
	mov _t41, _t456
	mov _t457, 8
	lea _t458, qword ptr [_t457 + _t41 * 8]
	mov _t217, _t458
	mov rdi, _t217
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t459, _RV1
	mov _t216, _t459
	mov _t40, _t216
	mov qword ptr [_t40], _t41
	lea _t460, qword ptr [_t40 + 8]
	mov _t39, _t460
	mov _t461, 0
	mov _t33, _t461
	_l23:
	cmp _t33, _t34
	jge _l21
	_l22:
	lea _t462, qword ptr [_t39 + _t33 * 8]
	mov _t218, _t462
	mov _t463, qword ptr [_t35 + _t33 * 8]
	mov qword ptr [_t218], _t463
	lea _t464, qword ptr [_t33 + 1]
	mov _t33, _t464
	jmp _l23
	_l21:
	lea _t465, qword ptr [_t39 + _t34 * 8]
	mov _t42, _t465
	mov _t466, 0
	mov _t36, _t466
	_l20:
	cmp _t36, _t37
	jge _l18
	_l19:
	lea _t467, qword ptr [_t42 + _t36 * 8]
	mov _t219, _t467
	mov _t468, qword ptr [_t38 + _t36 * 8]
	mov qword ptr [_t219], _t468
	lea _t469, qword ptr [_t36 + 1]
	mov _t36, _t469
	jmp _l20
	_l18:
	mov _t211, _t39
	mov rdi, _t211
	and rsp, -16
	call _Iprintln_pai
	mov _t470, 24
	mov _t222, _t470
	mov rdi, _t222
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t471, _RV1
	mov _t221, _t471
	mov _t54, _t221
	mov _t472, 2
	mov qword ptr [_t54], _t472
	lea _t473, qword ptr [_t54 + 8]
	mov _t474, 113
	mov qword ptr [_t473], _t474
	lea _t475, qword ptr [_t54 + 16]
	mov _t476, 32
	mov qword ptr [_t475], _t476
	lea _t477, qword ptr [_t54 + 8]
	mov _t46, _t477
	mov _t224, q
	mov rdi, _t224
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t478, _RV1
	mov _t223, _t478
	mov _t49, _t223
	mov _t479, 8
	mov _t480, _t46
	sub _t480, _t479
	mov _t481, qword ptr [_t480]
	mov _t45, _t481
	mov _t482, 8
	mov _t483, _t49
	sub _t483, _t482
	mov _t484, qword ptr [_t483]
	mov _t48, _t484
	lea _t485, qword ptr [_t45 + _t48]
	mov _t52, _t485
	mov _t486, 8
	lea _t487, qword ptr [_t486 + _t52 * 8]
	mov _t226, _t487
	mov rdi, _t226
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t488, _RV1
	mov _t225, _t488
	mov _t51, _t225
	mov qword ptr [_t51], _t52
	lea _t489, qword ptr [_t51 + 8]
	mov _t50, _t489
	mov _t490, 0
	mov _t44, _t490
	_l29:
	cmp _t44, _t45
	jge _l27
	_l28:
	lea _t491, qword ptr [_t50 + _t44 * 8]
	mov _t227, _t491
	mov _t492, qword ptr [_t46 + _t44 * 8]
	mov qword ptr [_t227], _t492
	lea _t493, qword ptr [_t44 + 1]
	mov _t44, _t493
	jmp _l29
	_l27:
	lea _t494, qword ptr [_t50 + _t45 * 8]
	mov _t53, _t494
	mov _t495, 0
	mov _t47, _t495
	_l26:
	cmp _t47, _t48
	jge _l24
	_l25:
	lea _t496, qword ptr [_t53 + _t47 * 8]
	mov _t228, _t496
	mov _t497, qword ptr [_t49 + _t47 * 8]
	mov qword ptr [_t228], _t497
	lea _t498, qword ptr [_t47 + 1]
	mov _t47, _t498
	jmp _l26
	_l24:
	mov _t220, _t50
	mov rdi, _t220
	and rsp, -16
	call _Iprintln_pai
	mov _t499, 24
	mov _t231, _t499
	mov rdi, _t231
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t500, _RV1
	mov _t230, _t500
	mov _t65, _t230
	mov _t501, 2
	mov qword ptr [_t65], _t501
	lea _t502, qword ptr [_t65 + 8]
	mov _t503, 114
	mov qword ptr [_t502], _t503
	lea _t504, qword ptr [_t65 + 16]
	mov _t505, 32
	mov qword ptr [_t504], _t505
	lea _t506, qword ptr [_t65 + 8]
	mov _t57, _t506
	mov _t233, r
	mov rdi, _t233
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t507, _RV1
	mov _t232, _t507
	mov _t60, _t232
	mov _t508, 8
	mov _t509, _t57
	sub _t509, _t508
	mov _t510, qword ptr [_t509]
	mov _t56, _t510
	mov _t511, 8
	mov _t512, _t60
	sub _t512, _t511
	mov _t513, qword ptr [_t512]
	mov _t59, _t513
	lea _t514, qword ptr [_t56 + _t59]
	mov _t63, _t514
	mov _t515, 8
	lea _t516, qword ptr [_t515 + _t63 * 8]
	mov _t235, _t516
	mov rdi, _t235
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t517, _RV1
	mov _t234, _t517
	mov _t62, _t234
	mov qword ptr [_t62], _t63
	lea _t518, qword ptr [_t62 + 8]
	mov _t61, _t518
	mov _t519, 0
	mov _t55, _t519
	_l35:
	cmp _t55, _t56
	jge _l33
	_l34:
	lea _t520, qword ptr [_t61 + _t55 * 8]
	mov _t236, _t520
	mov _t521, qword ptr [_t57 + _t55 * 8]
	mov qword ptr [_t236], _t521
	lea _t522, qword ptr [_t55 + 1]
	mov _t55, _t522
	jmp _l35
	_l33:
	lea _t523, qword ptr [_t61 + _t56 * 8]
	mov _t64, _t523
	mov _t524, 0
	mov _t58, _t524
	_l32:
	cmp _t58, _t59
	jge _l30
	_l31:
	lea _t525, qword ptr [_t64 + _t58 * 8]
	mov _t237, _t525
	mov _t526, qword ptr [_t60 + _t58 * 8]
	mov qword ptr [_t237], _t526
	lea _t527, qword ptr [_t58 + 1]
	mov _t58, _t527
	jmp _l32
	_l30:
	mov _t229, _t61
	mov rdi, _t229
	and rsp, -16
	call _Iprintln_pai
	mov _t528, 24
	mov _t240, _t528
	mov rdi, _t240
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t529, _RV1
	mov _t239, _t529
	mov _t76, _t239
	mov _t530, 2
	mov qword ptr [_t76], _t530
	lea _t531, qword ptr [_t76 + 8]
	mov _t532, 115
	mov qword ptr [_t531], _t532
	lea _t533, qword ptr [_t76 + 16]
	mov _t534, 32
	mov qword ptr [_t533], _t534
	lea _t535, qword ptr [_t76 + 8]
	mov _t68, _t535
	mov _t242, s
	mov rdi, _t242
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t536, _RV1
	mov _t241, _t536
	mov _t71, _t241
	mov _t537, 8
	mov _t538, _t68
	sub _t538, _t537
	mov _t539, qword ptr [_t538]
	mov _t67, _t539
	mov _t540, 8
	mov _t541, _t71
	sub _t541, _t540
	mov _t542, qword ptr [_t541]
	mov _t70, _t542
	lea _t543, qword ptr [_t67 + _t70]
	mov _t74, _t543
	mov _t544, 8
	lea _t545, qword ptr [_t544 + _t74 * 8]
	mov _t244, _t545
	mov rdi, _t244
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t546, _RV1
	mov _t243, _t546
	mov _t73, _t243
	mov qword ptr [_t73], _t74
	lea _t547, qword ptr [_t73 + 8]
	mov _t72, _t547
	mov _t548, 0
	mov _t66, _t548
	_l41:
	cmp _t66, _t67
	jge _l39
	_l40:
	lea _t549, qword ptr [_t72 + _t66 * 8]
	mov _t245, _t549
	mov _t550, qword ptr [_t68 + _t66 * 8]
	mov qword ptr [_t245], _t550
	lea _t551, qword ptr [_t66 + 1]
	mov _t66, _t551
	jmp _l41
	_l39:
	lea _t552, qword ptr [_t72 + _t67 * 8]
	mov _t75, _t552
	mov _t553, 0
	mov _t69, _t553
	_l38:
	cmp _t69, _t70
	jge _l36
	_l37:
	lea _t554, qword ptr [_t75 + _t69 * 8]
	mov _t246, _t554
	mov _t555, qword ptr [_t71 + _t69 * 8]
	mov qword ptr [_t246], _t555
	lea _t556, qword ptr [_t69 + 1]
	mov _t69, _t556
	jmp _l38
	_l36:
	mov _t238, _t72
	mov rdi, _t238
	and rsp, -16
	call _Iprintln_pai
	mov _t557, 24
	mov _t249, _t557
	mov rdi, _t249
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t558, _RV1
	mov _t248, _t558
	mov _t87, _t248
	mov _t559, 2
	mov qword ptr [_t87], _t559
	lea _t560, qword ptr [_t87 + 8]
	mov _t561, 116
	mov qword ptr [_t560], _t561
	lea _t562, qword ptr [_t87 + 16]
	mov _t563, 32
	mov qword ptr [_t562], _t563
	lea _t564, qword ptr [_t87 + 8]
	mov _t79, _t564
	mov _t251, t
	mov rdi, _t251
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t565, _RV1
	mov _t250, _t565
	mov _t82, _t250
	mov _t566, 8
	mov _t567, _t79
	sub _t567, _t566
	mov _t568, qword ptr [_t567]
	mov _t78, _t568
	mov _t569, 8
	mov _t570, _t82
	sub _t570, _t569
	mov _t571, qword ptr [_t570]
	mov _t81, _t571
	lea _t572, qword ptr [_t78 + _t81]
	mov _t85, _t572
	mov _t573, 8
	lea _t574, qword ptr [_t573 + _t85 * 8]
	mov _t253, _t574
	mov rdi, _t253
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t575, _RV1
	mov _t252, _t575
	mov _t84, _t252
	mov qword ptr [_t84], _t85
	lea _t576, qword ptr [_t84 + 8]
	mov _t83, _t576
	mov _t577, 0
	mov _t77, _t577
	_l47:
	cmp _t77, _t78
	jge _l45
	_l46:
	lea _t578, qword ptr [_t83 + _t77 * 8]
	mov _t254, _t578
	mov _t579, qword ptr [_t79 + _t77 * 8]
	mov qword ptr [_t254], _t579
	lea _t580, qword ptr [_t77 + 1]
	mov _t77, _t580
	jmp _l47
	_l45:
	lea _t581, qword ptr [_t83 + _t78 * 8]
	mov _t86, _t581
	mov _t582, 0
	mov _t80, _t582
	_l44:
	cmp _t80, _t81
	jge _l42
	_l43:
	lea _t583, qword ptr [_t86 + _t80 * 8]
	mov _t255, _t583
	mov _t584, qword ptr [_t82 + _t80 * 8]
	mov qword ptr [_t255], _t584
	lea _t585, qword ptr [_t80 + 1]
	mov _t80, _t585
	jmp _l44
	_l42:
	mov _t247, _t83
	mov rdi, _t247
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
_Ifoo_t8iiiiiiiiiiiiiiii:
	mov _t586, rdi
	mov _ARG1, rsi
	mov _ARG2, rdx
	mov _ARG3, rcx
	mov _ARG4, r8
	mov _ARG5, r9
	pop _ARG6
	pop _ARG7
	pop _ARG8
	mov _t587, _ARG1
	mov a, _t587
	mov _t588, _ARG2
	mov b, _t588
	mov _t589, _ARG3
	mov c, _t589
	mov _t590, _ARG4
	mov d, _t590
	mov _t591, _ARG5
	mov e, _t591
	mov _t592, _ARG6
	mov f, _t592
	mov _t593, _ARG7
	mov g, _t593
	mov _t594, _ARG8
	mov h, _t594
	mov _t595, 24
	mov _t258, _t595
	mov rdi, _t258
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t596, _RV1
	mov _t257, _t596
	mov _t98, _t257
	mov _t597, 2
	mov qword ptr [_t98], _t597
	lea _t598, qword ptr [_t98 + 8]
	mov _t599, 97
	mov qword ptr [_t598], _t599
	lea _t600, qword ptr [_t98 + 16]
	mov _t601, 32
	mov qword ptr [_t600], _t601
	lea _t602, qword ptr [_t98 + 8]
	mov _t90, _t602
	mov _t260, a
	mov rdi, _t260
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t603, _RV1
	mov _t259, _t603
	mov _t93, _t259
	mov _t604, 8
	mov _t605, _t90
	sub _t605, _t604
	mov _t606, qword ptr [_t605]
	mov _t89, _t606
	mov _t607, 8
	mov _t608, _t93
	sub _t608, _t607
	mov _t609, qword ptr [_t608]
	mov _t92, _t609
	lea _t610, qword ptr [_t89 + _t92]
	mov _t96, _t610
	mov _t611, 8
	lea _t612, qword ptr [_t611 + _t96 * 8]
	mov _t262, _t612
	mov rdi, _t262
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t613, _RV1
	mov _t261, _t613
	mov _t95, _t261
	mov qword ptr [_t95], _t96
	lea _t614, qword ptr [_t95 + 8]
	mov _t94, _t614
	mov _t615, 0
	mov _t88, _t615
	_l53:
	cmp _t88, _t89
	jge _l51
	_l52:
	lea _t616, qword ptr [_t94 + _t88 * 8]
	mov _t263, _t616
	mov _t617, qword ptr [_t90 + _t88 * 8]
	mov qword ptr [_t263], _t617
	lea _t618, qword ptr [_t88 + 1]
	mov _t88, _t618
	jmp _l53
	_l51:
	lea _t619, qword ptr [_t94 + _t89 * 8]
	mov _t97, _t619
	mov _t620, 0
	mov _t91, _t620
	_l50:
	cmp _t91, _t92
	jge _l48
	_l49:
	lea _t621, qword ptr [_t97 + _t91 * 8]
	mov _t264, _t621
	mov _t622, qword ptr [_t93 + _t91 * 8]
	mov qword ptr [_t264], _t622
	lea _t623, qword ptr [_t91 + 1]
	mov _t91, _t623
	jmp _l50
	_l48:
	mov _t256, _t94
	mov rdi, _t256
	and rsp, -16
	call _Iprintln_pai
	mov _t624, 24
	mov _t267, _t624
	mov rdi, _t267
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t625, _RV1
	mov _t266, _t625
	mov _t109, _t266
	mov _t626, 2
	mov qword ptr [_t109], _t626
	lea _t627, qword ptr [_t109 + 8]
	mov _t628, 98
	mov qword ptr [_t627], _t628
	lea _t629, qword ptr [_t109 + 16]
	mov _t630, 32
	mov qword ptr [_t629], _t630
	lea _t631, qword ptr [_t109 + 8]
	mov _t101, _t631
	mov _t269, b
	mov rdi, _t269
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t632, _RV1
	mov _t268, _t632
	mov _t104, _t268
	mov _t633, 8
	mov _t634, _t101
	sub _t634, _t633
	mov _t635, qword ptr [_t634]
	mov _t100, _t635
	mov _t636, 8
	mov _t637, _t104
	sub _t637, _t636
	mov _t638, qword ptr [_t637]
	mov _t103, _t638
	lea _t639, qword ptr [_t100 + _t103]
	mov _t107, _t639
	mov _t640, 8
	lea _t641, qword ptr [_t640 + _t107 * 8]
	mov _t271, _t641
	mov rdi, _t271
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t642, _RV1
	mov _t270, _t642
	mov _t106, _t270
	mov qword ptr [_t106], _t107
	lea _t643, qword ptr [_t106 + 8]
	mov _t105, _t643
	mov _t644, 0
	mov _t99, _t644
	_l59:
	cmp _t99, _t100
	jge _l57
	_l58:
	lea _t645, qword ptr [_t105 + _t99 * 8]
	mov _t272, _t645
	mov _t646, qword ptr [_t101 + _t99 * 8]
	mov qword ptr [_t272], _t646
	lea _t647, qword ptr [_t99 + 1]
	mov _t99, _t647
	jmp _l59
	_l57:
	lea _t648, qword ptr [_t105 + _t100 * 8]
	mov _t108, _t648
	mov _t649, 0
	mov _t102, _t649
	_l56:
	cmp _t102, _t103
	jge _l54
	_l55:
	lea _t650, qword ptr [_t108 + _t102 * 8]
	mov _t273, _t650
	mov _t651, qword ptr [_t104 + _t102 * 8]
	mov qword ptr [_t273], _t651
	lea _t652, qword ptr [_t102 + 1]
	mov _t102, _t652
	jmp _l56
	_l54:
	mov _t265, _t105
	mov rdi, _t265
	and rsp, -16
	call _Iprintln_pai
	mov _t653, 24
	mov _t276, _t653
	mov rdi, _t276
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t654, _RV1
	mov _t275, _t654
	mov _t120, _t275
	mov _t655, 2
	mov qword ptr [_t120], _t655
	lea _t656, qword ptr [_t120 + 8]
	mov _t657, 99
	mov qword ptr [_t656], _t657
	lea _t658, qword ptr [_t120 + 16]
	mov _t659, 32
	mov qword ptr [_t658], _t659
	lea _t660, qword ptr [_t120 + 8]
	mov _t112, _t660
	mov _t278, c
	mov rdi, _t278
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t661, _RV1
	mov _t277, _t661
	mov _t115, _t277
	mov _t662, 8
	mov _t663, _t112
	sub _t663, _t662
	mov _t664, qword ptr [_t663]
	mov _t111, _t664
	mov _t665, 8
	mov _t666, _t115
	sub _t666, _t665
	mov _t667, qword ptr [_t666]
	mov _t114, _t667
	lea _t668, qword ptr [_t111 + _t114]
	mov _t118, _t668
	mov _t669, 8
	lea _t670, qword ptr [_t669 + _t118 * 8]
	mov _t280, _t670
	mov rdi, _t280
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t671, _RV1
	mov _t279, _t671
	mov _t117, _t279
	mov qword ptr [_t117], _t118
	lea _t672, qword ptr [_t117 + 8]
	mov _t116, _t672
	mov _t673, 0
	mov _t110, _t673
	_l65:
	cmp _t110, _t111
	jge _l63
	_l64:
	lea _t674, qword ptr [_t116 + _t110 * 8]
	mov _t281, _t674
	mov _t675, qword ptr [_t112 + _t110 * 8]
	mov qword ptr [_t281], _t675
	lea _t676, qword ptr [_t110 + 1]
	mov _t110, _t676
	jmp _l65
	_l63:
	lea _t677, qword ptr [_t116 + _t111 * 8]
	mov _t119, _t677
	mov _t678, 0
	mov _t113, _t678
	_l62:
	cmp _t113, _t114
	jge _l60
	_l61:
	lea _t679, qword ptr [_t119 + _t113 * 8]
	mov _t282, _t679
	mov _t680, qword ptr [_t115 + _t113 * 8]
	mov qword ptr [_t282], _t680
	lea _t681, qword ptr [_t113 + 1]
	mov _t113, _t681
	jmp _l62
	_l60:
	mov _t274, _t116
	mov rdi, _t274
	and rsp, -16
	call _Iprintln_pai
	mov _t682, 24
	mov _t285, _t682
	mov rdi, _t285
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t683, _RV1
	mov _t284, _t683
	mov _t131, _t284
	mov _t684, 2
	mov qword ptr [_t131], _t684
	lea _t685, qword ptr [_t131 + 8]
	mov _t686, 100
	mov qword ptr [_t685], _t686
	lea _t687, qword ptr [_t131 + 16]
	mov _t688, 32
	mov qword ptr [_t687], _t688
	lea _t689, qword ptr [_t131 + 8]
	mov _t123, _t689
	mov _t287, d
	mov rdi, _t287
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t690, _RV1
	mov _t286, _t690
	mov _t126, _t286
	mov _t691, 8
	mov _t692, _t123
	sub _t692, _t691
	mov _t693, qword ptr [_t692]
	mov _t122, _t693
	mov _t694, 8
	mov _t695, _t126
	sub _t695, _t694
	mov _t696, qword ptr [_t695]
	mov _t125, _t696
	lea _t697, qword ptr [_t122 + _t125]
	mov _t129, _t697
	mov _t698, 8
	lea _t699, qword ptr [_t698 + _t129 * 8]
	mov _t289, _t699
	mov rdi, _t289
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t700, _RV1
	mov _t288, _t700
	mov _t128, _t288
	mov qword ptr [_t128], _t129
	lea _t701, qword ptr [_t128 + 8]
	mov _t127, _t701
	mov _t702, 0
	mov _t121, _t702
	_l71:
	cmp _t121, _t122
	jge _l69
	_l70:
	lea _t703, qword ptr [_t127 + _t121 * 8]
	mov _t290, _t703
	mov _t704, qword ptr [_t123 + _t121 * 8]
	mov qword ptr [_t290], _t704
	lea _t705, qword ptr [_t121 + 1]
	mov _t121, _t705
	jmp _l71
	_l69:
	lea _t706, qword ptr [_t127 + _t122 * 8]
	mov _t130, _t706
	mov _t707, 0
	mov _t124, _t707
	_l68:
	cmp _t124, _t125
	jge _l66
	_l67:
	lea _t708, qword ptr [_t130 + _t124 * 8]
	mov _t291, _t708
	mov _t709, qword ptr [_t126 + _t124 * 8]
	mov qword ptr [_t291], _t709
	lea _t710, qword ptr [_t124 + 1]
	mov _t124, _t710
	jmp _l68
	_l66:
	mov _t283, _t127
	mov rdi, _t283
	and rsp, -16
	call _Iprintln_pai
	mov _t711, 24
	mov _t294, _t711
	mov rdi, _t294
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t712, _RV1
	mov _t293, _t712
	mov _t142, _t293
	mov _t713, 2
	mov qword ptr [_t142], _t713
	lea _t714, qword ptr [_t142 + 8]
	mov _t715, 101
	mov qword ptr [_t714], _t715
	lea _t716, qword ptr [_t142 + 16]
	mov _t717, 32
	mov qword ptr [_t716], _t717
	lea _t718, qword ptr [_t142 + 8]
	mov _t134, _t718
	mov _t296, e
	mov rdi, _t296
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t719, _RV1
	mov _t295, _t719
	mov _t137, _t295
	mov _t720, 8
	mov _t721, _t134
	sub _t721, _t720
	mov _t722, qword ptr [_t721]
	mov _t133, _t722
	mov _t723, 8
	mov _t724, _t137
	sub _t724, _t723
	mov _t725, qword ptr [_t724]
	mov _t136, _t725
	lea _t726, qword ptr [_t133 + _t136]
	mov _t140, _t726
	mov _t727, 8
	lea _t728, qword ptr [_t727 + _t140 * 8]
	mov _t298, _t728
	mov rdi, _t298
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t729, _RV1
	mov _t297, _t729
	mov _t139, _t297
	mov qword ptr [_t139], _t140
	lea _t730, qword ptr [_t139 + 8]
	mov _t138, _t730
	mov _t731, 0
	mov _t132, _t731
	_l77:
	cmp _t132, _t133
	jge _l75
	_l76:
	lea _t732, qword ptr [_t138 + _t132 * 8]
	mov _t299, _t732
	mov _t733, qword ptr [_t134 + _t132 * 8]
	mov qword ptr [_t299], _t733
	lea _t734, qword ptr [_t132 + 1]
	mov _t132, _t734
	jmp _l77
	_l75:
	lea _t735, qword ptr [_t138 + _t133 * 8]
	mov _t141, _t735
	mov _t736, 0
	mov _t135, _t736
	_l74:
	cmp _t135, _t136
	jge _l72
	_l73:
	lea _t737, qword ptr [_t141 + _t135 * 8]
	mov _t300, _t737
	mov _t738, qword ptr [_t137 + _t135 * 8]
	mov qword ptr [_t300], _t738
	lea _t739, qword ptr [_t135 + 1]
	mov _t135, _t739
	jmp _l74
	_l72:
	mov _t292, _t138
	mov rdi, _t292
	and rsp, -16
	call _Iprintln_pai
	mov _t740, 24
	mov _t303, _t740
	mov rdi, _t303
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t741, _RV1
	mov _t302, _t741
	mov _t153, _t302
	mov _t742, 2
	mov qword ptr [_t153], _t742
	lea _t743, qword ptr [_t153 + 8]
	mov _t744, 102
	mov qword ptr [_t743], _t744
	lea _t745, qword ptr [_t153 + 16]
	mov _t746, 32
	mov qword ptr [_t745], _t746
	lea _t747, qword ptr [_t153 + 8]
	mov _t145, _t747
	mov _t305, f
	mov rdi, _t305
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t748, _RV1
	mov _t304, _t748
	mov _t148, _t304
	mov _t749, 8
	mov _t750, _t145
	sub _t750, _t749
	mov _t751, qword ptr [_t750]
	mov _t144, _t751
	mov _t752, 8
	mov _t753, _t148
	sub _t753, _t752
	mov _t754, qword ptr [_t753]
	mov _t147, _t754
	lea _t755, qword ptr [_t144 + _t147]
	mov _t151, _t755
	mov _t756, 8
	lea _t757, qword ptr [_t756 + _t151 * 8]
	mov _t307, _t757
	mov rdi, _t307
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t758, _RV1
	mov _t306, _t758
	mov _t150, _t306
	mov qword ptr [_t150], _t151
	lea _t759, qword ptr [_t150 + 8]
	mov _t149, _t759
	mov _t760, 0
	mov _t143, _t760
	_l83:
	cmp _t143, _t144
	jge _l81
	_l82:
	lea _t761, qword ptr [_t149 + _t143 * 8]
	mov _t308, _t761
	mov _t762, qword ptr [_t145 + _t143 * 8]
	mov qword ptr [_t308], _t762
	lea _t763, qword ptr [_t143 + 1]
	mov _t143, _t763
	jmp _l83
	_l81:
	lea _t764, qword ptr [_t149 + _t144 * 8]
	mov _t152, _t764
	mov _t765, 0
	mov _t146, _t765
	_l80:
	cmp _t146, _t147
	jge _l78
	_l79:
	lea _t766, qword ptr [_t152 + _t146 * 8]
	mov _t309, _t766
	mov _t767, qword ptr [_t148 + _t146 * 8]
	mov qword ptr [_t309], _t767
	lea _t768, qword ptr [_t146 + 1]
	mov _t146, _t768
	jmp _l80
	_l78:
	mov _t301, _t149
	mov rdi, _t301
	and rsp, -16
	call _Iprintln_pai
	mov _t769, 24
	mov _t312, _t769
	mov rdi, _t312
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t770, _RV1
	mov _t311, _t770
	mov _t164, _t311
	mov _t771, 2
	mov qword ptr [_t164], _t771
	lea _t772, qword ptr [_t164 + 8]
	mov _t773, 103
	mov qword ptr [_t772], _t773
	lea _t774, qword ptr [_t164 + 16]
	mov _t775, 32
	mov qword ptr [_t774], _t775
	lea _t776, qword ptr [_t164 + 8]
	mov _t156, _t776
	mov _t314, g
	mov rdi, _t314
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t777, _RV1
	mov _t313, _t777
	mov _t159, _t313
	mov _t778, 8
	mov _t779, _t156
	sub _t779, _t778
	mov _t780, qword ptr [_t779]
	mov _t155, _t780
	mov _t781, 8
	mov _t782, _t159
	sub _t782, _t781
	mov _t783, qword ptr [_t782]
	mov _t158, _t783
	lea _t784, qword ptr [_t155 + _t158]
	mov _t162, _t784
	mov _t785, 8
	lea _t786, qword ptr [_t785 + _t162 * 8]
	mov _t316, _t786
	mov rdi, _t316
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t787, _RV1
	mov _t315, _t787
	mov _t161, _t315
	mov qword ptr [_t161], _t162
	lea _t788, qword ptr [_t161 + 8]
	mov _t160, _t788
	mov _t789, 0
	mov _t154, _t789
	_l89:
	cmp _t154, _t155
	jge _l87
	_l88:
	lea _t790, qword ptr [_t160 + _t154 * 8]
	mov _t317, _t790
	mov _t791, qword ptr [_t156 + _t154 * 8]
	mov qword ptr [_t317], _t791
	lea _t792, qword ptr [_t154 + 1]
	mov _t154, _t792
	jmp _l89
	_l87:
	lea _t793, qword ptr [_t160 + _t155 * 8]
	mov _t163, _t793
	mov _t794, 0
	mov _t157, _t794
	_l86:
	cmp _t157, _t158
	jge _l84
	_l85:
	lea _t795, qword ptr [_t163 + _t157 * 8]
	mov _t318, _t795
	mov _t796, qword ptr [_t159 + _t157 * 8]
	mov qword ptr [_t318], _t796
	lea _t797, qword ptr [_t157 + 1]
	mov _t157, _t797
	jmp _l86
	_l84:
	mov _t310, _t160
	mov rdi, _t310
	and rsp, -16
	call _Iprintln_pai
	mov _t798, 24
	mov _t321, _t798
	mov rdi, _t321
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t799, _RV1
	mov _t320, _t799
	mov _t175, _t320
	mov _t800, 2
	mov qword ptr [_t175], _t800
	lea _t801, qword ptr [_t175 + 8]
	mov _t802, 104
	mov qword ptr [_t801], _t802
	lea _t803, qword ptr [_t175 + 16]
	mov _t804, 32
	mov qword ptr [_t803], _t804
	lea _t805, qword ptr [_t175 + 8]
	mov _t167, _t805
	mov _t323, h
	mov rdi, _t323
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t806, _RV1
	mov _t322, _t806
	mov _t170, _t322
	mov _t807, 8
	mov _t808, _t167
	sub _t808, _t807
	mov _t809, qword ptr [_t808]
	mov _t166, _t809
	mov _t810, 8
	mov _t811, _t170
	sub _t811, _t810
	mov _t812, qword ptr [_t811]
	mov _t169, _t812
	lea _t813, qword ptr [_t166 + _t169]
	mov _t173, _t813
	mov _t814, 8
	lea _t815, qword ptr [_t814 + _t173 * 8]
	mov _t325, _t815
	mov rdi, _t325
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t816, _RV1
	mov _t324, _t816
	mov _t172, _t324
	mov qword ptr [_t172], _t173
	lea _t817, qword ptr [_t172 + 8]
	mov _t171, _t817
	mov _t818, 0
	mov _t165, _t818
	_l95:
	cmp _t165, _t166
	jge _l93
	_l94:
	lea _t819, qword ptr [_t171 + _t165 * 8]
	mov _t326, _t819
	mov _t820, qword ptr [_t167 + _t165 * 8]
	mov qword ptr [_t326], _t820
	lea _t821, qword ptr [_t165 + 1]
	mov _t165, _t821
	jmp _l95
	_l93:
	lea _t822, qword ptr [_t171 + _t166 * 8]
	mov _t174, _t822
	mov _t823, 0
	mov _t168, _t823
	_l92:
	cmp _t168, _t169
	jge _l90
	_l91:
	lea _t824, qword ptr [_t174 + _t168 * 8]
	mov _t327, _t824
	mov _t825, qword ptr [_t170 + _t168 * 8]
	mov qword ptr [_t327], _t825
	lea _t826, qword ptr [_t168 + 1]
	mov _t168, _t826
	jmp _l92
	_l90:
	mov _t319, _t171
	mov rdi, _t319
	and rsp, -16
	call _Iprintln_pai
	mov _t827, 22
	mov _t328, _t827
	mov _t828, 24
	mov _t329, _t828
	mov _t829, 3
	mov _t330, _t829
	mov _t830, 4
	mov _t331, _t830
	mov _t831, 5
	mov _t332, _t831
	mov _t832, 6
	mov _t333, _t832
	mov _t833, 7
	mov _t334, _t833
	mov _t834, 28
	mov _t335, _t834
	mov rax, _t328
	mov rdx, _t329
	mov qword ptr [_t586 + 0], _t330
	mov qword ptr [_t586 + 8], _t331
	mov qword ptr [_t586 + 16], _t332
	mov qword ptr [_t586 + 24], _t333
	mov qword ptr [_t586 + 32], _t334
	mov qword ptr [_t586 + 40], _t335
	leave
	ret
