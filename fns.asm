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
	mov _ARG1, rdi
	mov _t321, _ARG1
	mov args, _t321
	mov _t322, 32
	mov _t192, _t322
	mov _t323, 34
	mov _t193, _t323
	mov _t324, 13
	mov _t194, _t324
	mov _t325, 14
	mov _t195, _t325
	mov _t326, 15
	mov _t196, _t326
	mov _t327, 16
	mov _t197, _t327
	mov _t328, 17
	mov _t198, _t328
	mov _t329, 38
	mov _t199, _t329
	sub rsp, 48
	mov rdi, rsp
	push _t199
	push _t198
	push _t197
	mov r9, _t196
	mov r8, _t195
	mov rcx, _t194
	mov rdx, _t193
	mov rsi, _t192
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
	mov _t330, _RV1
	mov n, _t330
	mov _t331, _RV2
	mov m, _t331
	mov _t332, _RV3
	mov o, _t332
	mov _t333, _RV4
	mov p, _t333
	mov _t334, _RV5
	mov q, _t334
	mov _t335, _RV6
	mov r, _t335
	mov _t336, _RV7
	mov s, _t336
	mov _t337, _RV8
	mov t, _t337
	lea _t338, qword ptr _g0[rip]
	mov _t10, _t338
	mov _t339, 8
	lea _t340, qword ptr [_t10 + _t339]
	mov _t11, _t340
	mov _t2, _t11
	mov _t202, n
	mov rdi, _t202
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t341, _RV1
	mov _t201, _t341
	mov _t5, _t201
	mov _t342, 8
	mov _t343, _t2
	sub _t343, _t342
	mov _t343, qword ptr [_t343]
	mov _t1, _t343
	mov _t344, 8
	mov _t345, _t5
	sub _t345, _t344
	mov _t345, qword ptr [_t345]
	mov _t4, _t345
	lea _t346, qword ptr [_t1 + _t4]
	mov _t8, _t346
	mov _t347, 8
	mov _t348, 8
	mov _t349, _t8
	imul _t349, _t348
	lea _t350, qword ptr [_t347 + _t349]
	mov _t204, _t350
	mov rdi, _t204
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t351, _RV1
	mov _t203, _t351
	mov _t7, _t203
	mov qword ptr [_t7], _t8
	mov _t352, 8
	lea _t353, qword ptr [_t7 + _t352]
	mov _t6, _t353
	mov _t354, 0
	mov _t0, _t354
	_l5:
	cmp _t0, _t1
	jge _l3
	_l4:
	mov _t355, 8
	mov _t356, _t0
	imul _t356, _t355
	lea _t357, qword ptr [_t6 + _t356]
	mov _t205, _t357
	mov _t358, 8
	mov _t359, _t0
	imul _t359, _t358
	lea _t360, qword ptr [_t2 + _t359]
	mov _t360, qword ptr [_t360]
	mov qword ptr [_t205], _t360
	mov _t361, 1
	lea _t362, qword ptr [_t0 + _t361]
	mov _t0, _t362
	jmp _l5
	_l3:
	mov _t363, 8
	mov _t364, _t1
	imul _t364, _t363
	lea _t365, qword ptr [_t6 + _t364]
	mov _t9, _t365
	mov _t366, 0
	mov _t3, _t366
	_l2:
	cmp _t3, _t4
	jge _l0
	_l1:
	mov _t367, 8
	mov _t368, _t3
	imul _t368, _t367
	lea _t369, qword ptr [_t9 + _t368]
	mov _t206, _t369
	mov _t370, 8
	mov _t371, _t3
	imul _t371, _t370
	lea _t372, qword ptr [_t5 + _t371]
	mov _t372, qword ptr [_t372]
	mov qword ptr [_t206], _t372
	mov _t373, 1
	lea _t374, qword ptr [_t3 + _t373]
	mov _t3, _t374
	jmp _l2
	_l0:
	mov _t200, _t6
	mov rdi, _t200
	and rsp, -16
	call _Iprintln_pai
	lea _t375, qword ptr _g1[rip]
	mov _t22, _t375
	mov _t376, 8
	lea _t377, qword ptr [_t22 + _t376]
	mov _t23, _t377
	mov _t14, _t23
	mov _t209, m
	mov rdi, _t209
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t378, _RV1
	mov _t208, _t378
	mov _t17, _t208
	mov _t379, 8
	mov _t380, _t14
	sub _t380, _t379
	mov _t380, qword ptr [_t380]
	mov _t13, _t380
	mov _t381, 8
	mov _t382, _t17
	sub _t382, _t381
	mov _t382, qword ptr [_t382]
	mov _t16, _t382
	lea _t383, qword ptr [_t13 + _t16]
	mov _t20, _t383
	mov _t384, 8
	mov _t385, 8
	mov _t386, _t20
	imul _t386, _t385
	lea _t387, qword ptr [_t384 + _t386]
	mov _t211, _t387
	mov rdi, _t211
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t388, _RV1
	mov _t210, _t388
	mov _t19, _t210
	mov qword ptr [_t19], _t20
	mov _t389, 8
	lea _t390, qword ptr [_t19 + _t389]
	mov _t18, _t390
	mov _t391, 0
	mov _t12, _t391
	_l11:
	cmp _t12, _t13
	jge _l9
	_l10:
	mov _t392, 8
	mov _t393, _t12
	imul _t393, _t392
	lea _t394, qword ptr [_t18 + _t393]
	mov _t212, _t394
	mov _t395, 8
	mov _t396, _t12
	imul _t396, _t395
	lea _t397, qword ptr [_t14 + _t396]
	mov _t397, qword ptr [_t397]
	mov qword ptr [_t212], _t397
	mov _t398, 1
	lea _t399, qword ptr [_t12 + _t398]
	mov _t12, _t399
	jmp _l11
	_l9:
	mov _t400, 8
	mov _t401, _t13
	imul _t401, _t400
	lea _t402, qword ptr [_t18 + _t401]
	mov _t21, _t402
	mov _t403, 0
	mov _t15, _t403
	_l8:
	cmp _t15, _t16
	jge _l6
	_l7:
	mov _t404, 8
	mov _t405, _t15
	imul _t405, _t404
	lea _t406, qword ptr [_t21 + _t405]
	mov _t213, _t406
	mov _t407, 8
	mov _t408, _t15
	imul _t408, _t407
	lea _t409, qword ptr [_t17 + _t408]
	mov _t409, qword ptr [_t409]
	mov qword ptr [_t213], _t409
	mov _t410, 1
	lea _t411, qword ptr [_t15 + _t410]
	mov _t15, _t411
	jmp _l8
	_l6:
	mov _t207, _t18
	mov rdi, _t207
	and rsp, -16
	call _Iprintln_pai
	lea _t412, qword ptr _g2[rip]
	mov _t34, _t412
	mov _t413, 8
	lea _t414, qword ptr [_t34 + _t413]
	mov _t35, _t414
	mov _t26, _t35
	mov _t216, o
	mov rdi, _t216
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t415, _RV1
	mov _t215, _t415
	mov _t29, _t215
	mov _t416, 8
	mov _t417, _t26
	sub _t417, _t416
	mov _t417, qword ptr [_t417]
	mov _t25, _t417
	mov _t418, 8
	mov _t419, _t29
	sub _t419, _t418
	mov _t419, qword ptr [_t419]
	mov _t28, _t419
	lea _t420, qword ptr [_t25 + _t28]
	mov _t32, _t420
	mov _t421, 8
	mov _t422, 8
	mov _t423, _t32
	imul _t423, _t422
	lea _t424, qword ptr [_t421 + _t423]
	mov _t218, _t424
	mov rdi, _t218
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t425, _RV1
	mov _t217, _t425
	mov _t31, _t217
	mov qword ptr [_t31], _t32
	mov _t426, 8
	lea _t427, qword ptr [_t31 + _t426]
	mov _t30, _t427
	mov _t428, 0
	mov _t24, _t428
	_l17:
	cmp _t24, _t25
	jge _l15
	_l16:
	mov _t429, 8
	mov _t430, _t24
	imul _t430, _t429
	lea _t431, qword ptr [_t30 + _t430]
	mov _t219, _t431
	mov _t432, 8
	mov _t433, _t24
	imul _t433, _t432
	lea _t434, qword ptr [_t26 + _t433]
	mov _t434, qword ptr [_t434]
	mov qword ptr [_t219], _t434
	mov _t435, 1
	lea _t436, qword ptr [_t24 + _t435]
	mov _t24, _t436
	jmp _l17
	_l15:
	mov _t437, 8
	mov _t438, _t25
	imul _t438, _t437
	lea _t439, qword ptr [_t30 + _t438]
	mov _t33, _t439
	mov _t440, 0
	mov _t27, _t440
	_l14:
	cmp _t27, _t28
	jge _l12
	_l13:
	mov _t441, 8
	mov _t442, _t27
	imul _t442, _t441
	lea _t443, qword ptr [_t33 + _t442]
	mov _t220, _t443
	mov _t444, 8
	mov _t445, _t27
	imul _t445, _t444
	lea _t446, qword ptr [_t29 + _t445]
	mov _t446, qword ptr [_t446]
	mov qword ptr [_t220], _t446
	mov _t447, 1
	lea _t448, qword ptr [_t27 + _t447]
	mov _t27, _t448
	jmp _l14
	_l12:
	mov _t214, _t30
	mov rdi, _t214
	and rsp, -16
	call _Iprintln_pai
	lea _t449, qword ptr _g3[rip]
	mov _t46, _t449
	mov _t450, 8
	lea _t451, qword ptr [_t46 + _t450]
	mov _t47, _t451
	mov _t38, _t47
	mov _t223, p
	mov rdi, _t223
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t452, _RV1
	mov _t222, _t452
	mov _t41, _t222
	mov _t453, 8
	mov _t454, _t38
	sub _t454, _t453
	mov _t454, qword ptr [_t454]
	mov _t37, _t454
	mov _t455, 8
	mov _t456, _t41
	sub _t456, _t455
	mov _t456, qword ptr [_t456]
	mov _t40, _t456
	lea _t457, qword ptr [_t37 + _t40]
	mov _t44, _t457
	mov _t458, 8
	mov _t459, 8
	mov _t460, _t44
	imul _t460, _t459
	lea _t461, qword ptr [_t458 + _t460]
	mov _t225, _t461
	mov rdi, _t225
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t462, _RV1
	mov _t224, _t462
	mov _t43, _t224
	mov qword ptr [_t43], _t44
	mov _t463, 8
	lea _t464, qword ptr [_t43 + _t463]
	mov _t42, _t464
	mov _t465, 0
	mov _t36, _t465
	_l23:
	cmp _t36, _t37
	jge _l21
	_l22:
	mov _t466, 8
	mov _t467, _t36
	imul _t467, _t466
	lea _t468, qword ptr [_t42 + _t467]
	mov _t226, _t468
	mov _t469, 8
	mov _t470, _t36
	imul _t470, _t469
	lea _t471, qword ptr [_t38 + _t470]
	mov _t471, qword ptr [_t471]
	mov qword ptr [_t226], _t471
	mov _t472, 1
	lea _t473, qword ptr [_t36 + _t472]
	mov _t36, _t473
	jmp _l23
	_l21:
	mov _t474, 8
	mov _t475, _t37
	imul _t475, _t474
	lea _t476, qword ptr [_t42 + _t475]
	mov _t45, _t476
	mov _t477, 0
	mov _t39, _t477
	_l20:
	cmp _t39, _t40
	jge _l18
	_l19:
	mov _t478, 8
	mov _t479, _t39
	imul _t479, _t478
	lea _t480, qword ptr [_t45 + _t479]
	mov _t227, _t480
	mov _t481, 8
	mov _t482, _t39
	imul _t482, _t481
	lea _t483, qword ptr [_t41 + _t482]
	mov _t483, qword ptr [_t483]
	mov qword ptr [_t227], _t483
	mov _t484, 1
	lea _t485, qword ptr [_t39 + _t484]
	mov _t39, _t485
	jmp _l20
	_l18:
	mov _t221, _t42
	mov rdi, _t221
	and rsp, -16
	call _Iprintln_pai
	lea _t486, qword ptr _g4[rip]
	mov _t58, _t486
	mov _t487, 8
	lea _t488, qword ptr [_t58 + _t487]
	mov _t59, _t488
	mov _t50, _t59
	mov _t230, q
	mov rdi, _t230
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t489, _RV1
	mov _t229, _t489
	mov _t53, _t229
	mov _t490, 8
	mov _t491, _t50
	sub _t491, _t490
	mov _t491, qword ptr [_t491]
	mov _t49, _t491
	mov _t492, 8
	mov _t493, _t53
	sub _t493, _t492
	mov _t493, qword ptr [_t493]
	mov _t52, _t493
	lea _t494, qword ptr [_t49 + _t52]
	mov _t56, _t494
	mov _t495, 8
	mov _t496, 8
	mov _t497, _t56
	imul _t497, _t496
	lea _t498, qword ptr [_t495 + _t497]
	mov _t232, _t498
	mov rdi, _t232
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t499, _RV1
	mov _t231, _t499
	mov _t55, _t231
	mov qword ptr [_t55], _t56
	mov _t500, 8
	lea _t501, qword ptr [_t55 + _t500]
	mov _t54, _t501
	mov _t502, 0
	mov _t48, _t502
	_l29:
	cmp _t48, _t49
	jge _l27
	_l28:
	mov _t503, 8
	mov _t504, _t48
	imul _t504, _t503
	lea _t505, qword ptr [_t54 + _t504]
	mov _t233, _t505
	mov _t506, 8
	mov _t507, _t48
	imul _t507, _t506
	lea _t508, qword ptr [_t50 + _t507]
	mov _t508, qword ptr [_t508]
	mov qword ptr [_t233], _t508
	mov _t509, 1
	lea _t510, qword ptr [_t48 + _t509]
	mov _t48, _t510
	jmp _l29
	_l27:
	mov _t511, 8
	mov _t512, _t49
	imul _t512, _t511
	lea _t513, qword ptr [_t54 + _t512]
	mov _t57, _t513
	mov _t514, 0
	mov _t51, _t514
	_l26:
	cmp _t51, _t52
	jge _l24
	_l25:
	mov _t515, 8
	mov _t516, _t51
	imul _t516, _t515
	lea _t517, qword ptr [_t57 + _t516]
	mov _t234, _t517
	mov _t518, 8
	mov _t519, _t51
	imul _t519, _t518
	lea _t520, qword ptr [_t53 + _t519]
	mov _t520, qword ptr [_t520]
	mov qword ptr [_t234], _t520
	mov _t521, 1
	lea _t522, qword ptr [_t51 + _t521]
	mov _t51, _t522
	jmp _l26
	_l24:
	mov _t228, _t54
	mov rdi, _t228
	and rsp, -16
	call _Iprintln_pai
	lea _t523, qword ptr _g5[rip]
	mov _t70, _t523
	mov _t524, 8
	lea _t525, qword ptr [_t70 + _t524]
	mov _t71, _t525
	mov _t62, _t71
	mov _t237, r
	mov rdi, _t237
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t526, _RV1
	mov _t236, _t526
	mov _t65, _t236
	mov _t527, 8
	mov _t528, _t62
	sub _t528, _t527
	mov _t528, qword ptr [_t528]
	mov _t61, _t528
	mov _t529, 8
	mov _t530, _t65
	sub _t530, _t529
	mov _t530, qword ptr [_t530]
	mov _t64, _t530
	lea _t531, qword ptr [_t61 + _t64]
	mov _t68, _t531
	mov _t532, 8
	mov _t533, 8
	mov _t534, _t68
	imul _t534, _t533
	lea _t535, qword ptr [_t532 + _t534]
	mov _t239, _t535
	mov rdi, _t239
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t536, _RV1
	mov _t238, _t536
	mov _t67, _t238
	mov qword ptr [_t67], _t68
	mov _t537, 8
	lea _t538, qword ptr [_t67 + _t537]
	mov _t66, _t538
	mov _t539, 0
	mov _t60, _t539
	_l35:
	cmp _t60, _t61
	jge _l33
	_l34:
	mov _t540, 8
	mov _t541, _t60
	imul _t541, _t540
	lea _t542, qword ptr [_t66 + _t541]
	mov _t240, _t542
	mov _t543, 8
	mov _t544, _t60
	imul _t544, _t543
	lea _t545, qword ptr [_t62 + _t544]
	mov _t545, qword ptr [_t545]
	mov qword ptr [_t240], _t545
	mov _t546, 1
	lea _t547, qword ptr [_t60 + _t546]
	mov _t60, _t547
	jmp _l35
	_l33:
	mov _t548, 8
	mov _t549, _t61
	imul _t549, _t548
	lea _t550, qword ptr [_t66 + _t549]
	mov _t69, _t550
	mov _t551, 0
	mov _t63, _t551
	_l32:
	cmp _t63, _t64
	jge _l30
	_l31:
	mov _t552, 8
	mov _t553, _t63
	imul _t553, _t552
	lea _t554, qword ptr [_t69 + _t553]
	mov _t241, _t554
	mov _t555, 8
	mov _t556, _t63
	imul _t556, _t555
	lea _t557, qword ptr [_t65 + _t556]
	mov _t557, qword ptr [_t557]
	mov qword ptr [_t241], _t557
	mov _t558, 1
	lea _t559, qword ptr [_t63 + _t558]
	mov _t63, _t559
	jmp _l32
	_l30:
	mov _t235, _t66
	mov rdi, _t235
	and rsp, -16
	call _Iprintln_pai
	lea _t560, qword ptr _g6[rip]
	mov _t82, _t560
	mov _t561, 8
	lea _t562, qword ptr [_t82 + _t561]
	mov _t83, _t562
	mov _t74, _t83
	mov _t244, s
	mov rdi, _t244
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t563, _RV1
	mov _t243, _t563
	mov _t77, _t243
	mov _t564, 8
	mov _t565, _t74
	sub _t565, _t564
	mov _t565, qword ptr [_t565]
	mov _t73, _t565
	mov _t566, 8
	mov _t567, _t77
	sub _t567, _t566
	mov _t567, qword ptr [_t567]
	mov _t76, _t567
	lea _t568, qword ptr [_t73 + _t76]
	mov _t80, _t568
	mov _t569, 8
	mov _t570, 8
	mov _t571, _t80
	imul _t571, _t570
	lea _t572, qword ptr [_t569 + _t571]
	mov _t246, _t572
	mov rdi, _t246
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t573, _RV1
	mov _t245, _t573
	mov _t79, _t245
	mov qword ptr [_t79], _t80
	mov _t574, 8
	lea _t575, qword ptr [_t79 + _t574]
	mov _t78, _t575
	mov _t576, 0
	mov _t72, _t576
	_l41:
	cmp _t72, _t73
	jge _l39
	_l40:
	mov _t577, 8
	mov _t578, _t72
	imul _t578, _t577
	lea _t579, qword ptr [_t78 + _t578]
	mov _t247, _t579
	mov _t580, 8
	mov _t581, _t72
	imul _t581, _t580
	lea _t582, qword ptr [_t74 + _t581]
	mov _t582, qword ptr [_t582]
	mov qword ptr [_t247], _t582
	mov _t583, 1
	lea _t584, qword ptr [_t72 + _t583]
	mov _t72, _t584
	jmp _l41
	_l39:
	mov _t585, 8
	mov _t586, _t73
	imul _t586, _t585
	lea _t587, qword ptr [_t78 + _t586]
	mov _t81, _t587
	mov _t588, 0
	mov _t75, _t588
	_l38:
	cmp _t75, _t76
	jge _l36
	_l37:
	mov _t589, 8
	mov _t590, _t75
	imul _t590, _t589
	lea _t591, qword ptr [_t81 + _t590]
	mov _t248, _t591
	mov _t592, 8
	mov _t593, _t75
	imul _t593, _t592
	lea _t594, qword ptr [_t77 + _t593]
	mov _t594, qword ptr [_t594]
	mov qword ptr [_t248], _t594
	mov _t595, 1
	lea _t596, qword ptr [_t75 + _t595]
	mov _t75, _t596
	jmp _l38
	_l36:
	mov _t242, _t78
	mov rdi, _t242
	and rsp, -16
	call _Iprintln_pai
	lea _t597, qword ptr _g7[rip]
	mov _t94, _t597
	mov _t598, 8
	lea _t599, qword ptr [_t94 + _t598]
	mov _t95, _t599
	mov _t86, _t95
	mov _t251, t
	mov rdi, _t251
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t600, _RV1
	mov _t250, _t600
	mov _t89, _t250
	mov _t601, 8
	mov _t602, _t86
	sub _t602, _t601
	mov _t602, qword ptr [_t602]
	mov _t85, _t602
	mov _t603, 8
	mov _t604, _t89
	sub _t604, _t603
	mov _t604, qword ptr [_t604]
	mov _t88, _t604
	lea _t605, qword ptr [_t85 + _t88]
	mov _t92, _t605
	mov _t606, 8
	mov _t607, 8
	mov _t608, _t92
	imul _t608, _t607
	lea _t609, qword ptr [_t606 + _t608]
	mov _t253, _t609
	mov rdi, _t253
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t610, _RV1
	mov _t252, _t610
	mov _t91, _t252
	mov qword ptr [_t91], _t92
	mov _t611, 8
	lea _t612, qword ptr [_t91 + _t611]
	mov _t90, _t612
	mov _t613, 0
	mov _t84, _t613
	_l47:
	cmp _t84, _t85
	jge _l45
	_l46:
	mov _t614, 8
	mov _t615, _t84
	imul _t615, _t614
	lea _t616, qword ptr [_t90 + _t615]
	mov _t254, _t616
	mov _t617, 8
	mov _t618, _t84
	imul _t618, _t617
	lea _t619, qword ptr [_t86 + _t618]
	mov _t619, qword ptr [_t619]
	mov qword ptr [_t254], _t619
	mov _t620, 1
	lea _t621, qword ptr [_t84 + _t620]
	mov _t84, _t621
	jmp _l47
	_l45:
	mov _t622, 8
	mov _t623, _t85
	imul _t623, _t622
	lea _t624, qword ptr [_t90 + _t623]
	mov _t93, _t624
	mov _t625, 0
	mov _t87, _t625
	_l44:
	cmp _t87, _t88
	jge _l42
	_l43:
	mov _t626, 8
	mov _t627, _t87
	imul _t627, _t626
	lea _t628, qword ptr [_t93 + _t627]
	mov _t255, _t628
	mov _t629, 8
	mov _t630, _t87
	imul _t630, _t629
	lea _t631, qword ptr [_t89 + _t630]
	mov _t631, qword ptr [_t631]
	mov qword ptr [_t255], _t631
	mov _t632, 1
	lea _t633, qword ptr [_t87 + _t632]
	mov _t87, _t633
	jmp _l44
	_l42:
	mov _t249, _t90
	mov rdi, _t249
	and rsp, -16
	call _Iprintln_pai
	leave
	ret
_Ifoo_t8iiiiiiiiiiiiiiii:
	mov _t634, rdi
	mov _ARG1, rsi
	mov _ARG2, rdx
	mov _ARG3, rcx
	mov _ARG4, r8
	mov _ARG5, r9
	pop _ARG6
	pop _ARG7
	pop _ARG8
	mov _t635, _ARG1
	mov a, _t635
	mov _t636, _ARG2
	mov b, _t636
	mov _t637, _ARG3
	mov c, _t637
	mov _t638, _ARG4
	mov d, _t638
	mov _t639, _ARG5
	mov e, _t639
	mov _t640, _ARG6
	mov f, _t640
	mov _t641, _ARG7
	mov g, _t641
	mov _t642, _ARG8
	mov h, _t642
	lea _t643, qword ptr _g8[rip]
	mov _t106, _t643
	mov _t644, 8
	lea _t645, qword ptr [_t106 + _t644]
	mov _t107, _t645
	mov _t98, _t107
	mov _t258, a
	mov rdi, _t258
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t646, _RV1
	mov _t257, _t646
	mov _t101, _t257
	mov _t647, 8
	mov _t648, _t98
	sub _t648, _t647
	mov _t648, qword ptr [_t648]
	mov _t97, _t648
	mov _t649, 8
	mov _t650, _t101
	sub _t650, _t649
	mov _t650, qword ptr [_t650]
	mov _t100, _t650
	lea _t651, qword ptr [_t97 + _t100]
	mov _t104, _t651
	mov _t652, 8
	mov _t653, 8
	mov _t654, _t104
	imul _t654, _t653
	lea _t655, qword ptr [_t652 + _t654]
	mov _t260, _t655
	mov rdi, _t260
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t656, _RV1
	mov _t259, _t656
	mov _t103, _t259
	mov qword ptr [_t103], _t104
	mov _t657, 8
	lea _t658, qword ptr [_t103 + _t657]
	mov _t102, _t658
	mov _t659, 0
	mov _t96, _t659
	_l53:
	cmp _t96, _t97
	jge _l51
	_l52:
	mov _t660, 8
	mov _t661, _t96
	imul _t661, _t660
	lea _t662, qword ptr [_t102 + _t661]
	mov _t261, _t662
	mov _t663, 8
	mov _t664, _t96
	imul _t664, _t663
	lea _t665, qword ptr [_t98 + _t664]
	mov _t665, qword ptr [_t665]
	mov qword ptr [_t261], _t665
	mov _t666, 1
	lea _t667, qword ptr [_t96 + _t666]
	mov _t96, _t667
	jmp _l53
	_l51:
	mov _t668, 8
	mov _t669, _t97
	imul _t669, _t668
	lea _t670, qword ptr [_t102 + _t669]
	mov _t105, _t670
	mov _t671, 0
	mov _t99, _t671
	_l50:
	cmp _t99, _t100
	jge _l48
	_l49:
	mov _t672, 8
	mov _t673, _t99
	imul _t673, _t672
	lea _t674, qword ptr [_t105 + _t673]
	mov _t262, _t674
	mov _t675, 8
	mov _t676, _t99
	imul _t676, _t675
	lea _t677, qword ptr [_t101 + _t676]
	mov _t677, qword ptr [_t677]
	mov qword ptr [_t262], _t677
	mov _t678, 1
	lea _t679, qword ptr [_t99 + _t678]
	mov _t99, _t679
	jmp _l50
	_l48:
	mov _t256, _t102
	mov rdi, _t256
	and rsp, -16
	call _Iprintln_pai
	lea _t680, qword ptr _g9[rip]
	mov _t118, _t680
	mov _t681, 8
	lea _t682, qword ptr [_t118 + _t681]
	mov _t119, _t682
	mov _t110, _t119
	mov _t265, b
	mov rdi, _t265
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t683, _RV1
	mov _t264, _t683
	mov _t113, _t264
	mov _t684, 8
	mov _t685, _t110
	sub _t685, _t684
	mov _t685, qword ptr [_t685]
	mov _t109, _t685
	mov _t686, 8
	mov _t687, _t113
	sub _t687, _t686
	mov _t687, qword ptr [_t687]
	mov _t112, _t687
	lea _t688, qword ptr [_t109 + _t112]
	mov _t116, _t688
	mov _t689, 8
	mov _t690, 8
	mov _t691, _t116
	imul _t691, _t690
	lea _t692, qword ptr [_t689 + _t691]
	mov _t267, _t692
	mov rdi, _t267
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t693, _RV1
	mov _t266, _t693
	mov _t115, _t266
	mov qword ptr [_t115], _t116
	mov _t694, 8
	lea _t695, qword ptr [_t115 + _t694]
	mov _t114, _t695
	mov _t696, 0
	mov _t108, _t696
	_l59:
	cmp _t108, _t109
	jge _l57
	_l58:
	mov _t697, 8
	mov _t698, _t108
	imul _t698, _t697
	lea _t699, qword ptr [_t114 + _t698]
	mov _t268, _t699
	mov _t700, 8
	mov _t701, _t108
	imul _t701, _t700
	lea _t702, qword ptr [_t110 + _t701]
	mov _t702, qword ptr [_t702]
	mov qword ptr [_t268], _t702
	mov _t703, 1
	lea _t704, qword ptr [_t108 + _t703]
	mov _t108, _t704
	jmp _l59
	_l57:
	mov _t705, 8
	mov _t706, _t109
	imul _t706, _t705
	lea _t707, qword ptr [_t114 + _t706]
	mov _t117, _t707
	mov _t708, 0
	mov _t111, _t708
	_l56:
	cmp _t111, _t112
	jge _l54
	_l55:
	mov _t709, 8
	mov _t710, _t111
	imul _t710, _t709
	lea _t711, qword ptr [_t117 + _t710]
	mov _t269, _t711
	mov _t712, 8
	mov _t713, _t111
	imul _t713, _t712
	lea _t714, qword ptr [_t113 + _t713]
	mov _t714, qword ptr [_t714]
	mov qword ptr [_t269], _t714
	mov _t715, 1
	lea _t716, qword ptr [_t111 + _t715]
	mov _t111, _t716
	jmp _l56
	_l54:
	mov _t263, _t114
	mov rdi, _t263
	and rsp, -16
	call _Iprintln_pai
	lea _t717, qword ptr _g10[rip]
	mov _t130, _t717
	mov _t718, 8
	lea _t719, qword ptr [_t130 + _t718]
	mov _t131, _t719
	mov _t122, _t131
	mov _t272, c
	mov rdi, _t272
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t720, _RV1
	mov _t271, _t720
	mov _t125, _t271
	mov _t721, 8
	mov _t722, _t122
	sub _t722, _t721
	mov _t722, qword ptr [_t722]
	mov _t121, _t722
	mov _t723, 8
	mov _t724, _t125
	sub _t724, _t723
	mov _t724, qword ptr [_t724]
	mov _t124, _t724
	lea _t725, qword ptr [_t121 + _t124]
	mov _t128, _t725
	mov _t726, 8
	mov _t727, 8
	mov _t728, _t128
	imul _t728, _t727
	lea _t729, qword ptr [_t726 + _t728]
	mov _t274, _t729
	mov rdi, _t274
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t730, _RV1
	mov _t273, _t730
	mov _t127, _t273
	mov qword ptr [_t127], _t128
	mov _t731, 8
	lea _t732, qword ptr [_t127 + _t731]
	mov _t126, _t732
	mov _t733, 0
	mov _t120, _t733
	_l65:
	cmp _t120, _t121
	jge _l63
	_l64:
	mov _t734, 8
	mov _t735, _t120
	imul _t735, _t734
	lea _t736, qword ptr [_t126 + _t735]
	mov _t275, _t736
	mov _t737, 8
	mov _t738, _t120
	imul _t738, _t737
	lea _t739, qword ptr [_t122 + _t738]
	mov _t739, qword ptr [_t739]
	mov qword ptr [_t275], _t739
	mov _t740, 1
	lea _t741, qword ptr [_t120 + _t740]
	mov _t120, _t741
	jmp _l65
	_l63:
	mov _t742, 8
	mov _t743, _t121
	imul _t743, _t742
	lea _t744, qword ptr [_t126 + _t743]
	mov _t129, _t744
	mov _t745, 0
	mov _t123, _t745
	_l62:
	cmp _t123, _t124
	jge _l60
	_l61:
	mov _t746, 8
	mov _t747, _t123
	imul _t747, _t746
	lea _t748, qword ptr [_t129 + _t747]
	mov _t276, _t748
	mov _t749, 8
	mov _t750, _t123
	imul _t750, _t749
	lea _t751, qword ptr [_t125 + _t750]
	mov _t751, qword ptr [_t751]
	mov qword ptr [_t276], _t751
	mov _t752, 1
	lea _t753, qword ptr [_t123 + _t752]
	mov _t123, _t753
	jmp _l62
	_l60:
	mov _t270, _t126
	mov rdi, _t270
	and rsp, -16
	call _Iprintln_pai
	lea _t754, qword ptr _g11[rip]
	mov _t142, _t754
	mov _t755, 8
	lea _t756, qword ptr [_t142 + _t755]
	mov _t143, _t756
	mov _t134, _t143
	mov _t279, d
	mov rdi, _t279
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t757, _RV1
	mov _t278, _t757
	mov _t137, _t278
	mov _t758, 8
	mov _t759, _t134
	sub _t759, _t758
	mov _t759, qword ptr [_t759]
	mov _t133, _t759
	mov _t760, 8
	mov _t761, _t137
	sub _t761, _t760
	mov _t761, qword ptr [_t761]
	mov _t136, _t761
	lea _t762, qword ptr [_t133 + _t136]
	mov _t140, _t762
	mov _t763, 8
	mov _t764, 8
	mov _t765, _t140
	imul _t765, _t764
	lea _t766, qword ptr [_t763 + _t765]
	mov _t281, _t766
	mov rdi, _t281
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t767, _RV1
	mov _t280, _t767
	mov _t139, _t280
	mov qword ptr [_t139], _t140
	mov _t768, 8
	lea _t769, qword ptr [_t139 + _t768]
	mov _t138, _t769
	mov _t770, 0
	mov _t132, _t770
	_l71:
	cmp _t132, _t133
	jge _l69
	_l70:
	mov _t771, 8
	mov _t772, _t132
	imul _t772, _t771
	lea _t773, qword ptr [_t138 + _t772]
	mov _t282, _t773
	mov _t774, 8
	mov _t775, _t132
	imul _t775, _t774
	lea _t776, qword ptr [_t134 + _t775]
	mov _t776, qword ptr [_t776]
	mov qword ptr [_t282], _t776
	mov _t777, 1
	lea _t778, qword ptr [_t132 + _t777]
	mov _t132, _t778
	jmp _l71
	_l69:
	mov _t779, 8
	mov _t780, _t133
	imul _t780, _t779
	lea _t781, qword ptr [_t138 + _t780]
	mov _t141, _t781
	mov _t782, 0
	mov _t135, _t782
	_l68:
	cmp _t135, _t136
	jge _l66
	_l67:
	mov _t783, 8
	mov _t784, _t135
	imul _t784, _t783
	lea _t785, qword ptr [_t141 + _t784]
	mov _t283, _t785
	mov _t786, 8
	mov _t787, _t135
	imul _t787, _t786
	lea _t788, qword ptr [_t137 + _t787]
	mov _t788, qword ptr [_t788]
	mov qword ptr [_t283], _t788
	mov _t789, 1
	lea _t790, qword ptr [_t135 + _t789]
	mov _t135, _t790
	jmp _l68
	_l66:
	mov _t277, _t138
	mov rdi, _t277
	and rsp, -16
	call _Iprintln_pai
	lea _t791, qword ptr _g12[rip]
	mov _t154, _t791
	mov _t792, 8
	lea _t793, qword ptr [_t154 + _t792]
	mov _t155, _t793
	mov _t146, _t155
	mov _t286, e
	mov rdi, _t286
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t794, _RV1
	mov _t285, _t794
	mov _t149, _t285
	mov _t795, 8
	mov _t796, _t146
	sub _t796, _t795
	mov _t796, qword ptr [_t796]
	mov _t145, _t796
	mov _t797, 8
	mov _t798, _t149
	sub _t798, _t797
	mov _t798, qword ptr [_t798]
	mov _t148, _t798
	lea _t799, qword ptr [_t145 + _t148]
	mov _t152, _t799
	mov _t800, 8
	mov _t801, 8
	mov _t802, _t152
	imul _t802, _t801
	lea _t803, qword ptr [_t800 + _t802]
	mov _t288, _t803
	mov rdi, _t288
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t804, _RV1
	mov _t287, _t804
	mov _t151, _t287
	mov qword ptr [_t151], _t152
	mov _t805, 8
	lea _t806, qword ptr [_t151 + _t805]
	mov _t150, _t806
	mov _t807, 0
	mov _t144, _t807
	_l77:
	cmp _t144, _t145
	jge _l75
	_l76:
	mov _t808, 8
	mov _t809, _t144
	imul _t809, _t808
	lea _t810, qword ptr [_t150 + _t809]
	mov _t289, _t810
	mov _t811, 8
	mov _t812, _t144
	imul _t812, _t811
	lea _t813, qword ptr [_t146 + _t812]
	mov _t813, qword ptr [_t813]
	mov qword ptr [_t289], _t813
	mov _t814, 1
	lea _t815, qword ptr [_t144 + _t814]
	mov _t144, _t815
	jmp _l77
	_l75:
	mov _t816, 8
	mov _t817, _t145
	imul _t817, _t816
	lea _t818, qword ptr [_t150 + _t817]
	mov _t153, _t818
	mov _t819, 0
	mov _t147, _t819
	_l74:
	cmp _t147, _t148
	jge _l72
	_l73:
	mov _t820, 8
	mov _t821, _t147
	imul _t821, _t820
	lea _t822, qword ptr [_t153 + _t821]
	mov _t290, _t822
	mov _t823, 8
	mov _t824, _t147
	imul _t824, _t823
	lea _t825, qword ptr [_t149 + _t824]
	mov _t825, qword ptr [_t825]
	mov qword ptr [_t290], _t825
	mov _t826, 1
	lea _t827, qword ptr [_t147 + _t826]
	mov _t147, _t827
	jmp _l74
	_l72:
	mov _t284, _t150
	mov rdi, _t284
	and rsp, -16
	call _Iprintln_pai
	lea _t828, qword ptr _g13[rip]
	mov _t166, _t828
	mov _t829, 8
	lea _t830, qword ptr [_t166 + _t829]
	mov _t167, _t830
	mov _t158, _t167
	mov _t293, f
	mov rdi, _t293
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t831, _RV1
	mov _t292, _t831
	mov _t161, _t292
	mov _t832, 8
	mov _t833, _t158
	sub _t833, _t832
	mov _t833, qword ptr [_t833]
	mov _t157, _t833
	mov _t834, 8
	mov _t835, _t161
	sub _t835, _t834
	mov _t835, qword ptr [_t835]
	mov _t160, _t835
	lea _t836, qword ptr [_t157 + _t160]
	mov _t164, _t836
	mov _t837, 8
	mov _t838, 8
	mov _t839, _t164
	imul _t839, _t838
	lea _t840, qword ptr [_t837 + _t839]
	mov _t295, _t840
	mov rdi, _t295
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t841, _RV1
	mov _t294, _t841
	mov _t163, _t294
	mov qword ptr [_t163], _t164
	mov _t842, 8
	lea _t843, qword ptr [_t163 + _t842]
	mov _t162, _t843
	mov _t844, 0
	mov _t156, _t844
	_l83:
	cmp _t156, _t157
	jge _l81
	_l82:
	mov _t845, 8
	mov _t846, _t156
	imul _t846, _t845
	lea _t847, qword ptr [_t162 + _t846]
	mov _t296, _t847
	mov _t848, 8
	mov _t849, _t156
	imul _t849, _t848
	lea _t850, qword ptr [_t158 + _t849]
	mov _t850, qword ptr [_t850]
	mov qword ptr [_t296], _t850
	mov _t851, 1
	lea _t852, qword ptr [_t156 + _t851]
	mov _t156, _t852
	jmp _l83
	_l81:
	mov _t853, 8
	mov _t854, _t157
	imul _t854, _t853
	lea _t855, qword ptr [_t162 + _t854]
	mov _t165, _t855
	mov _t856, 0
	mov _t159, _t856
	_l80:
	cmp _t159, _t160
	jge _l78
	_l79:
	mov _t857, 8
	mov _t858, _t159
	imul _t858, _t857
	lea _t859, qword ptr [_t165 + _t858]
	mov _t297, _t859
	mov _t860, 8
	mov _t861, _t159
	imul _t861, _t860
	lea _t862, qword ptr [_t161 + _t861]
	mov _t862, qword ptr [_t862]
	mov qword ptr [_t297], _t862
	mov _t863, 1
	lea _t864, qword ptr [_t159 + _t863]
	mov _t159, _t864
	jmp _l80
	_l78:
	mov _t291, _t162
	mov rdi, _t291
	and rsp, -16
	call _Iprintln_pai
	lea _t865, qword ptr _g14[rip]
	mov _t178, _t865
	mov _t866, 8
	lea _t867, qword ptr [_t178 + _t866]
	mov _t179, _t867
	mov _t170, _t179
	mov _t300, g
	mov rdi, _t300
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t868, _RV1
	mov _t299, _t868
	mov _t173, _t299
	mov _t869, 8
	mov _t870, _t170
	sub _t870, _t869
	mov _t870, qword ptr [_t870]
	mov _t169, _t870
	mov _t871, 8
	mov _t872, _t173
	sub _t872, _t871
	mov _t872, qword ptr [_t872]
	mov _t172, _t872
	lea _t873, qword ptr [_t169 + _t172]
	mov _t176, _t873
	mov _t874, 8
	mov _t875, 8
	mov _t876, _t176
	imul _t876, _t875
	lea _t877, qword ptr [_t874 + _t876]
	mov _t302, _t877
	mov rdi, _t302
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t878, _RV1
	mov _t301, _t878
	mov _t175, _t301
	mov qword ptr [_t175], _t176
	mov _t879, 8
	lea _t880, qword ptr [_t175 + _t879]
	mov _t174, _t880
	mov _t881, 0
	mov _t168, _t881
	_l89:
	cmp _t168, _t169
	jge _l87
	_l88:
	mov _t882, 8
	mov _t883, _t168
	imul _t883, _t882
	lea _t884, qword ptr [_t174 + _t883]
	mov _t303, _t884
	mov _t885, 8
	mov _t886, _t168
	imul _t886, _t885
	lea _t887, qword ptr [_t170 + _t886]
	mov _t887, qword ptr [_t887]
	mov qword ptr [_t303], _t887
	mov _t888, 1
	lea _t889, qword ptr [_t168 + _t888]
	mov _t168, _t889
	jmp _l89
	_l87:
	mov _t890, 8
	mov _t891, _t169
	imul _t891, _t890
	lea _t892, qword ptr [_t174 + _t891]
	mov _t177, _t892
	mov _t893, 0
	mov _t171, _t893
	_l86:
	cmp _t171, _t172
	jge _l84
	_l85:
	mov _t894, 8
	mov _t895, _t171
	imul _t895, _t894
	lea _t896, qword ptr [_t177 + _t895]
	mov _t304, _t896
	mov _t897, 8
	mov _t898, _t171
	imul _t898, _t897
	lea _t899, qword ptr [_t173 + _t898]
	mov _t899, qword ptr [_t899]
	mov qword ptr [_t304], _t899
	mov _t900, 1
	lea _t901, qword ptr [_t171 + _t900]
	mov _t171, _t901
	jmp _l86
	_l84:
	mov _t298, _t174
	mov rdi, _t298
	and rsp, -16
	call _Iprintln_pai
	lea _t902, qword ptr _g15[rip]
	mov _t190, _t902
	mov _t903, 8
	lea _t904, qword ptr [_t190 + _t903]
	mov _t191, _t904
	mov _t182, _t191
	mov _t307, h
	mov rdi, _t307
	and rsp, -16
	call _IunparseInt_aii
	mov _RV1, rax
	mov _t905, _RV1
	mov _t306, _t905
	mov _t185, _t306
	mov _t906, 8
	mov _t907, _t182
	sub _t907, _t906
	mov _t907, qword ptr [_t907]
	mov _t181, _t907
	mov _t908, 8
	mov _t909, _t185
	sub _t909, _t908
	mov _t909, qword ptr [_t909]
	mov _t184, _t909
	lea _t910, qword ptr [_t181 + _t184]
	mov _t188, _t910
	mov _t911, 8
	mov _t912, 8
	mov _t913, _t188
	imul _t913, _t912
	lea _t914, qword ptr [_t911 + _t913]
	mov _t309, _t914
	mov rdi, _t309
	and rsp, -16
	call _xi_alloc
	mov _RV1, rax
	mov _t915, _RV1
	mov _t308, _t915
	mov _t187, _t308
	mov qword ptr [_t187], _t188
	mov _t916, 8
	lea _t917, qword ptr [_t187 + _t916]
	mov _t186, _t917
	mov _t918, 0
	mov _t180, _t918
	_l95:
	cmp _t180, _t181
	jge _l93
	_l94:
	mov _t919, 8
	mov _t920, _t180
	imul _t920, _t919
	lea _t921, qword ptr [_t186 + _t920]
	mov _t310, _t921
	mov _t922, 8
	mov _t923, _t180
	imul _t923, _t922
	lea _t924, qword ptr [_t182 + _t923]
	mov _t924, qword ptr [_t924]
	mov qword ptr [_t310], _t924
	mov _t925, 1
	lea _t926, qword ptr [_t180 + _t925]
	mov _t180, _t926
	jmp _l95
	_l93:
	mov _t927, 8
	mov _t928, _t181
	imul _t928, _t927
	lea _t929, qword ptr [_t186 + _t928]
	mov _t189, _t929
	mov _t930, 0
	mov _t183, _t930
	_l92:
	cmp _t183, _t184
	jge _l90
	_l91:
	mov _t931, 8
	mov _t932, _t183
	imul _t932, _t931
	lea _t933, qword ptr [_t189 + _t932]
	mov _t311, _t933
	mov _t934, 8
	mov _t935, _t183
	imul _t935, _t934
	lea _t936, qword ptr [_t185 + _t935]
	mov _t936, qword ptr [_t936]
	mov qword ptr [_t311], _t936
	mov _t937, 1
	lea _t938, qword ptr [_t183 + _t937]
	mov _t183, _t938
	jmp _l92
	_l90:
	mov _t305, _t186
	mov rdi, _t305
	and rsp, -16
	call _Iprintln_pai
	mov _t939, 22
	mov _t312, _t939
	mov _t940, 24
	mov _t313, _t940
	mov _t941, 3
	mov _t314, _t941
	mov _t942, 4
	mov _t315, _t942
	mov _t943, 5
	mov _t316, _t943
	mov _t944, 6
	mov _t317, _t944
	mov _t945, 7
	mov _t318, _t945
	mov _t946, 28
	mov _t319, _t946
	mov rax, _t312
	mov rdx, _t313
	mov qword ptr [_t634 + 0], _t314
	mov qword ptr [_t634 + 8], _t315
	mov qword ptr [_t634 + 16], _t316
	mov qword ptr [_t634 + 24], _t317
	mov qword ptr [_t634 + 32], _t318
	mov qword ptr [_t634 + 40], _t319
	leave
	ret
