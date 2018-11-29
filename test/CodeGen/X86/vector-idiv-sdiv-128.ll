; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-unknown-unknown -mattr=+sse2 | FileCheck %s --check-prefix=SSE --check-prefix=SSE2
; RUN: llc < %s -mtriple=x86_64-unknown-unknown -mattr=+sse4.1 | FileCheck %s --check-prefix=SSE --check-prefix=SSE41
; RUN: llc < %s -mtriple=x86_64-unknown-unknown -mattr=+avx | FileCheck %s --check-prefix=AVX --check-prefix=AVX1
; RUN: llc < %s -mtriple=x86_64-unknown-unknown -mattr=+avx2 | FileCheck %s --check-prefix=AVX --check-prefix=AVX2 --check-prefix=AVX2NOBW
; RUN: llc < %s -mtriple=x86_64-unknown-unknown -mattr=+avx512bw | FileCheck %s --check-prefix=AVX --check-prefix=AVX2 --check-prefix=AVX512BW

;
; sdiv by 7
;

define <2 x i64> @test_div7_2i64(<2 x i64> %a) nounwind {
; SSE2-LABEL: test_div7_2i64:
; SSE2:       # %bb.0:
; SSE2-NEXT:    movq %xmm0, %rax
; SSE2-NEXT:    movabsq $5270498306774157605, %rcx # imm = 0x4924924924924925
; SSE2-NEXT:    imulq %rcx
; SSE2-NEXT:    movq %rdx, %rax
; SSE2-NEXT:    shrq $63, %rax
; SSE2-NEXT:    sarq %rdx
; SSE2-NEXT:    addq %rax, %rdx
; SSE2-NEXT:    movq %rdx, %xmm1
; SSE2-NEXT:    pshufd {{.*#+}} xmm0 = xmm0[2,3,0,1]
; SSE2-NEXT:    movq %xmm0, %rax
; SSE2-NEXT:    imulq %rcx
; SSE2-NEXT:    movq %rdx, %rax
; SSE2-NEXT:    shrq $63, %rax
; SSE2-NEXT:    sarq %rdx
; SSE2-NEXT:    addq %rax, %rdx
; SSE2-NEXT:    movq %rdx, %xmm0
; SSE2-NEXT:    punpcklqdq {{.*#+}} xmm1 = xmm1[0],xmm0[0]
; SSE2-NEXT:    movdqa %xmm1, %xmm0
; SSE2-NEXT:    retq
;
; SSE41-LABEL: test_div7_2i64:
; SSE41:       # %bb.0:
; SSE41-NEXT:    pextrq $1, %xmm0, %rax
; SSE41-NEXT:    movabsq $5270498306774157605, %rcx # imm = 0x4924924924924925
; SSE41-NEXT:    imulq %rcx
; SSE41-NEXT:    movq %rdx, %rax
; SSE41-NEXT:    shrq $63, %rax
; SSE41-NEXT:    sarq %rdx
; SSE41-NEXT:    addq %rax, %rdx
; SSE41-NEXT:    movq %rdx, %xmm1
; SSE41-NEXT:    movq %xmm0, %rax
; SSE41-NEXT:    imulq %rcx
; SSE41-NEXT:    movq %rdx, %rax
; SSE41-NEXT:    shrq $63, %rax
; SSE41-NEXT:    sarq %rdx
; SSE41-NEXT:    addq %rax, %rdx
; SSE41-NEXT:    movq %rdx, %xmm0
; SSE41-NEXT:    punpcklqdq {{.*#+}} xmm0 = xmm0[0],xmm1[0]
; SSE41-NEXT:    retq
;
; AVX-LABEL: test_div7_2i64:
; AVX:       # %bb.0:
; AVX-NEXT:    vpextrq $1, %xmm0, %rax
; AVX-NEXT:    movabsq $5270498306774157605, %rcx # imm = 0x4924924924924925
; AVX-NEXT:    imulq %rcx
; AVX-NEXT:    movq %rdx, %rax
; AVX-NEXT:    shrq $63, %rax
; AVX-NEXT:    sarq %rdx
; AVX-NEXT:    addq %rax, %rdx
; AVX-NEXT:    vmovq %rdx, %xmm1
; AVX-NEXT:    vmovq %xmm0, %rax
; AVX-NEXT:    imulq %rcx
; AVX-NEXT:    movq %rdx, %rax
; AVX-NEXT:    shrq $63, %rax
; AVX-NEXT:    sarq %rdx
; AVX-NEXT:    addq %rax, %rdx
; AVX-NEXT:    vmovq %rdx, %xmm0
; AVX-NEXT:    vpunpcklqdq {{.*#+}} xmm0 = xmm0[0],xmm1[0]
; AVX-NEXT:    retq
  %res = sdiv <2 x i64> %a, <i64 7, i64 7>
  ret <2 x i64> %res
}

define <4 x i32> @test_div7_4i32(<4 x i32> %a) nounwind {
; SSE2-LABEL: test_div7_4i32:
; SSE2:       # %bb.0:
; SSE2-NEXT:    movdqa {{.*#+}} xmm2 = [2454267027,2454267027,2454267027,2454267027]
; SSE2-NEXT:    movdqa %xmm0, %xmm1
; SSE2-NEXT:    pmuludq %xmm2, %xmm1
; SSE2-NEXT:    pshufd {{.*#+}} xmm1 = xmm1[1,3,2,3]
; SSE2-NEXT:    pshufd {{.*#+}} xmm3 = xmm0[1,1,3,3]
; SSE2-NEXT:    pmuludq %xmm2, %xmm3
; SSE2-NEXT:    pshufd {{.*#+}} xmm3 = xmm3[1,3,2,3]
; SSE2-NEXT:    punpckldq {{.*#+}} xmm1 = xmm1[0],xmm3[0],xmm1[1],xmm3[1]
; SSE2-NEXT:    pxor %xmm3, %xmm3
; SSE2-NEXT:    pcmpgtd %xmm0, %xmm3
; SSE2-NEXT:    pand %xmm2, %xmm3
; SSE2-NEXT:    paddd %xmm0, %xmm3
; SSE2-NEXT:    psubd %xmm3, %xmm1
; SSE2-NEXT:    paddd %xmm0, %xmm1
; SSE2-NEXT:    movdqa %xmm1, %xmm0
; SSE2-NEXT:    psrld $31, %xmm0
; SSE2-NEXT:    psrad $2, %xmm1
; SSE2-NEXT:    paddd %xmm0, %xmm1
; SSE2-NEXT:    movdqa %xmm1, %xmm0
; SSE2-NEXT:    retq
;
; SSE41-LABEL: test_div7_4i32:
; SSE41:       # %bb.0:
; SSE41-NEXT:    pshufd {{.*#+}} xmm2 = xmm0[1,1,3,3]
; SSE41-NEXT:    movdqa {{.*#+}} xmm1 = [2454267027,2454267027,2454267027,2454267027]
; SSE41-NEXT:    pmuldq %xmm1, %xmm2
; SSE41-NEXT:    pmuldq %xmm0, %xmm1
; SSE41-NEXT:    pshufd {{.*#+}} xmm1 = xmm1[1,1,3,3]
; SSE41-NEXT:    pblendw {{.*#+}} xmm1 = xmm1[0,1],xmm2[2,3],xmm1[4,5],xmm2[6,7]
; SSE41-NEXT:    paddd %xmm0, %xmm1
; SSE41-NEXT:    movdqa %xmm1, %xmm0
; SSE41-NEXT:    psrld $31, %xmm0
; SSE41-NEXT:    psrad $2, %xmm1
; SSE41-NEXT:    paddd %xmm0, %xmm1
; SSE41-NEXT:    movdqa %xmm1, %xmm0
; SSE41-NEXT:    retq
;
; AVX1-LABEL: test_div7_4i32:
; AVX1:       # %bb.0:
; AVX1-NEXT:    vpshufd {{.*#+}} xmm1 = xmm0[1,1,3,3]
; AVX1-NEXT:    vmovdqa {{.*#+}} xmm2 = [2454267027,2454267027,2454267027,2454267027]
; AVX1-NEXT:    vpmuldq %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpmuldq %xmm2, %xmm0, %xmm2
; AVX1-NEXT:    vpshufd {{.*#+}} xmm2 = xmm2[1,1,3,3]
; AVX1-NEXT:    vpblendw {{.*#+}} xmm1 = xmm2[0,1],xmm1[2,3],xmm2[4,5],xmm1[6,7]
; AVX1-NEXT:    vpaddd %xmm0, %xmm1, %xmm0
; AVX1-NEXT:    vpsrld $31, %xmm0, %xmm1
; AVX1-NEXT:    vpsrad $2, %xmm0, %xmm0
; AVX1-NEXT:    vpaddd %xmm1, %xmm0, %xmm0
; AVX1-NEXT:    retq
;
; AVX2-LABEL: test_div7_4i32:
; AVX2:       # %bb.0:
; AVX2-NEXT:    vpshufd {{.*#+}} xmm1 = xmm0[1,1,3,3]
; AVX2-NEXT:    vpbroadcastd {{.*#+}} xmm2 = [2454267027,2454267027,2454267027,2454267027]
; AVX2-NEXT:    vpmuldq %xmm2, %xmm1, %xmm1
; AVX2-NEXT:    vpmuldq %xmm2, %xmm0, %xmm2
; AVX2-NEXT:    vpshufd {{.*#+}} xmm2 = xmm2[1,1,3,3]
; AVX2-NEXT:    vpblendd {{.*#+}} xmm1 = xmm2[0],xmm1[1],xmm2[2],xmm1[3]
; AVX2-NEXT:    vpaddd %xmm0, %xmm1, %xmm0
; AVX2-NEXT:    vpsrld $31, %xmm0, %xmm1
; AVX2-NEXT:    vpsrad $2, %xmm0, %xmm0
; AVX2-NEXT:    vpaddd %xmm1, %xmm0, %xmm0
; AVX2-NEXT:    retq
  %res = sdiv <4 x i32> %a, <i32 7, i32 7, i32 7, i32 7>
  ret <4 x i32> %res
}

define <8 x i16> @test_div7_8i16(<8 x i16> %a) nounwind {
; SSE-LABEL: test_div7_8i16:
; SSE:       # %bb.0:
; SSE-NEXT:    pmulhw {{.*}}(%rip), %xmm0
; SSE-NEXT:    movdqa %xmm0, %xmm1
; SSE-NEXT:    psrlw $15, %xmm1
; SSE-NEXT:    psraw $1, %xmm0
; SSE-NEXT:    paddw %xmm1, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: test_div7_8i16:
; AVX:       # %bb.0:
; AVX-NEXT:    vpmulhw {{.*}}(%rip), %xmm0, %xmm0
; AVX-NEXT:    vpsrlw $15, %xmm0, %xmm1
; AVX-NEXT:    vpsraw $1, %xmm0, %xmm0
; AVX-NEXT:    vpaddw %xmm1, %xmm0, %xmm0
; AVX-NEXT:    retq
  %res = sdiv <8 x i16> %a, <i16 7, i16 7, i16 7, i16 7, i16 7, i16 7, i16 7, i16 7>
  ret <8 x i16> %res
}

define <16 x i8> @test_div7_16i8(<16 x i8> %a) nounwind {
; SSE2-LABEL: test_div7_16i8:
; SSE2:       # %bb.0:
; SSE2-NEXT:    punpckhbw {{.*#+}} xmm2 = xmm2[8],xmm0[8],xmm2[9],xmm0[9],xmm2[10],xmm0[10],xmm2[11],xmm0[11],xmm2[12],xmm0[12],xmm2[13],xmm0[13],xmm2[14],xmm0[14],xmm2[15],xmm0[15]
; SSE2-NEXT:    psraw $8, %xmm2
; SSE2-NEXT:    movdqa {{.*#+}} xmm3 = [65427,65427,65427,65427,65427,65427,65427,65427]
; SSE2-NEXT:    pmullw %xmm3, %xmm2
; SSE2-NEXT:    psrlw $8, %xmm2
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm1 = xmm1[0],xmm0[0],xmm1[1],xmm0[1],xmm1[2],xmm0[2],xmm1[3],xmm0[3],xmm1[4],xmm0[4],xmm1[5],xmm0[5],xmm1[6],xmm0[6],xmm1[7],xmm0[7]
; SSE2-NEXT:    psraw $8, %xmm1
; SSE2-NEXT:    pmullw %xmm3, %xmm1
; SSE2-NEXT:    psrlw $8, %xmm1
; SSE2-NEXT:    packuswb %xmm2, %xmm1
; SSE2-NEXT:    paddb %xmm0, %xmm1
; SSE2-NEXT:    movdqa %xmm1, %xmm0
; SSE2-NEXT:    psrlw $2, %xmm0
; SSE2-NEXT:    pand {{.*}}(%rip), %xmm0
; SSE2-NEXT:    movdqa {{.*#+}} xmm2 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; SSE2-NEXT:    pxor %xmm2, %xmm0
; SSE2-NEXT:    psubb %xmm2, %xmm0
; SSE2-NEXT:    psrlw $7, %xmm1
; SSE2-NEXT:    pand {{.*}}(%rip), %xmm1
; SSE2-NEXT:    paddb %xmm0, %xmm1
; SSE2-NEXT:    movdqa %xmm1, %xmm0
; SSE2-NEXT:    retq
;
; SSE41-LABEL: test_div7_16i8:
; SSE41:       # %bb.0:
; SSE41-NEXT:    pmovsxbw %xmm0, %xmm1
; SSE41-NEXT:    movdqa {{.*#+}} xmm2 = [65427,65427,65427,65427,65427,65427,65427,65427]
; SSE41-NEXT:    pmullw %xmm2, %xmm1
; SSE41-NEXT:    psrlw $8, %xmm1
; SSE41-NEXT:    pshufd {{.*#+}} xmm3 = xmm0[2,3,0,1]
; SSE41-NEXT:    pmovsxbw %xmm3, %xmm3
; SSE41-NEXT:    pmullw %xmm2, %xmm3
; SSE41-NEXT:    psrlw $8, %xmm3
; SSE41-NEXT:    packuswb %xmm3, %xmm1
; SSE41-NEXT:    paddb %xmm0, %xmm1
; SSE41-NEXT:    movdqa %xmm1, %xmm0
; SSE41-NEXT:    psrlw $2, %xmm0
; SSE41-NEXT:    pand {{.*}}(%rip), %xmm0
; SSE41-NEXT:    movdqa {{.*#+}} xmm2 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; SSE41-NEXT:    pxor %xmm2, %xmm0
; SSE41-NEXT:    psubb %xmm2, %xmm0
; SSE41-NEXT:    psrlw $7, %xmm1
; SSE41-NEXT:    pand {{.*}}(%rip), %xmm1
; SSE41-NEXT:    paddb %xmm0, %xmm1
; SSE41-NEXT:    movdqa %xmm1, %xmm0
; SSE41-NEXT:    retq
;
; AVX1-LABEL: test_div7_16i8:
; AVX1:       # %bb.0:
; AVX1-NEXT:    vpmovsxbw %xmm0, %xmm1
; AVX1-NEXT:    vmovdqa {{.*#+}} xmm2 = [65427,65427,65427,65427,65427,65427,65427,65427]
; AVX1-NEXT:    vpmullw %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpsrlw $8, %xmm1, %xmm1
; AVX1-NEXT:    vpshufd {{.*#+}} xmm3 = xmm0[2,3,0,1]
; AVX1-NEXT:    vpmovsxbw %xmm3, %xmm3
; AVX1-NEXT:    vpmullw %xmm2, %xmm3, %xmm2
; AVX1-NEXT:    vpsrlw $8, %xmm2, %xmm2
; AVX1-NEXT:    vpackuswb %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpaddb %xmm0, %xmm1, %xmm0
; AVX1-NEXT:    vpsrlw $2, %xmm0, %xmm1
; AVX1-NEXT:    vpand {{.*}}(%rip), %xmm1, %xmm1
; AVX1-NEXT:    vmovdqa {{.*#+}} xmm2 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; AVX1-NEXT:    vpxor %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpsubb %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpsrlw $7, %xmm0, %xmm0
; AVX1-NEXT:    vpand {{.*}}(%rip), %xmm0, %xmm0
; AVX1-NEXT:    vpaddb %xmm0, %xmm1, %xmm0
; AVX1-NEXT:    retq
;
; AVX2NOBW-LABEL: test_div7_16i8:
; AVX2NOBW:       # %bb.0:
; AVX2NOBW-NEXT:    vpmovsxbw %xmm0, %ymm1
; AVX2NOBW-NEXT:    vpmullw {{.*}}(%rip), %ymm1, %ymm1
; AVX2NOBW-NEXT:    vpsrlw $8, %ymm1, %ymm1
; AVX2NOBW-NEXT:    vextracti128 $1, %ymm1, %xmm2
; AVX2NOBW-NEXT:    vpackuswb %xmm2, %xmm1, %xmm1
; AVX2NOBW-NEXT:    vpaddb %xmm0, %xmm1, %xmm0
; AVX2NOBW-NEXT:    vpsrlw $2, %xmm0, %xmm1
; AVX2NOBW-NEXT:    vpand {{.*}}(%rip), %xmm1, %xmm1
; AVX2NOBW-NEXT:    vmovdqa {{.*#+}} xmm2 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; AVX2NOBW-NEXT:    vpxor %xmm2, %xmm1, %xmm1
; AVX2NOBW-NEXT:    vpsubb %xmm2, %xmm1, %xmm1
; AVX2NOBW-NEXT:    vpsrlw $7, %xmm0, %xmm0
; AVX2NOBW-NEXT:    vpand {{.*}}(%rip), %xmm0, %xmm0
; AVX2NOBW-NEXT:    vpaddb %xmm0, %xmm1, %xmm0
; AVX2NOBW-NEXT:    vzeroupper
; AVX2NOBW-NEXT:    retq
;
; AVX512BW-LABEL: test_div7_16i8:
; AVX512BW:       # %bb.0:
; AVX512BW-NEXT:    vpmovsxbw %xmm0, %ymm1
; AVX512BW-NEXT:    vpmullw {{.*}}(%rip), %ymm1, %ymm1
; AVX512BW-NEXT:    vpsrlw $8, %ymm1, %ymm1
; AVX512BW-NEXT:    vpmovwb %zmm1, %ymm1
; AVX512BW-NEXT:    vpaddb %xmm0, %xmm1, %xmm0
; AVX512BW-NEXT:    vpsrlw $2, %xmm0, %xmm1
; AVX512BW-NEXT:    vpand {{.*}}(%rip), %xmm1, %xmm1
; AVX512BW-NEXT:    vmovdqa {{.*#+}} xmm2 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; AVX512BW-NEXT:    vpxor %xmm2, %xmm1, %xmm1
; AVX512BW-NEXT:    vpsubb %xmm2, %xmm1, %xmm1
; AVX512BW-NEXT:    vpsrlw $7, %xmm0, %xmm0
; AVX512BW-NEXT:    vpand {{.*}}(%rip), %xmm0, %xmm0
; AVX512BW-NEXT:    vpaddb %xmm0, %xmm1, %xmm0
; AVX512BW-NEXT:    vzeroupper
; AVX512BW-NEXT:    retq
  %res = sdiv <16 x i8> %a, <i8 7, i8 7, i8 7, i8 7,i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7,i8 7, i8 7, i8 7, i8 7>
  ret <16 x i8> %res
}

;
; srem by 7
;

define <2 x i64> @test_rem7_2i64(<2 x i64> %a) nounwind {
; SSE2-LABEL: test_rem7_2i64:
; SSE2:       # %bb.0:
; SSE2-NEXT:    movq %xmm0, %rcx
; SSE2-NEXT:    movabsq $5270498306774157605, %rsi # imm = 0x4924924924924925
; SSE2-NEXT:    movq %rcx, %rax
; SSE2-NEXT:    imulq %rsi
; SSE2-NEXT:    movq %rdx, %rax
; SSE2-NEXT:    shrq $63, %rax
; SSE2-NEXT:    sarq %rdx
; SSE2-NEXT:    addq %rax, %rdx
; SSE2-NEXT:    leaq (,%rdx,8), %rax
; SSE2-NEXT:    subq %rax, %rdx
; SSE2-NEXT:    addq %rcx, %rdx
; SSE2-NEXT:    movq %rdx, %xmm1
; SSE2-NEXT:    pshufd {{.*#+}} xmm0 = xmm0[2,3,0,1]
; SSE2-NEXT:    movq %xmm0, %rcx
; SSE2-NEXT:    movq %rcx, %rax
; SSE2-NEXT:    imulq %rsi
; SSE2-NEXT:    movq %rdx, %rax
; SSE2-NEXT:    shrq $63, %rax
; SSE2-NEXT:    sarq %rdx
; SSE2-NEXT:    addq %rax, %rdx
; SSE2-NEXT:    leaq (,%rdx,8), %rax
; SSE2-NEXT:    subq %rax, %rdx
; SSE2-NEXT:    addq %rcx, %rdx
; SSE2-NEXT:    movq %rdx, %xmm0
; SSE2-NEXT:    punpcklqdq {{.*#+}} xmm1 = xmm1[0],xmm0[0]
; SSE2-NEXT:    movdqa %xmm1, %xmm0
; SSE2-NEXT:    retq
;
; SSE41-LABEL: test_rem7_2i64:
; SSE41:       # %bb.0:
; SSE41-NEXT:    pextrq $1, %xmm0, %rcx
; SSE41-NEXT:    movabsq $5270498306774157605, %rsi # imm = 0x4924924924924925
; SSE41-NEXT:    movq %rcx, %rax
; SSE41-NEXT:    imulq %rsi
; SSE41-NEXT:    movq %rdx, %rax
; SSE41-NEXT:    shrq $63, %rax
; SSE41-NEXT:    sarq %rdx
; SSE41-NEXT:    addq %rax, %rdx
; SSE41-NEXT:    leaq (,%rdx,8), %rax
; SSE41-NEXT:    subq %rax, %rdx
; SSE41-NEXT:    addq %rcx, %rdx
; SSE41-NEXT:    movq %rdx, %xmm1
; SSE41-NEXT:    movq %xmm0, %rcx
; SSE41-NEXT:    movq %rcx, %rax
; SSE41-NEXT:    imulq %rsi
; SSE41-NEXT:    movq %rdx, %rax
; SSE41-NEXT:    shrq $63, %rax
; SSE41-NEXT:    sarq %rdx
; SSE41-NEXT:    addq %rax, %rdx
; SSE41-NEXT:    leaq (,%rdx,8), %rax
; SSE41-NEXT:    subq %rax, %rdx
; SSE41-NEXT:    addq %rcx, %rdx
; SSE41-NEXT:    movq %rdx, %xmm0
; SSE41-NEXT:    punpcklqdq {{.*#+}} xmm0 = xmm0[0],xmm1[0]
; SSE41-NEXT:    retq
;
; AVX-LABEL: test_rem7_2i64:
; AVX:       # %bb.0:
; AVX-NEXT:    vpextrq $1, %xmm0, %rcx
; AVX-NEXT:    movabsq $5270498306774157605, %rsi # imm = 0x4924924924924925
; AVX-NEXT:    movq %rcx, %rax
; AVX-NEXT:    imulq %rsi
; AVX-NEXT:    movq %rdx, %rax
; AVX-NEXT:    shrq $63, %rax
; AVX-NEXT:    sarq %rdx
; AVX-NEXT:    addq %rax, %rdx
; AVX-NEXT:    leaq (,%rdx,8), %rax
; AVX-NEXT:    subq %rax, %rdx
; AVX-NEXT:    addq %rcx, %rdx
; AVX-NEXT:    vmovq %rdx, %xmm1
; AVX-NEXT:    vmovq %xmm0, %rcx
; AVX-NEXT:    movq %rcx, %rax
; AVX-NEXT:    imulq %rsi
; AVX-NEXT:    movq %rdx, %rax
; AVX-NEXT:    shrq $63, %rax
; AVX-NEXT:    sarq %rdx
; AVX-NEXT:    addq %rax, %rdx
; AVX-NEXT:    leaq (,%rdx,8), %rax
; AVX-NEXT:    subq %rax, %rdx
; AVX-NEXT:    addq %rcx, %rdx
; AVX-NEXT:    vmovq %rdx, %xmm0
; AVX-NEXT:    vpunpcklqdq {{.*#+}} xmm0 = xmm0[0],xmm1[0]
; AVX-NEXT:    retq
  %res = srem <2 x i64> %a, <i64 7, i64 7>
  ret <2 x i64> %res
}

define <4 x i32> @test_rem7_4i32(<4 x i32> %a) nounwind {
; SSE2-LABEL: test_rem7_4i32:
; SSE2:       # %bb.0:
; SSE2-NEXT:    movdqa {{.*#+}} xmm1 = [2454267027,2454267027,2454267027,2454267027]
; SSE2-NEXT:    movdqa %xmm0, %xmm2
; SSE2-NEXT:    pmuludq %xmm1, %xmm2
; SSE2-NEXT:    pshufd {{.*#+}} xmm2 = xmm2[1,3,2,3]
; SSE2-NEXT:    pshufd {{.*#+}} xmm3 = xmm0[1,1,3,3]
; SSE2-NEXT:    pmuludq %xmm1, %xmm3
; SSE2-NEXT:    pshufd {{.*#+}} xmm3 = xmm3[1,3,2,3]
; SSE2-NEXT:    punpckldq {{.*#+}} xmm2 = xmm2[0],xmm3[0],xmm2[1],xmm3[1]
; SSE2-NEXT:    pxor %xmm3, %xmm3
; SSE2-NEXT:    pcmpgtd %xmm0, %xmm3
; SSE2-NEXT:    pand %xmm1, %xmm3
; SSE2-NEXT:    paddd %xmm0, %xmm3
; SSE2-NEXT:    psubd %xmm3, %xmm2
; SSE2-NEXT:    paddd %xmm0, %xmm2
; SSE2-NEXT:    movdqa %xmm2, %xmm1
; SSE2-NEXT:    psrld $31, %xmm1
; SSE2-NEXT:    psrad $2, %xmm2
; SSE2-NEXT:    paddd %xmm1, %xmm2
; SSE2-NEXT:    movdqa %xmm2, %xmm1
; SSE2-NEXT:    pslld $3, %xmm1
; SSE2-NEXT:    psubd %xmm1, %xmm2
; SSE2-NEXT:    paddd %xmm2, %xmm0
; SSE2-NEXT:    retq
;
; SSE41-LABEL: test_rem7_4i32:
; SSE41:       # %bb.0:
; SSE41-NEXT:    pshufd {{.*#+}} xmm1 = xmm0[1,1,3,3]
; SSE41-NEXT:    movdqa {{.*#+}} xmm2 = [2454267027,2454267027,2454267027,2454267027]
; SSE41-NEXT:    pmuldq %xmm2, %xmm1
; SSE41-NEXT:    pmuldq %xmm0, %xmm2
; SSE41-NEXT:    pshufd {{.*#+}} xmm2 = xmm2[1,1,3,3]
; SSE41-NEXT:    pblendw {{.*#+}} xmm2 = xmm2[0,1],xmm1[2,3],xmm2[4,5],xmm1[6,7]
; SSE41-NEXT:    paddd %xmm0, %xmm2
; SSE41-NEXT:    movdqa %xmm2, %xmm1
; SSE41-NEXT:    psrld $31, %xmm1
; SSE41-NEXT:    psrad $2, %xmm2
; SSE41-NEXT:    paddd %xmm1, %xmm2
; SSE41-NEXT:    pmulld {{.*}}(%rip), %xmm2
; SSE41-NEXT:    psubd %xmm2, %xmm0
; SSE41-NEXT:    retq
;
; AVX1-LABEL: test_rem7_4i32:
; AVX1:       # %bb.0:
; AVX1-NEXT:    vpshufd {{.*#+}} xmm1 = xmm0[1,1,3,3]
; AVX1-NEXT:    vmovdqa {{.*#+}} xmm2 = [2454267027,2454267027,2454267027,2454267027]
; AVX1-NEXT:    vpmuldq %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpmuldq %xmm2, %xmm0, %xmm2
; AVX1-NEXT:    vpshufd {{.*#+}} xmm2 = xmm2[1,1,3,3]
; AVX1-NEXT:    vpblendw {{.*#+}} xmm1 = xmm2[0,1],xmm1[2,3],xmm2[4,5],xmm1[6,7]
; AVX1-NEXT:    vpaddd %xmm0, %xmm1, %xmm1
; AVX1-NEXT:    vpsrld $31, %xmm1, %xmm2
; AVX1-NEXT:    vpsrad $2, %xmm1, %xmm1
; AVX1-NEXT:    vpaddd %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpmulld {{.*}}(%rip), %xmm1, %xmm1
; AVX1-NEXT:    vpsubd %xmm1, %xmm0, %xmm0
; AVX1-NEXT:    retq
;
; AVX2-LABEL: test_rem7_4i32:
; AVX2:       # %bb.0:
; AVX2-NEXT:    vpshufd {{.*#+}} xmm1 = xmm0[1,1,3,3]
; AVX2-NEXT:    vpbroadcastd {{.*#+}} xmm2 = [2454267027,2454267027,2454267027,2454267027]
; AVX2-NEXT:    vpmuldq %xmm2, %xmm1, %xmm1
; AVX2-NEXT:    vpmuldq %xmm2, %xmm0, %xmm2
; AVX2-NEXT:    vpshufd {{.*#+}} xmm2 = xmm2[1,1,3,3]
; AVX2-NEXT:    vpblendd {{.*#+}} xmm1 = xmm2[0],xmm1[1],xmm2[2],xmm1[3]
; AVX2-NEXT:    vpaddd %xmm0, %xmm1, %xmm1
; AVX2-NEXT:    vpsrld $31, %xmm1, %xmm2
; AVX2-NEXT:    vpsrad $2, %xmm1, %xmm1
; AVX2-NEXT:    vpaddd %xmm2, %xmm1, %xmm1
; AVX2-NEXT:    vpbroadcastd {{.*#+}} xmm2 = [7,7,7,7]
; AVX2-NEXT:    vpmulld %xmm2, %xmm1, %xmm1
; AVX2-NEXT:    vpsubd %xmm1, %xmm0, %xmm0
; AVX2-NEXT:    retq
  %res = srem <4 x i32> %a, <i32 7, i32 7, i32 7, i32 7>
  ret <4 x i32> %res
}

define <8 x i16> @test_rem7_8i16(<8 x i16> %a) nounwind {
; SSE-LABEL: test_rem7_8i16:
; SSE:       # %bb.0:
; SSE-NEXT:    movdqa {{.*#+}} xmm1 = [18725,18725,18725,18725,18725,18725,18725,18725]
; SSE-NEXT:    pmulhw %xmm0, %xmm1
; SSE-NEXT:    movdqa %xmm1, %xmm2
; SSE-NEXT:    psrlw $15, %xmm2
; SSE-NEXT:    psraw $1, %xmm1
; SSE-NEXT:    paddw %xmm2, %xmm1
; SSE-NEXT:    pmullw {{.*}}(%rip), %xmm1
; SSE-NEXT:    psubw %xmm1, %xmm0
; SSE-NEXT:    retq
;
; AVX-LABEL: test_rem7_8i16:
; AVX:       # %bb.0:
; AVX-NEXT:    vpmulhw {{.*}}(%rip), %xmm0, %xmm1
; AVX-NEXT:    vpsrlw $15, %xmm1, %xmm2
; AVX-NEXT:    vpsraw $1, %xmm1, %xmm1
; AVX-NEXT:    vpaddw %xmm2, %xmm1, %xmm1
; AVX-NEXT:    vpmullw {{.*}}(%rip), %xmm1, %xmm1
; AVX-NEXT:    vpsubw %xmm1, %xmm0, %xmm0
; AVX-NEXT:    retq
  %res = srem <8 x i16> %a, <i16 7, i16 7, i16 7, i16 7, i16 7, i16 7, i16 7, i16 7>
  ret <8 x i16> %res
}

define <16 x i8> @test_rem7_16i8(<16 x i8> %a) nounwind {
; SSE2-LABEL: test_rem7_16i8:
; SSE2:       # %bb.0:
; SSE2-NEXT:    punpckhbw {{.*#+}} xmm2 = xmm2[8],xmm0[8],xmm2[9],xmm0[9],xmm2[10],xmm0[10],xmm2[11],xmm0[11],xmm2[12],xmm0[12],xmm2[13],xmm0[13],xmm2[14],xmm0[14],xmm2[15],xmm0[15]
; SSE2-NEXT:    psraw $8, %xmm2
; SSE2-NEXT:    movdqa {{.*#+}} xmm3 = [65427,65427,65427,65427,65427,65427,65427,65427]
; SSE2-NEXT:    pmullw %xmm3, %xmm2
; SSE2-NEXT:    psrlw $8, %xmm2
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm1 = xmm1[0],xmm0[0],xmm1[1],xmm0[1],xmm1[2],xmm0[2],xmm1[3],xmm0[3],xmm1[4],xmm0[4],xmm1[5],xmm0[5],xmm1[6],xmm0[6],xmm1[7],xmm0[7]
; SSE2-NEXT:    psraw $8, %xmm1
; SSE2-NEXT:    pmullw %xmm3, %xmm1
; SSE2-NEXT:    psrlw $8, %xmm1
; SSE2-NEXT:    packuswb %xmm2, %xmm1
; SSE2-NEXT:    paddb %xmm0, %xmm1
; SSE2-NEXT:    movdqa %xmm1, %xmm2
; SSE2-NEXT:    psrlw $2, %xmm2
; SSE2-NEXT:    pand {{.*}}(%rip), %xmm2
; SSE2-NEXT:    movdqa {{.*#+}} xmm3 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; SSE2-NEXT:    pxor %xmm3, %xmm2
; SSE2-NEXT:    psubb %xmm3, %xmm2
; SSE2-NEXT:    psrlw $7, %xmm1
; SSE2-NEXT:    pand {{.*}}(%rip), %xmm1
; SSE2-NEXT:    paddb %xmm2, %xmm1
; SSE2-NEXT:    movdqa %xmm1, %xmm2
; SSE2-NEXT:    psllw $3, %xmm2
; SSE2-NEXT:    pand {{.*}}(%rip), %xmm2
; SSE2-NEXT:    psubb %xmm2, %xmm1
; SSE2-NEXT:    paddb %xmm1, %xmm0
; SSE2-NEXT:    retq
;
; SSE41-LABEL: test_rem7_16i8:
; SSE41:       # %bb.0:
; SSE41-NEXT:    pmovsxbw %xmm0, %xmm1
; SSE41-NEXT:    movdqa {{.*#+}} xmm2 = [65427,65427,65427,65427,65427,65427,65427,65427]
; SSE41-NEXT:    pmullw %xmm2, %xmm1
; SSE41-NEXT:    psrlw $8, %xmm1
; SSE41-NEXT:    pshufd {{.*#+}} xmm3 = xmm0[2,3,0,1]
; SSE41-NEXT:    pmovsxbw %xmm3, %xmm3
; SSE41-NEXT:    pmullw %xmm2, %xmm3
; SSE41-NEXT:    psrlw $8, %xmm3
; SSE41-NEXT:    packuswb %xmm3, %xmm1
; SSE41-NEXT:    paddb %xmm0, %xmm1
; SSE41-NEXT:    movdqa %xmm1, %xmm2
; SSE41-NEXT:    psrlw $2, %xmm2
; SSE41-NEXT:    pand {{.*}}(%rip), %xmm2
; SSE41-NEXT:    movdqa {{.*#+}} xmm3 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; SSE41-NEXT:    pxor %xmm3, %xmm2
; SSE41-NEXT:    psubb %xmm3, %xmm2
; SSE41-NEXT:    psrlw $7, %xmm1
; SSE41-NEXT:    pand {{.*}}(%rip), %xmm1
; SSE41-NEXT:    paddb %xmm2, %xmm1
; SSE41-NEXT:    movdqa %xmm1, %xmm2
; SSE41-NEXT:    psllw $3, %xmm2
; SSE41-NEXT:    pand {{.*}}(%rip), %xmm2
; SSE41-NEXT:    psubb %xmm2, %xmm1
; SSE41-NEXT:    paddb %xmm1, %xmm0
; SSE41-NEXT:    retq
;
; AVX1-LABEL: test_rem7_16i8:
; AVX1:       # %bb.0:
; AVX1-NEXT:    vpmovsxbw %xmm0, %xmm1
; AVX1-NEXT:    vmovdqa {{.*#+}} xmm2 = [65427,65427,65427,65427,65427,65427,65427,65427]
; AVX1-NEXT:    vpmullw %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpsrlw $8, %xmm1, %xmm1
; AVX1-NEXT:    vpshufd {{.*#+}} xmm3 = xmm0[2,3,0,1]
; AVX1-NEXT:    vpmovsxbw %xmm3, %xmm3
; AVX1-NEXT:    vpmullw %xmm2, %xmm3, %xmm2
; AVX1-NEXT:    vpsrlw $8, %xmm2, %xmm2
; AVX1-NEXT:    vpackuswb %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpaddb %xmm0, %xmm1, %xmm1
; AVX1-NEXT:    vpsrlw $2, %xmm1, %xmm2
; AVX1-NEXT:    vpand {{.*}}(%rip), %xmm2, %xmm2
; AVX1-NEXT:    vmovdqa {{.*#+}} xmm3 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; AVX1-NEXT:    vpxor %xmm3, %xmm2, %xmm2
; AVX1-NEXT:    vpsubb %xmm3, %xmm2, %xmm2
; AVX1-NEXT:    vpsrlw $7, %xmm1, %xmm1
; AVX1-NEXT:    vpand {{.*}}(%rip), %xmm1, %xmm1
; AVX1-NEXT:    vpaddb %xmm1, %xmm2, %xmm1
; AVX1-NEXT:    vpsllw $3, %xmm1, %xmm2
; AVX1-NEXT:    vpand {{.*}}(%rip), %xmm2, %xmm2
; AVX1-NEXT:    vpsubb %xmm2, %xmm1, %xmm1
; AVX1-NEXT:    vpaddb %xmm1, %xmm0, %xmm0
; AVX1-NEXT:    retq
;
; AVX2NOBW-LABEL: test_rem7_16i8:
; AVX2NOBW:       # %bb.0:
; AVX2NOBW-NEXT:    vpmovsxbw %xmm0, %ymm1
; AVX2NOBW-NEXT:    vpmullw {{.*}}(%rip), %ymm1, %ymm1
; AVX2NOBW-NEXT:    vpsrlw $8, %ymm1, %ymm1
; AVX2NOBW-NEXT:    vextracti128 $1, %ymm1, %xmm2
; AVX2NOBW-NEXT:    vpackuswb %xmm2, %xmm1, %xmm1
; AVX2NOBW-NEXT:    vpaddb %xmm0, %xmm1, %xmm1
; AVX2NOBW-NEXT:    vpsrlw $2, %xmm1, %xmm2
; AVX2NOBW-NEXT:    vpand {{.*}}(%rip), %xmm2, %xmm2
; AVX2NOBW-NEXT:    vmovdqa {{.*#+}} xmm3 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; AVX2NOBW-NEXT:    vpxor %xmm3, %xmm2, %xmm2
; AVX2NOBW-NEXT:    vpsubb %xmm3, %xmm2, %xmm2
; AVX2NOBW-NEXT:    vpsrlw $7, %xmm1, %xmm1
; AVX2NOBW-NEXT:    vpand {{.*}}(%rip), %xmm1, %xmm1
; AVX2NOBW-NEXT:    vpaddb %xmm1, %xmm2, %xmm1
; AVX2NOBW-NEXT:    vpsllw $3, %xmm1, %xmm2
; AVX2NOBW-NEXT:    vpand {{.*}}(%rip), %xmm2, %xmm2
; AVX2NOBW-NEXT:    vpsubb %xmm2, %xmm1, %xmm1
; AVX2NOBW-NEXT:    vpaddb %xmm1, %xmm0, %xmm0
; AVX2NOBW-NEXT:    vzeroupper
; AVX2NOBW-NEXT:    retq
;
; AVX512BW-LABEL: test_rem7_16i8:
; AVX512BW:       # %bb.0:
; AVX512BW-NEXT:    vpmovsxbw %xmm0, %ymm1
; AVX512BW-NEXT:    vpmullw {{.*}}(%rip), %ymm1, %ymm1
; AVX512BW-NEXT:    vpsrlw $8, %ymm1, %ymm1
; AVX512BW-NEXT:    vpmovwb %zmm1, %ymm1
; AVX512BW-NEXT:    vpaddb %xmm0, %xmm1, %xmm1
; AVX512BW-NEXT:    vpsrlw $2, %xmm1, %xmm2
; AVX512BW-NEXT:    vpand {{.*}}(%rip), %xmm2, %xmm2
; AVX512BW-NEXT:    vmovdqa {{.*#+}} xmm3 = [32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32]
; AVX512BW-NEXT:    vpxor %xmm3, %xmm2, %xmm2
; AVX512BW-NEXT:    vpsubb %xmm3, %xmm2, %xmm2
; AVX512BW-NEXT:    vpsrlw $7, %xmm1, %xmm1
; AVX512BW-NEXT:    vpand {{.*}}(%rip), %xmm1, %xmm1
; AVX512BW-NEXT:    vpaddb %xmm1, %xmm2, %xmm1
; AVX512BW-NEXT:    vpsllw $3, %xmm1, %xmm2
; AVX512BW-NEXT:    vpand {{.*}}(%rip), %xmm2, %xmm2
; AVX512BW-NEXT:    vpsubb %xmm2, %xmm1, %xmm1
; AVX512BW-NEXT:    vpaddb %xmm1, %xmm0, %xmm0
; AVX512BW-NEXT:    vzeroupper
; AVX512BW-NEXT:    retq
  %res = srem <16 x i8> %a, <i8 7, i8 7, i8 7, i8 7,i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7,i8 7, i8 7, i8 7, i8 7>
  ret <16 x i8> %res
}

; This test is just to show what an scalarized v16i8 division looks like.
; FIXME: Fix the extract zero before inserting back into the vector.
define <16 x i8> @test_rem_variable_16i8(<16 x i8> %a, <16 x i8> %b) nounwind {
; SSE2-LABEL: test_rem_variable_16i8:
; SSE2:       # %bb.0:
; SSE2-NEXT:    movaps %xmm0, -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movaps %xmm1, -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm0
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm1
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm1 = xmm1[0],xmm0[0],xmm1[1],xmm0[1],xmm1[2],xmm0[2],xmm1[3],xmm0[3],xmm1[4],xmm0[4],xmm1[5],xmm0[5],xmm1[6],xmm0[6],xmm1[7],xmm0[7]
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm2
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm0
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm0 = xmm0[0],xmm2[0],xmm0[1],xmm2[1],xmm0[2],xmm2[2],xmm0[3],xmm2[3],xmm0[4],xmm2[4],xmm0[5],xmm2[5],xmm0[6],xmm2[6],xmm0[7],xmm2[7]
; SSE2-NEXT:    punpcklwd {{.*#+}} xmm0 = xmm0[0],xmm1[0],xmm0[1],xmm1[1],xmm0[2],xmm1[2],xmm0[3],xmm1[3]
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm1
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm2
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm2 = xmm2[0],xmm1[0],xmm2[1],xmm1[1],xmm2[2],xmm1[2],xmm2[3],xmm1[3],xmm2[4],xmm1[4],xmm2[5],xmm1[5],xmm2[6],xmm1[6],xmm2[7],xmm1[7]
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm3
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm1
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm1 = xmm1[0],xmm3[0],xmm1[1],xmm3[1],xmm1[2],xmm3[2],xmm1[3],xmm3[3],xmm1[4],xmm3[4],xmm1[5],xmm3[5],xmm1[6],xmm3[6],xmm1[7],xmm3[7]
; SSE2-NEXT:    punpcklwd {{.*#+}} xmm1 = xmm1[0],xmm2[0],xmm1[1],xmm2[1],xmm1[2],xmm2[2],xmm1[3],xmm2[3]
; SSE2-NEXT:    punpckldq {{.*#+}} xmm1 = xmm1[0],xmm0[0],xmm1[1],xmm0[1]
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm0
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm3
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm3 = xmm3[0],xmm0[0],xmm3[1],xmm0[1],xmm3[2],xmm0[2],xmm3[3],xmm0[3],xmm3[4],xmm0[4],xmm3[5],xmm0[5],xmm3[6],xmm0[6],xmm3[7],xmm0[7]
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm0
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm2
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm2 = xmm2[0],xmm0[0],xmm2[1],xmm0[1],xmm2[2],xmm0[2],xmm2[3],xmm0[3],xmm2[4],xmm0[4],xmm2[5],xmm0[5],xmm2[6],xmm0[6],xmm2[7],xmm0[7]
; SSE2-NEXT:    punpcklwd {{.*#+}} xmm2 = xmm2[0],xmm3[0],xmm2[1],xmm3[1],xmm2[2],xmm3[2],xmm2[3],xmm3[3]
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm0
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm3
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm3 = xmm3[0],xmm0[0],xmm3[1],xmm0[1],xmm3[2],xmm0[2],xmm3[3],xmm0[3],xmm3[4],xmm0[4],xmm3[5],xmm0[5],xmm3[6],xmm0[6],xmm3[7],xmm0[7]
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %al
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    movb -{{[0-9]+}}(%rsp), %cl
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm4
; SSE2-NEXT:    movl %ecx, %eax
; SSE2-NEXT:    cbtw
; SSE2-NEXT:    idivb -{{[0-9]+}}(%rsp)
; SSE2-NEXT:    movsbl %ah, %eax
; SSE2-NEXT:    movd %eax, %xmm0
; SSE2-NEXT:    punpcklbw {{.*#+}} xmm0 = xmm0[0],xmm4[0],xmm0[1],xmm4[1],xmm0[2],xmm4[2],xmm0[3],xmm4[3],xmm0[4],xmm4[4],xmm0[5],xmm4[5],xmm0[6],xmm4[6],xmm0[7],xmm4[7]
; SSE2-NEXT:    punpcklwd {{.*#+}} xmm0 = xmm0[0],xmm3[0],xmm0[1],xmm3[1],xmm0[2],xmm3[2],xmm0[3],xmm3[3]
; SSE2-NEXT:    punpckldq {{.*#+}} xmm0 = xmm0[0],xmm2[0],xmm0[1],xmm2[1]
; SSE2-NEXT:    punpcklqdq {{.*#+}} xmm0 = xmm0[0],xmm1[0]
; SSE2-NEXT:    retq
;
; SSE41-LABEL: test_rem_variable_16i8:
; SSE41:       # %bb.0:
; SSE41-NEXT:    pextrb $1, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pextrb $1, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $0, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pextrb $0, %xmm1, %edx
; SSE41-NEXT:    idivb %dl
; SSE41-NEXT:    movsbl %ah, %eax
; SSE41-NEXT:    movd %eax, %xmm2
; SSE41-NEXT:    pextrb $2, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $1, %ecx, %xmm2
; SSE41-NEXT:    pextrb $2, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $3, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $2, %ecx, %xmm2
; SSE41-NEXT:    pextrb $3, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $4, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $3, %ecx, %xmm2
; SSE41-NEXT:    pextrb $4, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $5, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $4, %ecx, %xmm2
; SSE41-NEXT:    pextrb $5, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $6, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $5, %ecx, %xmm2
; SSE41-NEXT:    pextrb $6, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $7, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $6, %ecx, %xmm2
; SSE41-NEXT:    pextrb $7, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $8, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $7, %ecx, %xmm2
; SSE41-NEXT:    pextrb $8, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $9, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $8, %ecx, %xmm2
; SSE41-NEXT:    pextrb $9, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $10, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $9, %ecx, %xmm2
; SSE41-NEXT:    pextrb $10, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $11, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $10, %ecx, %xmm2
; SSE41-NEXT:    pextrb $11, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $12, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $11, %ecx, %xmm2
; SSE41-NEXT:    pextrb $12, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $13, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $12, %ecx, %xmm2
; SSE41-NEXT:    pextrb $13, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $14, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $13, %ecx, %xmm2
; SSE41-NEXT:    pextrb $14, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %ecx
; SSE41-NEXT:    pextrb $15, %xmm0, %eax
; SSE41-NEXT:    # kill: def $al killed $al killed $eax
; SSE41-NEXT:    cbtw
; SSE41-NEXT:    pinsrb $14, %ecx, %xmm2
; SSE41-NEXT:    pextrb $15, %xmm1, %ecx
; SSE41-NEXT:    idivb %cl
; SSE41-NEXT:    movsbl %ah, %eax
; SSE41-NEXT:    pinsrb $15, %eax, %xmm2
; SSE41-NEXT:    movdqa %xmm2, %xmm0
; SSE41-NEXT:    retq
;
; AVX-LABEL: test_rem_variable_16i8:
; AVX:       # %bb.0:
; AVX-NEXT:    vpextrb $1, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpextrb $1, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $0, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpextrb $0, %xmm1, %edx
; AVX-NEXT:    idivb %dl
; AVX-NEXT:    movsbl %ah, %eax
; AVX-NEXT:    vmovd %eax, %xmm2
; AVX-NEXT:    vpextrb $2, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $1, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $2, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $3, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $2, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $3, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $4, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $3, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $4, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $5, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $4, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $5, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $6, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $5, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $6, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $7, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $6, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $7, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $8, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $7, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $8, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $9, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $8, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $9, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $10, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $9, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $10, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $11, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $10, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $11, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $12, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $11, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $12, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $13, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $12, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $13, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $14, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $13, %ecx, %xmm2, %xmm2
; AVX-NEXT:    vpextrb $14, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %ecx
; AVX-NEXT:    vpextrb $15, %xmm0, %eax
; AVX-NEXT:    # kill: def $al killed $al killed $eax
; AVX-NEXT:    cbtw
; AVX-NEXT:    vpinsrb $14, %ecx, %xmm2, %xmm0
; AVX-NEXT:    vpextrb $15, %xmm1, %ecx
; AVX-NEXT:    idivb %cl
; AVX-NEXT:    movsbl %ah, %eax
; AVX-NEXT:    vpinsrb $15, %eax, %xmm0, %xmm0
; AVX-NEXT:    retq
  %res = srem <16 x i8> %a, %b
  ret <16 x i8> %res
}
