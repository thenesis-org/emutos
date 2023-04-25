#include "asm.h"
#include "vdi/vdi_internal.h"

//********************************************************************************
// Common tools.
//********************************************************************************
#if CONF_WITH_VDI_PLANAR_1
#define vdi_Soft_planar1(a) a
#else
#define vdi_Soft_planar1(a)
#endif

#if CONF_WITH_VDI_PLANAR_2
#define vdi_Soft_planar2(a) a
#else
#define vdi_Soft_planar2(a)
#endif

#if CONF_WITH_VDI_PLANAR_4
#define vdi_Soft_planar4(a) a
#else
#define vdi_Soft_planar4(a)
#endif

#if CONF_WITH_VDI_PLANAR_8
#define vdi_Soft_planar8(a) a
#else
#define vdi_Soft_planar8(a)
#endif

forceinline ULONG broadcastWord(UWORD word) {
    ULONG word32;
    #if vdi_Soft_asmEnabled
    __asm__ volatile (
        "move.w %1,%0\n\t"
        "swap %0\n\t"
        "move.w %1,%0\n\t"
        : "=&d" (word32) : "r" (word) // word32 is an early clobber operand (clobbered before all reads finished), so the "&".
    );
    #else
    word32 = ((ULONG)word << 16) | word;
    #endif
    return word32;
}

forceinline UWORD colorToMaskUWORD(UWORD *color) {
    UWORD c = *color;
    UWORD m;
    #if vdi_Soft_asmEnabled
    __asm__ volatile (
        "moveq #0,%0\n\t"
        "lsr.w #1,%1\n\t"
        "negx.w %0\n\t"
        : "=d" (m), "+d" (c) :
    );
    #else
    m = (UWORD)-(c & 1);
    #endif
    *color = c;
    return m;
}

forceinline ULONG colorToMaskULONG(UWORD *color) {
    UWORD c = *color;
    ULONG m;
    #if vdi_Soft_asmEnabled
    __asm__ volatile (
        "moveq #0,%0\n\t"
        "lsr.w #1,%1\n\t"
        "negx.w %0\n\t"
        "swap %d0\n\t"
        "lsr.w #1,%1\n\t"
        "negx.w %0\n\t"
        : "=d" (m), "+d" (c) :
    );
    #else
    m = (UWORD)-(c & 1); c >>= 1; m <<= 16; m |= (UWORD)-(c & 1); c >>= 1;
    #endif
    *color = c;
    return m;
}

//********************************************************************************
// Span filling - Single plane.
//********************************************************************************
#if !vdi_blitterOnly

// Versions from smallest to biggest and slowest to fastest.
#define vdi_Soft_fillSpanSinglePlane_version_smallest 0 // Version without assembly and with the smallest generated code.
#define vdi_Soft_fillSpanSinglePlane_version_switch 1 // Version without assembly but optimized using a switch. Bigger code than "smallest" version and does not work as expected. Here for reference only.
#define vdi_Soft_fillSpanSinglePlane_version_assembly 2 // Version with assembly. Significantly faster and bigger code than "smallest" because all cases are generated with separate code.

#if defined(TARGET_192)
#define vdi_Soft_fillSpanSinglePlane_version vdi_Soft_fillSpanSinglePlane_version_smallest
#elif vdi_Soft_asmEnabled
#define vdi_Soft_fillSpanSinglePlane_version vdi_Soft_fillSpanSinglePlane_version_assembly
#else
#define vdi_Soft_fillSpanSinglePlane_version vdi_Soft_fillSpanSinglePlane_version_smallest
#endif

// We use these macros because we want to use dbra in the inner loops.
// We need rightFlag because we may have a number of pixels less than or equal to 16.
#if vdi_Soft_fillSpanSinglePlane_version == vdi_Soft_fillSpanSinglePlane_version_smallest
// This is the most compact version, but it is slow because of the loop overhead (one decrement and branch for every 16-bit word).
// For the loop, -3 is subtracted to wordCount and wordCount is compared to -1 to allow the use of dbra.
#define vdi_Soft_fillSpanSinglePlaneAsmEnabled 0
#define vdi_Soft_fillSpanSinglePlaneLoop(pattern, src, srcLeft, srcRight) \
    { \
        UWORD * RESTRICT dstWord; \
        dstWord = (UWORD*)dst; *dstWord = srcLeft; dst += dstWordStride; \
        WORD wordNbPlane = wordNb - 2; \
        if (wordNbPlane >= 0) { \
            if (wordNbPlane > 0) { \
                wordNbPlane--; \
                do { \
                    dstWord = (UWORD*)dst; *dstWord = src; dst += dstWordStride; \
                } while (--wordNbPlane != -1); \
            } \
            dstWord = (UWORD*)dst; *dstWord = srcRight; \
        } \
    }
#elif vdi_Soft_fillSpanSinglePlane_version == vdi_Soft_fillSpanSinglePlane_version_assembly
// This is the fastest version, with clean code generated. It uses one decrement and branch for 16x 16-bit words.
#define vdi_Soft_fillSpanSinglePlaneAsmEnabled 1
#define vdi_Soft_fillSpanSinglePlaneLoop(pattern, body, srcLeft, srcRight) \
    { \
        UWORD patternReg = pattern; \
        UWORD * RESTRICT dstWord; \
        dstWord = (UWORD*)dst; *dstWord = srcLeft; dst += dstWordStride; \
        WORD wordNbPlane = wordNb - 2; \
        if (wordNbPlane >= 0) { \
            if (wordNbPlane > 0) { \
                wordNbPlane--; \
                WORD wordNb16 = wordNbPlane >> 4; \
                WORD wordNbModulo16 = ~(wordNbPlane & 0xf); \
                WORD offset = wordNbModulo16 << 2; \
                __asm__ volatile ( \
                    "jmp (2f, PC, %4.w)\n\t" \
                    "1:\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    body "\n\tadda.l %2,%1\n\t" \
                    "2:\n\t" \
                    "dbra %0,1b\n\t" \
                    : "+d" (wordNb16), \
                      "+a" (dst) \
                    : "a" (dstWordStride), \
                      "d" (patternReg), \
                      "d" (offset) \
                ); \
            } \
            dstWord = (UWORD*)dst; *dstWord = srcRight; \
        } \
    }
#endif

forceinline void vdi_Soft_fillSpanSinglePlane(WORD mode, UWORD color, UWORD pattern, WORD dstWordStride, UBYTE * RESTRICT dst, UWORD leftMask, UWORD rightMask, WORD wordNb) {
    switch (mode) {
    case WM_TRANS:
    case WM_ERASE:
        if (mode == WM_ERASE)
            pattern = ~pattern;
        if (pattern == 0x0000) return;
        if (pattern != 0xffff) {
            if (color & 1) {
                #if vdi_Soft_fillSpanSinglePlaneAsmEnabled
                vdi_Soft_fillSpanSinglePlaneLoop(pattern, "or.w %3,(%1)", *dstWord | (pattern & leftMask), *dstWord | (pattern & rightMask));
                #else
                vdi_Soft_fillSpanSinglePlaneLoop(pattern, *dstWord | pattern, *dstWord | (pattern & leftMask), *dstWord | (pattern & rightMask));
                #endif
            } else {
                #if vdi_Soft_fillSpanSinglePlaneAsmEnabled
                vdi_Soft_fillSpanSinglePlaneLoop(~pattern, "and.w %3,(%1)", *dstWord & ~(pattern & leftMask), *dstWord & ~(pattern & rightMask));
                #else
                vdi_Soft_fillSpanSinglePlaneLoop(~pattern, *dstWord & ~pattern, *dstWord & ~(pattern & leftMask), *dstWord & ~(pattern & rightMask));
                #endif
            }
            break;
        }
        // Fallthrough. If the pattern is solid, this is the same as replace, but replace is faster.
    default:
    case WM_REPLACE:
        if ((color & 1) == 0)
            pattern = 0x0000;
        #if vdi_Soft_fillSpanSinglePlaneAsmEnabled
        vdi_Soft_fillSpanSinglePlaneLoop(pattern, "mov.w %3,(%1)", mergePixel(*dstWord, pattern, leftMask), mergePixel(*dstWord, pattern, rightMask));
        #else
        vdi_Soft_fillSpanSinglePlaneLoop(pattern, pattern, mergePixel(*dstWord, pattern, leftMask), mergePixel(*dstWord, pattern, rightMask));
        #endif
        break;
    case WM_XOR:
        if (pattern == 0x0000) return;
        #if vdi_Soft_fillSpanSinglePlaneAsmEnabled
        vdi_Soft_fillSpanSinglePlaneLoop(pattern, "eor.w %3,(%1)", *dstWord ^ (pattern & leftMask), *dstWord ^ (pattern & rightMask));
        #else
        vdi_Soft_fillSpanSinglePlaneLoop(pattern, *dstWord ^ pattern, *dstWord ^ (pattern & leftMask), *dstWord ^ (pattern & rightMask));
        #endif
        break;
    }
}

#endif

//********************************************************************************
// Span filling - Multi plane.
//********************************************************************************
#if !vdi_blitterOnly

#define vdi_Soft_fillSpanMultiplaneAsmEnabled vdi_Soft_asmEnabled

#define vdi_Soft_fillSpanMultiplaneColor(op, type, regIndex) \
    vdi_Soft_fillSpanMultiplaneColor##op(type, regIndex)
#define vdi_Soft_fillSpanMultiplanePattern(op, type, regIndex) \
    vdi_Soft_fillSpanMultiplanePattern##op(type, regIndex)

#define vdi_Soft_fillSpanMultiplaneFringeBegin(op, mask, type, size) \
    vdi_Soft_fillSpanMultiplaneFringeBegin##op(mask, type, size)
#define vdi_Soft_fillSpanMultiplaneFringeOp(op, mask, type, size, regIndex) \
    vdi_Soft_fillSpanMultiplaneFringeOp##op(mask, type, size, regIndex)
#define vdi_Soft_fillSpanMultiplaneFringeEnd(op, mask, regNb) \
    vdi_Soft_fillSpanMultiplaneFringeEnd##op(mask, regNb)

#define vdi_Soft_fillSpanMultiplaneBegin(op, type, size, solid) \
    vdi_Soft_fillSpanMultiplaneBegin##op(type, size, solid)
#define vdi_Soft_fillSpanMultiplaneOp(op, type, size, solid, regIndex) \
    vdi_Soft_fillSpanMultiplaneOp##op(type, size, solid, regIndex)
#define vdi_Soft_fillSpanMultiplaneEnd(op, regNb, solid) \
    vdi_Soft_fillSpanMultiplaneEnd##op(regNb, solid)

//--------------------------------------------------------------------------------
// Span filling - Multi plane - Assembly.
//--------------------------------------------------------------------------------
// TODO: Use jump into unrolled loop for replace and xor.
#if vdi_Soft_fillSpanMultiplaneAsmEnabled

#define vdi_Soft_fillSpanMultiplanePatternReg(type, regIndex) register type pattern##regIndex __asm__("d"#regIndex)
#define vdi_Soft_fillSpanMultiplanePatternRegs1 "d"(pattern0)
#define vdi_Soft_fillSpanMultiplanePatternRegs2 "d"(pattern0), "d"(pattern1)
#define vdi_Soft_fillSpanMultiplanePatternRegs4 "d"(pattern0), "d"(pattern1), "d"(pattern2), "d"(pattern3)

//
// Replace.
//

// Color and pattern.
#define vdi_Soft_fillSpanMultiplaneColorReplace(type, regIndex) \
    type color##regIndex = colorToMask##type(&color);
#define vdi_Soft_fillSpanMultiplanePatternReplace(type, regIndex) \
    vdi_Soft_fillSpanMultiplanePatternReg(type, regIndex) = color##regIndex & pattern;
// Fringe.
#define vdi_Soft_fillSpanMultiplaneFringeBeginReplace(mask, type, size) \
    { \
        ULONG t0, t1; \
        __asm__ volatile (
#define vdi_Soft_fillSpanMultiplaneFringeOpReplace(mask, type, size, regIndex) \
            "move."#size" (%0),%1\n\t" \
            "move."#size" %1,%2\n\t" \
            "eor."#size" d"#regIndex",%2\n\t" \
            "and."#size" %3,%2\n\t" \
            "eor."#size" %1,%2\n\t" \
            "move."#size" %2,(%0)+\n\t"
#define vdi_Soft_fillSpanMultiplaneFringeEndReplace(mask, regNb) \
            : "+a"(d), "=&d"(t0), "=&d"(t1) : "d"(mask), vdi_Soft_fillSpanMultiplanePatternRegs##regNb : "cc" \
        ); \
    }
// Middle.
#define vdi_Soft_fillSpanMultiplaneBeginReplace(type, size, solid) \
    { \
        __asm__ volatile ( \
            "subq.w #1,%1\n\t" \
            "1:\n\t"
#define vdi_Soft_fillSpanMultiplaneOpReplace(type, size, solid, regIndex) \
            "move."#size" d"#regIndex",(%0)+\n\t"
#define vdi_Soft_fillSpanMultiplaneEndReplace(regNb, solid) \
            "dbra %1,1b\n\t" \
            : "+a"(d), "+d"(wordCount) : vdi_Soft_fillSpanMultiplanePatternRegs##regNb : "cc" \
        ); \
    }

//
// Xor.
//

// Color and pattern. Ignored for XOR.
#define vdi_Soft_fillSpanMultiplaneColorXor(type, regIndex)
#define vdi_Soft_fillSpanMultiplanePatternXor(type, regIndex)
// Fringe.
#define vdi_Soft_fillSpanMultiplaneFringeBeginXor(mask, type, size) \
    { \
        type patternAndMask = pattern & mask; \
        __asm__ volatile (
#define vdi_Soft_fillSpanMultiplaneFringeOpXor(mask, type, size, regIndex) \
            "eor."#size" %1,(%0)+\n\t"
#define vdi_Soft_fillSpanMultiplaneFringeEndXor(mask, regNb) \
            : "+a"(d) : "d"(patternAndMask) : "cc" \
        ); \
    }
// Middle.
#define vdi_Soft_fillSpanMultiplaneBeginXor(type, size, solid) \
    vdi_Soft_fillSpanMultiplaneBeginXor_##solid(type, size) 
#define vdi_Soft_fillSpanMultiplaneOpXor(type, size, solid, regIndex) \
    vdi_Soft_fillSpanMultiplaneOpXor_##solid(type, size, regIndex) 
#define vdi_Soft_fillSpanMultiplaneEndXor(regNb, solid) \
    vdi_Soft_fillSpanMultiplaneEndXor_##solid(regNb) 
// When the pattern is not solid.
#define vdi_Soft_fillSpanMultiplaneBeginXor_0(type, size) \
    { \
        __asm__ volatile ( \
            "subq.w #1,%1\n\t" \
            "1:\n\t"
#define vdi_Soft_fillSpanMultiplaneOpXor_0(type, size, regIndex) \
            "eor."#size" %2,(%0)+\n\t"
#define vdi_Soft_fillSpanMultiplaneEndXor_0(regNb) \
            "dbra %1,1b\n\t" \
            : "+a"(d), "+d"(wordCount) : "d"(pattern) : "cc" \
        ); \
    }
// When the pattern is solid (all ones).
#define vdi_Soft_fillSpanMultiplaneBeginXor_1(type, size) \
    { \
        __asm__ volatile ( \
            "subq.w #1,%1\n\t" \
            "1:\n\t"
#define vdi_Soft_fillSpanMultiplaneOpXor_1(type, size, regIndex) \
            "not."#size" (%0)+\n\t"
#define vdi_Soft_fillSpanMultiplaneEndXor_1(regNb) \
            "dbra %1,1b\n\t" \
            : "+a"(d), "+d"(wordCount) :  : "cc" \
        ); \
    }

//
// Trans.
// Not used when the line pattern is solid because it is the same as replace.
//

// Color and pattern.
#define vdi_Soft_fillSpanMultiplaneColorTrans(type, regIndex) \
    type color##regIndex = colorToMask##type(&color);
#define vdi_Soft_fillSpanMultiplanePatternTrans(type, regIndex) \
    vdi_Soft_fillSpanMultiplanePatternReg(type, regIndex) = color##regIndex & pattern;
// Fringe.
#define vdi_Soft_fillSpanMultiplaneFringeBeginTrans(mask, type, size) \
    { \
        type patternAndMask = pattern & mask; \
        ULONG t0, t1; \
        __asm__ volatile (
#define vdi_Soft_fillSpanMultiplaneFringeOpTrans(mask, type, size, regIndex) \
            "move."#size" (%0),%1\n\t" \
            "move."#size" %1,%2\n\t" \
            "eor."#size" d"#regIndex",%2\n\t" \
            "and."#size" %3,%2\n\t" \
            "eor."#size" %1,%2\n\t" \
            "move."#size" %2,(%0)+\n\t"
#define vdi_Soft_fillSpanMultiplaneFringeEndTrans(mask, regNb) \
            : "+a"(d), "=&d"(t0), "=&d"(t1) : "d"(patternAndMask), vdi_Soft_fillSpanMultiplanePatternRegs##regNb : "cc" \
        ); \
    }
// Middle.
#define vdi_Soft_fillSpanMultiplaneBeginTrans(type, size, solid) \
    { \
        type patternInverse = ~pattern; \
        ULONG t; \
        __asm__ volatile (\
            "subq.w #1,%1\n\t" \
            "1:\n\t"
#define vdi_Soft_fillSpanMultiplaneOpTrans(type, size, solid, regIndex) \
            "move."#size" %3,%2\n\t" \
            "and."#size" (%0),%2\n\t" \
            "or."#size" d"#regIndex",%2\n\t" \
            "move."#size" %2,(%0)+\n\t"
#define vdi_Soft_fillSpanMultiplaneEndTrans(regNb, solid) \
            "dbra %1,1b\n\t" \
            : "+a"(d), "+d"(wordCount), "=&d"(t) : "d"(patternInverse), vdi_Soft_fillSpanMultiplanePatternRegs##regNb : "cc" \
        ); \
    }

//--------------------------------------------------------------------------------
// Span filling - Multi plane - C.
//--------------------------------------------------------------------------------
#else

//
// Replace.
//

// Color and pattern.
#define vdi_Soft_fillSpanMultiplaneColorReplace(type, regIndex) \
    type color##regIndex = colorToMask##type(&color);
#define vdi_Soft_fillSpanMultiplanePatternReplace(type, regIndex) \
    type pattern##regIndex = color##regIndex & pattern;
// Fringe.
#define vdi_Soft_fillSpanMultiplaneFringeBeginReplace(mask, type, size)
#define vdi_Soft_fillSpanMultiplaneFringeOpReplace(mask, type, size, regIndex) \
    { type dp = *d; *d++ = (((dp ^ pattern##regIndex) & mask) ^ dp); }
#define vdi_Soft_fillSpanMultiplaneFringeEndReplace(mask, regNb)
// Middle.
#define vdi_Soft_fillSpanMultiplaneBeginReplace(type, size, solid) \
    LOOP_DO(wordIndex, wordCount) {
#define vdi_Soft_fillSpanMultiplaneOpReplace(type, size, solid, regIndex) \
        *d++ = pattern##regIndex;
#define vdi_Soft_fillSpanMultiplaneEndReplace(regNb, solid) \
    } LOOP_WHILE(wordIndex);

//
// Xor.
//

// Color and pattern. Ignored for XOR.
#define vdi_Soft_fillSpanMultiplaneColorXor(type, regIndex)
#define vdi_Soft_fillSpanMultiplanePatternXor(type, regIndex)
// Fringe.
#define vdi_Soft_fillSpanMultiplaneFringeBeginXor(mask, type, size) \
    { \
        type patternAndMask = pattern & mask;
#define vdi_Soft_fillSpanMultiplaneFringeOpXor(mask, type, size, regIndex) \
        *d++ ^= patternAndMask;
#define vdi_Soft_fillSpanMultiplaneFringeEndXor(mask, regNb) \
    }
// Middle.
#define vdi_Soft_fillSpanMultiplaneBeginXor(type, size, solid) \
    LOOP_DO(wordIndex, wordCount) {
#define vdi_Soft_fillSpanMultiplaneOpXor(type, size, solid, regIndex) \
        *d++ ^= pattern;
#define vdi_Soft_fillSpanMultiplaneEndXor(regNb, solid) \
    } LOOP_WHILE(wordIndex);

//
// Trans.
//

// Color and pattern.
#define vdi_Soft_fillSpanMultiplaneColorTrans(type, regIndex) \
    type color##regIndex = colorToMask##type(&color);
#define vdi_Soft_fillSpanMultiplanePatternTrans(type, regIndex) \
    type pattern##regIndex = color##regIndex & pattern;
// Fringe.
#define vdi_Soft_fillSpanMultiplaneFringeBeginTrans(mask, type, size) \
    { \
        type patternAndMask = pattern & mask;
#define vdi_Soft_fillSpanMultiplaneFringeOpTrans(mask, type, size, regIndex) \
        { type dp = *d; *d++ = ((dp ^ pattern##regIndex) & patternAndMask) ^ dp; }
#define vdi_Soft_fillSpanMultiplaneFringeEndTrans(mask, regNb) \
    }
// Middle.
#define vdi_Soft_fillSpanMultiplaneBeginTrans(type, size, solid) \
    { \
        type patternInverse = ~pattern; \
        LOOP_DO(wordIndex, wordCount) {
#define vdi_Soft_fillSpanMultiplaneOpTrans(type, size, solid, regIndex) \
            { type dp = *d; *d++ = (dp & patternInverse) | pattern##regIndex; }
#define vdi_Soft_fillSpanMultiplaneEndTrans(regNb, solid) \
        } LOOP_WHILE(wordIndex); \
    }

#endif

//--------------------------------------------------------------------------------
// Span filling - Multi plane - Main.
//--------------------------------------------------------------------------------
#define vdi_Soft_fillSpanMultiplaneLoop1(linePattern, op, patternInverted, patternSolid) \
    { \
        vdi_Soft_fillSpanMultiplaneColor(op, UWORD, 0) \
        LOOP_DO(y, h) { \
            UWORD pattern = patternSolid ? 0xffff : linePattern ^ patternInverted; \
            vdi_Soft_fillSpanMultiplanePattern(op, UWORD, 0) \
            UWORD * RESTRICT d = (UWORD*)dst; \
            vdi_Soft_fillSpanMultiplaneFringeBegin(op, leftMask, UWORD, w) \
            vdi_Soft_fillSpanMultiplaneFringeOp(op, leftMask, UWORD, w, 0) \
            vdi_Soft_fillSpanMultiplaneFringeEnd(op, leftMask, 1) \
            WORD wordCount = wordNb; \
            if (wordCount >= 0) { \
                if (wordCount > 0) { \
                    vdi_Soft_fillSpanMultiplaneBegin(op, UWORD, w, patternSolid) \
                        vdi_Soft_fillSpanMultiplaneOp(op, UWORD, w, patternSolid, 0) \
                    vdi_Soft_fillSpanMultiplaneEnd(op, 1, patternSolid) \
                } \
                vdi_Soft_fillSpanMultiplaneFringeBegin(op, rightMask, UWORD, w) \
                vdi_Soft_fillSpanMultiplaneFringeOp(op, rightMask, UWORD, w, 0) \
                vdi_Soft_fillSpanMultiplaneFringeEnd(op, rightMask, 1) \
            } \
            dst += dstStrideLine; \
        } LOOP_WHILE(y); \
    }
#define vdi_Soft_fillSpanMultiplaneLoop2(linePattern, op, patternInverted, patternSolid) \
    { \
        vdi_Soft_fillSpanMultiplaneColor(op, ULONG, 0) \
        ULONG leftMask32 = broadcastWord(leftMask), rightMask32 = broadcastWord(rightMask); \
        LOOP_DO(y, h) { \
            ULONG pattern = patternSolid ? 0xfffffffful : broadcastWord(linePattern ^ patternInverted); \
            vdi_Soft_fillSpanMultiplanePattern(op, ULONG, 0) \
            ULONG * RESTRICT d = (ULONG*)dst; \
            vdi_Soft_fillSpanMultiplaneFringeBegin(op, leftMask32, ULONG, l) \
            vdi_Soft_fillSpanMultiplaneFringeOp(op, leftMask32, ULONG, l, 0) \
            vdi_Soft_fillSpanMultiplaneFringeEnd(op, leftMask32, 1) \
            WORD wordCount = wordNb; \
            if (wordCount >= 0) { \
                if (wordCount > 0) { \
                    vdi_Soft_fillSpanMultiplaneBegin(op, ULONG, l, patternSolid) \
                        vdi_Soft_fillSpanMultiplaneOp(op, ULONG, l, patternSolid, 0) \
                    vdi_Soft_fillSpanMultiplaneEnd(op, 1, patternSolid) \
                } \
                vdi_Soft_fillSpanMultiplaneFringeBegin(op, rightMask32, ULONG, l) \
                vdi_Soft_fillSpanMultiplaneFringeOp(op, rightMask32, ULONG, l, 0) \
                vdi_Soft_fillSpanMultiplaneFringeEnd(op, rightMask32, 1) \
            } \
            dst += dstStrideLine; \
        } LOOP_WHILE(y); \
    }
#define vdi_Soft_fillSpanMultiplaneLoop4(linePattern, op, patternInverted, patternSolid) \
    { \
        vdi_Soft_fillSpanMultiplaneColor(op, ULONG, 0) \
        vdi_Soft_fillSpanMultiplaneColor(op, ULONG, 1) \
        ULONG leftMask32 = broadcastWord(leftMask), rightMask32 = broadcastWord(rightMask); \
        LOOP_DO(y, h) { \
            ULONG pattern = patternSolid ? 0xfffffffful : broadcastWord(linePattern ^ patternInverted); \
            vdi_Soft_fillSpanMultiplanePattern(op, ULONG, 0); \
            vdi_Soft_fillSpanMultiplanePattern(op, ULONG, 1); \
            ULONG * RESTRICT d = (ULONG*)dst; \
            vdi_Soft_fillSpanMultiplaneFringeBegin(op, leftMask32, ULONG, l) \
            vdi_Soft_fillSpanMultiplaneFringeOp(op, leftMask32, ULONG, l, 0) \
            vdi_Soft_fillSpanMultiplaneFringeOp(op, leftMask32, ULONG, l, 1) \
            vdi_Soft_fillSpanMultiplaneFringeEnd(op, leftMask32, 2) \
            WORD wordCount = wordNb; \
            if (wordCount >= 0) { \
                if (wordCount > 0) { \
                    vdi_Soft_fillSpanMultiplaneBegin(op, ULONG, l, patternSolid) \
                        vdi_Soft_fillSpanMultiplaneOp(op, ULONG, l, patternSolid, 0) \
                        vdi_Soft_fillSpanMultiplaneOp(op, ULONG, l, patternSolid, 1) \
                    vdi_Soft_fillSpanMultiplaneEnd(op, 2, patternSolid); \
                } \
                vdi_Soft_fillSpanMultiplaneFringeBegin(op, rightMask32, ULONG, l) \
                vdi_Soft_fillSpanMultiplaneFringeOp(op, rightMask32, ULONG, l, 0) \
                vdi_Soft_fillSpanMultiplaneFringeOp(op, rightMask32, ULONG, l, 1) \
                vdi_Soft_fillSpanMultiplaneFringeEnd(op, rightMask32, 2) \
            } \
            dst += dstStrideLine; \
        } LOOP_WHILE(y); \
    }
#define vdi_Soft_fillSpanMultiplaneLoop8(linePattern, op, patternInverted, patternSolid) \
    { \
        vdi_Soft_fillSpanMultiplaneColor(op, ULONG, 0) \
        vdi_Soft_fillSpanMultiplaneColor(op, ULONG, 1) \
        vdi_Soft_fillSpanMultiplaneColor(op, ULONG, 2) \
        vdi_Soft_fillSpanMultiplaneColor(op, ULONG, 3) \
        ULONG leftMask32 = broadcastWord(leftMask), rightMask32 = broadcastWord(rightMask); \
        LOOP_DO(y, h) { \
            ULONG pattern = patternSolid ? 0xfffffffful : broadcastWord(linePattern ^ patternInverted); \
            vdi_Soft_fillSpanMultiplanePattern(op, ULONG, 0); \
            vdi_Soft_fillSpanMultiplanePattern(op, ULONG, 1); \
            vdi_Soft_fillSpanMultiplanePattern(op, ULONG, 2); \
            vdi_Soft_fillSpanMultiplanePattern(op, ULONG, 3); \
            ULONG * RESTRICT d = (ULONG*)dst; \
            vdi_Soft_fillSpanMultiplaneFringeBegin(op, leftMask32, ULONG, l) \
            vdi_Soft_fillSpanMultiplaneFringeOp(op, leftMask32, ULONG, l, 0) \
            vdi_Soft_fillSpanMultiplaneFringeOp(op, leftMask32, ULONG, l, 1) \
            vdi_Soft_fillSpanMultiplaneFringeOp(op, leftMask32, ULONG, l, 2) \
            vdi_Soft_fillSpanMultiplaneFringeOp(op, leftMask32, ULONG, l, 3) \
            vdi_Soft_fillSpanMultiplaneFringeEnd(op, leftMask32, 4) \
            WORD wordCount = wordNb; \
            if (wordCount >= 0) { \
                if (wordCount > 0) { \
                    vdi_Soft_fillSpanMultiplaneBegin(op, ULONG, l, patternSolid) \
                        vdi_Soft_fillSpanMultiplaneOp(op, ULONG, l, patternSolid, 0) \
                        vdi_Soft_fillSpanMultiplaneOp(op, ULONG, l, patternSolid, 1) \
                        vdi_Soft_fillSpanMultiplaneOp(op, ULONG, l, patternSolid, 2) \
                        vdi_Soft_fillSpanMultiplaneOp(op, ULONG, l, patternSolid, 3) \
                    vdi_Soft_fillSpanMultiplaneEnd(op, 4, patternSolid) \
                } \
                vdi_Soft_fillSpanMultiplaneFringeBegin(op, rightMask32, ULONG, l) \
                vdi_Soft_fillSpanMultiplaneFringeOp(op, rightMask32, ULONG, l, 0) \
                vdi_Soft_fillSpanMultiplaneFringeOp(op, rightMask32, ULONG, l, 1) \
                vdi_Soft_fillSpanMultiplaneFringeOp(op, rightMask32, ULONG, l, 2) \
                vdi_Soft_fillSpanMultiplaneFringeOp(op, rightMask32, ULONG, l, 3) \
                vdi_Soft_fillSpanMultiplaneFringeEnd(op, rightMask32, 4) \
            } \
            dst += dstStrideLine; \
        } LOOP_WHILE(y); \
        break; \
    }
    
#define vdi_Soft_fillSpanMultiplaneLoop(linePattern, op, patternInverted, patternSolid) \
    switch (planeNb) { \
    default: \
    vdi_Soft_planar1(case 1: vdi_Soft_fillSpanMultiplaneLoop1(linePattern, op, patternInverted, patternSolid) break;) \
    vdi_Soft_planar2(case 2: vdi_Soft_fillSpanMultiplaneLoop2(linePattern, op, patternInverted, patternSolid) break;) \
    vdi_Soft_planar4(case 4: vdi_Soft_fillSpanMultiplaneLoop4(linePattern, op, patternInverted, patternSolid) break;) \
    vdi_Soft_planar8(case 8: vdi_Soft_fillSpanMultiplaneLoop8(linePattern, op, patternInverted, patternSolid) break;) \
    }
    
#define vdi_Soft_fillSpanMultiplaneMode(linePattern) \
    { \
        UBYTE *dst = (UBYTE*)fi->addr; \
        WORD dstStrideLine = fi->stride; \
        WORD wordNb = fi->wordNb - 2; \
        UWORD leftMask = fi->leftMask, rightMask = fi->rightMask; \
        WORD planeNb = fi->planeNb; \
        WORD h = fi->height; \
        switch (mode) { \
        default: \
        case WM_ERASE: \
        case WM_TRANS: \
            { \
                UWORD patternInverted = -(mode == WM_ERASE); \
                vdi_Soft_fillSpanMultiplaneLoop(linePattern, Trans, patternInverted, false); \
            } \
            break; \
        case WM_REPLACE: \
            vdi_Soft_fillSpanMultiplaneLoop(linePattern, Replace, 0x0000, false); \
            break; \
        case WM_XOR: \
            vdi_Soft_fillSpanMultiplaneLoop(linePattern, Xor, 0x0000, false); \
            break; \
        } \
    }

forceinline void vdi_Soft_fillSpanMultiplaneTemplate(const vdi_FillingInfos * RESTRICT fi, const VwkAttrib * RESTRICT attr) {
    WORD mode = attr->wrt_mode;
    UWORD color = attr->color;
    const UWORD *patternData = attr->patptr;
    UWORD patternMask = attr->patmsk;
    WORD patternIndex = fi->y1;
    vdi_Soft_fillSpanMultiplaneMode(patternData[patternIndex++ & patternMask]);
}

#endif

//********************************************************************************
// Rectangle filling.
//********************************************************************************
#if !vdi_blitterOnly

// Versions from smallest to biggest and slowest to fastest.
#define vdi_Soft_fillRectangle_version_smallest 0 // Smallest. But slow because there is no specialization.
#define vdi_Soft_fillRectangle_version_fast 1 // Large. Fast but drawing is only plane by plane.
#define vdi_Soft_fillRectangle_version_fastest 2 // Largest. Fastest, drawing is done with all planes together if possible.

#if defined(TARGET_192)
#define vdi_Soft_fillRectangle_version vdi_Soft_fillRectangle_version_fast
#else
#define vdi_Soft_fillRectangle_version vdi_Soft_fillRectangle_version_fastest
#endif

/*
Available optimization flags:
vdi_Soft_fillRectangle_version_opSpecialized // There are functions specialized for each op.
vdi_Soft_fillRectangle_version_multiplane // All planes are draw simultaneously (2106 bytes with GCC 9.3.1).
*/

#if vdi_Soft_fillRectangle_version == vdi_Soft_fillRectangle_version_smallest
    #define vdi_Soft_fillRectangle_version_opSpecialized 0
    #define vdi_Soft_fillRectangle_version_multiplane 0
#elif vdi_Soft_fillRectangle_version == vdi_Soft_fillRectangle_version_fast
    #define vdi_Soft_fillRectangle_version_opSpecialized 1
    #define vdi_Soft_fillRectangle_version_multiplane 0
#elif vdi_Soft_fillRectangle_version == vdi_Soft_fillRectangle_version_fastest
    #define vdi_Soft_fillRectangle_version_opSpecialized 1
    #define vdi_Soft_fillRectangle_version_multiplane 1
#endif

// - "-ftree-scev-cprop" must be disabled because Gcc generates absurd (large, slow and useless) code.
//    Mainly it generates stupid multiplications to compute the value at the end of a loop. 
//    They are completely useless, and furthermore, it uses a function call.
#define vdi_Soft_fillRectangleOpti __attribute__ ((optimize("Os"), optimize("no-tree-scev-cprop")))

forceinline void vdi_Soft_fillRectangleTemplate(const vdi_FillingInfos * RESTRICT b, const VwkAttrib * RESTRICT attr, WORD mode) {
    UWORD color = attr->color;
    UWORD patternMask = attr->patmsk;
    const UWORD *patternData = attr->patptr;
    UWORD patternPlaneStep = attr->multifill ? 16 : 0;
    WORD patternIndex = b->y1;
    UBYTE *dst = (UBYTE*)b->addr;
    WORD dstStrideLine = b->stride;
    WORD wordNb = b->wordNb;
    UWORD leftMask = b->leftMask, rightMask = b->rightMask;
    WORD planeNb = b->planeNb;
    WORD dstWordStride = planeNb << 1;
    WORD h = b->height;
    LOOP_DO(y, h) {
        const UWORD *patternDataPlane = &patternData[patternIndex++ & patternMask];
        UWORD colorPlane = color;
        UBYTE *dstPlane = dst;
        LOOP_DO(plane, planeNb) {
            UWORD pattern = *patternDataPlane;
            patternDataPlane += patternPlaneStep;
            vdi_Soft_fillSpanSinglePlane(mode, colorPlane, pattern, dstWordStride, dstPlane, leftMask, rightMask, wordNb);
            colorPlane >>= 1;
            dstPlane += 2;
        } LOOP_WHILE(plane);
        dst += dstStrideLine;
    } LOOP_WHILE(y);
}

vdi_Soft_fillRectangleOpti
void vdi_Soft_fillRectangle(const vdi_FillingInfos * RESTRICT b, const VwkAttrib * RESTRICT attr) {
    #if vdi_Soft_fillRectangle_version_multiplane
    if (attr->multifill) {
    #endif
        #if vdi_Soft_fillRectangle_version_opSpecialized
        switch (attr->wrt_mode) {
        default:
        case WM_REPLACE:
            vdi_Soft_fillRectangleTemplate(b, attr, WM_REPLACE);
            break;
        case WM_XOR:
            vdi_Soft_fillRectangleTemplate(b, attr, WM_XOR);
            break;
        case WM_TRANS:
            vdi_Soft_fillRectangleTemplate(b, attr, WM_TRANS);
            break;
        case WM_ERASE:
            vdi_Soft_fillRectangleTemplate(b, attr, WM_ERASE);
            break;
        }
        #else
        vdi_Soft_fillRectangleTemplate(b, attr, attr->wrt_mode);
        #endif
    #if vdi_Soft_fillRectangle_version_multiplane
    } else
        vdi_Soft_fillSpanMultiplaneTemplate(b, attr);
    #endif
}

#endif

//********************************************************************************
// Line tools.
//********************************************************************************
forceinline void vdi_Soft_drawLinePointTemplate(WORD mode, UWORD color, UWORD lineMask, UWORD bit, UWORD *dst) {
    switch (mode) {
    default:
    case WM_REPLACE:
        if (color & lineMask & 1)
            *dst |= bit;
        else
            *dst &= ~bit;
        break;
    case WM_XOR:
        if (lineMask & 1)
            *dst ^= bit;
        break;
    case WM_TRANS:
    case WM_ERASE:
        if (lineMask & 1) {
            if (color & 1)
                *dst |= bit;
            else
                *dst &= ~bit;
        }
        break;
    }
}

//********************************************************************************
// Horizontal line drawing.
//********************************************************************************
#if CONF_WITH_VDI_HORILINE && !vdi_blitterOnly

// Versions from smallest to biggest and slowest to fastest.
#define vdi_Soft_drawHorizontalLine_version_smallest 0 // Smallest. But very slow because there is no specialization at all.
#define vdi_Soft_drawHorizontalLine_version_fast 1 // Bigger and faster than "smallest".
#define vdi_Soft_drawHorizontalLine_version_faster 2 // Bigger and faster than "fast".
#define vdi_Soft_drawHorizontalLine_version_fastest 3 // Largest. Bigger than "faster", fastest for all cases.

#if defined(TARGET_192)
#define vdi_Soft_drawHorizontalLine_version vdi_Soft_drawHorizontalLine_version_smallest
#else
// The "fast" version should be nearly as fast as "fastest" while being half smaller.
#define vdi_Soft_drawHorizontalLine_version vdi_Soft_drawHorizontalLine_version_fastest
#endif

/*
Available optimization flags:
vdi_Soft_drawHorizontalLine_version_multiPlaneSpecialized // Use the multiplane version (1616 bytes with GCC 9.3.1).
vdi_Soft_drawHorizontalLine_version_opSpecialized // There are functions specialized for each op.
vdi_Soft_drawHorizontalLine_version_lineMaskSpecialized // There are functions specialized for lineMask == 0xffff.
*/

#if vdi_Soft_drawHorizontalLine_version == vdi_Soft_drawHorizontalLine_version_smallest
    #define vdi_Soft_drawHorizontalLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawHorizontalLine_version_opSpecialized 0
    #define vdi_Soft_drawHorizontalLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawHorizontalLine_version == vdi_Soft_drawHorizontalLine_version_fast
    #define vdi_Soft_drawHorizontalLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawHorizontalLine_version_opSpecialized 1
    #define vdi_Soft_drawHorizontalLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawHorizontalLine_version == vdi_Soft_drawHorizontalLine_version_faster
    #define vdi_Soft_drawHorizontalLine_version_multiPlaneSpecialized 1
    #define vdi_Soft_drawHorizontalLine_version_opSpecialized 1
    #define vdi_Soft_drawHorizontalLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawHorizontalLine_version == vdi_Soft_drawHorizontalLine_version_fastest
    #define vdi_Soft_drawHorizontalLine_version_multiPlaneSpecialized 1
    #define vdi_Soft_drawHorizontalLine_version_opSpecialized 1
    #define vdi_Soft_drawHorizontalLine_version_lineMaskSpecialized 1
#endif

// - "-ftree-scev-cprop" must be disabled because Gcc generates absurd (large, slow and useless) code.
//    Mainly it generates stupid multiplications to compute the value at the end of a loop. 
//    They are completely useless, and furthermore, it uses a function call.
#define vdi_Soft_drawHorizontalLineOpti __attribute__ ((optimize("Os"), optimize("no-tree-scev-cprop")))

forceinline void vdi_Soft_drawHorizontalLineSinglePlane(const vdi_FillingInfos * RESTRICT fi, WORD mode, UWORD color, UWORD lineMask) {
    UBYTE * RESTRICT dst = (UBYTE *)fi->addr;
    WORD dstWordStride = fi->planeNb << 1;
    LOOP_DO(plane, fi->planeNb) {
        vdi_Soft_fillSpanSinglePlane(mode, color, lineMask, dstWordStride, dst, fi->leftMask, fi->rightMask, fi->wordNb);
        dst += 2;
        color >>= 1;
    } LOOP_WHILE(plane);
}

forceinline void vdi_Soft_drawHorizontalLineTemplate(const vdi_FillingInfos * RESTRICT fi, WORD mode, UWORD color, UWORD lineMask, bool lineMaskSolid, bool multiPlaneSpecialized, bool opSpecialized) {
    if (multiPlaneSpecialized) {
        UBYTE *dst = (UBYTE*)fi->addr;
        WORD dstStrideLine = fi->stride;
        WORD wordNb = fi->wordNb - 2;
        UWORD leftMask = fi->leftMask, rightMask = fi->rightMask;
        WORD planeNb = fi->planeNb;
        WORD h = fi->height;
        if (lineMaskSolid) {
            // Special handling for solid pattern (all ones), the most frequent case, to make things faster.
            switch (mode) {
            case WM_ERASE: // Erase does nothing when the line is solid.
                break;
            case WM_TRANS: // Transparent is the same as replace when the line is solid.
            default:
            case WM_REPLACE:
                vdi_Soft_fillSpanMultiplaneLoop(lineMask, Replace, 0x0000, true);
                break;
            case WM_XOR:
                vdi_Soft_fillSpanMultiplaneLoop(lineMask, Xor, 0x0000, true);
                break;
            }
        } else {
            switch (mode) {
            case WM_ERASE:
                lineMask = ~lineMask;
                // Fallthrough.
            case WM_TRANS:
                if (lineMask == 0x0000) return;
                if (lineMask != 0xffff) {
                    vdi_Soft_fillSpanMultiplaneLoop(lineMask, Trans, 0x0000, false);
                    break;
                }
                // Fallthrough. If the pattern is solid, this is the same as replace, but replace is faster.
            default:
            case WM_REPLACE:
                vdi_Soft_fillSpanMultiplaneLoop(lineMask, Replace, 0x0000, false);
                break;
            case WM_XOR:
                if (lineMask == 0x0000) return;
                vdi_Soft_fillSpanMultiplaneLoop(lineMask, Xor, 0x0000, false);
                break;
            }
        }
    } else if (opSpecialized) {
        switch (mode) {
        case WM_ERASE:
            lineMask = ~lineMask;
            // Fallthrough.
        case WM_TRANS:
            if (lineMask == 0x0000) return;
            if (lineMask != 0xffff) {
                vdi_Soft_drawHorizontalLineSinglePlane(fi, WM_TRANS, color, lineMask);
                break;
            }
            // Fallthrough. If the pattern is solid, this is the same as replace, but replace is faster.
        default:
        case WM_REPLACE:
            vdi_Soft_drawHorizontalLineSinglePlane(fi, WM_REPLACE, color, lineMask);
            break;
        case WM_XOR:
            vdi_Soft_drawHorizontalLineSinglePlane(fi, WM_XOR, color, lineMask);
            break;
        }
    } else
        vdi_Soft_drawHorizontalLineSinglePlane(fi, mode, color, lineMask);
}

vdi_Soft_drawHorizontalLineOpti
UWORD vdi_Soft_drawHorizontalLine(const Line * RESTRICT line, WORD mode, UWORD color, UWORD lineMask, bool lastLineFlag) {
    vdi_FillingInfos fi;
    if (vdi_setupHorizontalLine(line, lastLineFlag, mode, &fi))
        return lineMask;
    
    #if vdi_Soft_drawHorizontalLine_version_lineMaskSpecialized
    if (lineMask == 0xffff)
        vdi_Soft_drawHorizontalLineTemplate(&fi, mode, color, 0xffff, true, vdi_Soft_drawHorizontalLine_version_multiPlaneSpecialized, vdi_Soft_drawHorizontalLine_version_opSpecialized);
    else {
    #else
    {
        if (lineMask != 0x0000 && lineMask != 0xffff)
   #endif
        {
            rolw(lineMask, fi.width & 0xf);
        }
        vdi_Soft_drawHorizontalLineTemplate(&fi, mode, color, lineMask, false, vdi_Soft_drawHorizontalLine_version_multiPlaneSpecialized, vdi_Soft_drawHorizontalLine_version_opSpecialized);
    }
    return lineMask;
}

#endif

//********************************************************************************
// Vertical line drawing - Common.
//********************************************************************************
#if CONF_WITH_VDI_VERTLINE && !vdi_blitterOnly

// TODO: Multiplane draw for speed.

// Versions from smallest to biggest and slowest to fastest.
#define vdi_Soft_drawVerticalLine_version_smallest 0 // Smallest. But very slow because there is no specialization at all.
#define vdi_Soft_drawVerticalLine_version_small 1 // Bigger and faster than "smallest".
#define vdi_Soft_drawVerticalLine_version_fast 2 // Bigger and faster than "small".
#define vdi_Soft_drawVerticalLine_version_faster 3 // Bigger and faster than "fast".
#define vdi_Soft_drawVerticalLine_version_fastest 4 // Largest. Bigger than "faster", fastest for all cases.

// There is only 500 bytes between the smallest and the fastest version, so use the fastest.
#if defined(TARGET_192)
#define vdi_Soft_drawVerticalLine_version vdi_Soft_drawVerticalLine_version_smallest
#else
#define vdi_Soft_drawVerticalLine_version vdi_Soft_drawVerticalLine_version_fastest
#endif

/*
Available optimization flags:
vdi_Soft_drawVerticalLine_version_multiPlaneSpecialized // There are functions specialized for each plane number.
vdi_Soft_drawVerticalLine_version_opSpecialized // There are functions specialized for each op.
vdi_Soft_drawVerticalLine_version_colorSpecialized // There are functions specialized for each color bit value (only used if multiPlaneSpecialized is false).
vdi_Soft_drawVerticalLine_version_lineMaskSpecialized // There are functions specialized for lineMask == 0xffff.
*/

#if vdi_Soft_drawVerticalLine_version == vdi_Soft_drawVerticalLine_version_smallest
    #define vdi_Soft_drawVerticalLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawVerticalLine_version_opSpecialized 0
    #define vdi_Soft_drawVerticalLine_version_colorSpecialized 0
    #define vdi_Soft_drawVerticalLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawVerticalLine_version == vdi_Soft_drawVerticalLine_version_small
    #define vdi_Soft_drawVerticalLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawVerticalLine_version_opSpecialized 1
    #define vdi_Soft_drawVerticalLine_version_colorSpecialized 0
    #define vdi_Soft_drawVerticalLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawVerticalLine_version == vdi_Soft_drawVerticalLine_version_fast
    #define vdi_Soft_drawVerticalLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawVerticalLine_version_opSpecialized 1
    #define vdi_Soft_drawVerticalLine_version_colorSpecialized 1
    #define vdi_Soft_drawVerticalLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawVerticalLine_version == vdi_Soft_drawVerticalLine_version_faster
    #define vdi_Soft_drawVerticalLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawVerticalLine_version_opSpecialized 1
    #define vdi_Soft_drawVerticalLine_version_colorSpecialized 1
    #define vdi_Soft_drawVerticalLine_version_lineMaskSpecialized 1
#elif vdi_Soft_drawVerticalLine_version == vdi_Soft_drawVerticalLine_version_fastest
    #define vdi_Soft_drawVerticalLine_version_multiPlaneSpecialized 1
    #define vdi_Soft_drawVerticalLine_version_opSpecialized 1
    #define vdi_Soft_drawVerticalLine_version_colorSpecialized 1
    #define vdi_Soft_drawVerticalLine_version_lineMaskSpecialized 1
#endif

// - "-ftree-scev-cprop" must be disabled because Gcc generates absurd (large, slow and useless) code.
//    Mainly it generates stupid multiplications to compute the value at the end of a loop. 
//    They are completely useless, and furthermore, it uses a function call.
#define vdi_Soft_drawVerticalLineOpti __attribute__ ((optimize("Os"), optimize("no-tree-scev-cprop")))

#define vdi_Soft_drawVerticalLineAsmEnabled vdi_Soft_asmEnabled

//********************************************************************************
// Vertical line drawing - Single plane.
//********************************************************************************
forceinline UWORD vdi_Soft_drawVerticalLineLoopTemplate(WORD mode, UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h, bool lineMaskSolid) {
    LOOP_DO(y, h) {
        if (!lineMaskSolid)
            rolw1(lineMask);
        vdi_Soft_drawLinePointTemplate(mode, color, lineMask, bitMask, (UWORD*)dst);
        dst += dstStride;
    } LOOP_WHILE(y);
    return lineMask;
}

forceinline UWORD vdi_Soft_drawVerticalLinePlaneTemplate(WORD mode, UWORD color, UWORD lineMask, WORD planeNb, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h, bool lineMaskSolid, bool colorSpecialized) {
    UWORD lineMaskOut;
    LOOP_DO(planeIndex, planeNb) {
        if (colorSpecialized) {
            if (color & 1)
                lineMaskOut = vdi_Soft_drawVerticalLineLoopTemplate(mode, 1, lineMask, bitMask, dst, dstStride, h, lineMaskSolid);
            else
                lineMaskOut = vdi_Soft_drawVerticalLineLoopTemplate(mode, 0, lineMask, bitMask, dst, dstStride, h, lineMaskSolid);
        } else
            lineMaskOut = vdi_Soft_drawVerticalLineLoopTemplate(mode, color, lineMask, bitMask, dst, dstStride, h, lineMaskSolid);
        dst += 2;
        color >>= 1;
    } LOOP_WHILE(planeIndex);
    return lineMaskOut;
}

//********************************************************************************
// Vertical line drawing - Multi plane.
//********************************************************************************
//--------------------------------------------------------------------------------
// Vertical line drawing - Multi plane - Assembly.
//--------------------------------------------------------------------------------
// TODO: Use unrolled loops for solid cases.
#if vdi_Soft_drawVerticalLineAsmEnabled

#define vdi_Soft_drawVerticalLineMultiplaneDuplicate1(action) action(0)
#define vdi_Soft_drawVerticalLineMultiplaneDuplicate2(action) action(0) action(1)
#define vdi_Soft_drawVerticalLineMultiplaneDuplicate4(action) action(0) action(1) action(2) action(3)
#define vdi_Soft_drawVerticalLineMultiplaneDuplicate(action, regNb) vdi_Soft_drawVerticalLineMultiplaneDuplicate##regNb(action)

#define vdi_Soft_drawVerticalLineMultiplanePatternReg(regIndex) register ULONG pattern##regIndex __asm__("d"#regIndex) = colorToMaskULONG(&color) & bitMaskReg;
#define vdi_Soft_drawVerticalLineMultiplanePatternRegs1 "d"(pattern0)
#define vdi_Soft_drawVerticalLineMultiplanePatternRegs2 "d"(pattern0), "d"(pattern1)
#define vdi_Soft_drawVerticalLineMultiplanePatternRegs4 "d"(pattern0), "d"(pattern1), "d"(pattern2), "d"(pattern3)

//
// 1 plane.
//

forceinline UWORD vdi_Soft_drawVerticalLineMultiplane1Replace0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    UWORD bitMaskReg = bitMask, bitMaskInverse = ~bitMaskReg;
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "tst.w %[color]\n\t"
        "beq.s 3f\n\t"
        "1:\n\t"
        "ror.w #1,%[lineMask]\n\t"
        "bcc.s 2f\n\t"
        "or.w %[bitMaskReg],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        "bra.s 4f\n\t"
        "2:\n\t"
        "and.w %[bitMaskInverse],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        "bra.s 4f\n\t"
        "3:\n\t"
        "and.w %[bitMaskInverse],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],3b\n\t"
        "4:\n\t"
        : [dst]"+a"(dst), [h]"+d"(h), [lineMask]"+d"(lineMask) : [dstStride]"r"(dstStride), [bitMaskReg]"d"(bitMaskReg), [bitMaskInverse]"d"(bitMaskInverse), [color]"d"(color) : "cc"
    );
    return lineMask;
}
// Replace and transparent are identical when solid.
forceinline UWORD vdi_Soft_drawVerticalLineMultiplane1Replace1(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    (void)lineMask;
    UWORD bitMaskReg = bitMask;
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "tst.w %[color]\n\t"
        "beq.s 2f\n\t"
        "1:\n\t"
        "or.w %[bitMaskReg],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        "bra.s 3f\n\t"
        "2:\n\t"
        "not.w %[bitMaskReg]\n\t"
        "3:\n\t"
        "and.w %[bitMaskReg],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],3b\n\t"
        "3:\n\t"
        : [dst]"+a"(dst), [h]"+d"(h), [bitMaskReg]"+d"(bitMaskReg) : [dstStride]"r"(dstStride), [color]"d"(color) : "cc"
    );
    return lineMask;
}
forceinline UWORD vdi_Soft_drawVerticalLineMultiplane1Xor0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    UWORD bitMaskReg = bitMask;
    __asm__ volatile ( \
        "subq.w #1,%[h]\n\t"
        "1:\n\t"
        "ror.w #1,%[lineMask]\n\t"
        "bcc.s 2f\n\t"
        "eor.w %[bitMaskReg],(%[dst])\n\t"
        "2:\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        : [dst]"+a"(dst), [h]"+d"(h), [lineMask]"+d"(lineMask) : [dstStride]"r"(dstStride), [bitMaskReg]"d"(bitMaskReg) : "cc"
    );
    return lineMask;
}
forceinline UWORD vdi_Soft_drawVerticalLineMultiplane1Xor1(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    (void)lineMask;
    UWORD bitMaskReg = bitMask;
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "1:\n\t"
        "eor.w %[bitMaskReg],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        : [dst]"+a"(dst), [h]"+d"(h) : [dstStride]"r"(dstStride), [bitMaskReg]"d"(bitMaskReg) : "cc"
    );
    return lineMask;
}
forceinline UWORD vdi_Soft_drawVerticalLineMultiplane1Trans0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    UWORD bitMaskReg = bitMask;
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "tst.w %[color]\n\t"
        "beq.s 3f\n\t"
        "1:\n\t"
        "ror.w #1,%[lineMask]\n\t"
        "bcc.s 2f\n\t"
        "or.w %[bitMaskReg],(%[dst])\n\t"
        "2:\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        "bra.s 6f\n\t"
        "3:\n\t"
        "not.w %[bitMaskReg]\n\t"
        "4:\n\t"
        "ror.w #1,%[lineMask]\n\t"
        "bcc.s 5f\n\t"
        "and.w %[bitMaskReg],(%[dst])\n\t"
        "5:\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],4b\n\t"
        "6:\n\t"
        : [dst]"+a"(dst), [h]"+d"(h), [lineMask]"+d"(lineMask), [bitMaskReg]"+d"(bitMaskReg) : [dstStride]"r"(dstStride), [color]"d"(color) : "cc"
    );
    return lineMask;
}

//
// 2 planes.
//

forceinline UWORD vdi_Soft_drawVerticalLineMultiplane2Replace0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    ULONG bitMaskReg = broadcastWord(bitMask), bitMaskInverse = ~bitMaskReg, t;
    vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplanePatternReg, 1)
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "1:\n\t"
        "ror.w #1,%[lineMask]\n\t"
        "bcc.s 2f\n\t"
        "mov.l %[bitMaskInverse],%[t]\n\t"
        "and.l (%[dst]),%[t]\n\t"
        "or.l d0,%[t]\n\t"
        "mov.l %[t],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        "bra.s 3f\n\t"
        "2:\n\t"
        "and.l %[bitMaskInverse],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        "3:\n\t"
        : [dst]"+a"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h), [lineMask]"+d"(lineMask), [t]"=&d"(t) : [bitMaskInverse]"d"(bitMaskInverse), vdi_Soft_drawVerticalLineMultiplanePatternRegs1 : "cc"
    );
    return lineMask;
}
// Replace and transparent are identical when solid.
forceinline UWORD vdi_Soft_drawVerticalLineMultiplane2Replace1(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    (void)lineMask;
    ULONG bitMaskReg = broadcastWord(bitMask), bitMaskInverse = ~bitMaskReg, t;
    vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplanePatternReg, 1)
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "1:\n\t"
        "mov.l %[bitMaskInverse],%[t]\n\t"
        "and.l (%[dst]),%[t]\n\t"
        "or.l d0,%[t]\n\t"
        "mov.l %[t],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        : [dst]"+a"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h), [t]"=&d"(t) : [bitMaskInverse]"d"(bitMaskInverse), vdi_Soft_drawVerticalLineMultiplanePatternRegs1 : "cc"
    );
    return lineMask;
}

forceinline UWORD vdi_Soft_drawVerticalLineMultiplane2Xor0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    ULONG bitMaskReg = broadcastWord(bitMask);
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "1:\n\t"
        "ror.w #1,%[lineMask]\n\t"
        "bcc.s 2f\n\t"
        "eor.l %[bitMaskReg],(%[dst])\n\t"
        "2:\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        : [dst]"+a"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h), [lineMask]"+d"(lineMask) : [bitMaskReg]"d"(bitMaskReg) : "cc"
    );
    return lineMask;
}
forceinline UWORD vdi_Soft_drawVerticalLineMultiplane2Xor1(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    ULONG bitMaskReg = broadcastWord(bitMask);
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "1:\n\t"
        "eor.l %[bitMaskReg],(%[dst])\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        : [dst]"+a"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h) : [bitMaskReg]"d"(bitMaskReg) : "cc"
    );
    return lineMask;
}

forceinline UWORD vdi_Soft_drawVerticalLineMultiplane2Trans0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) {
    ULONG bitMaskReg = broadcastWord(bitMask), bitMaskInverse = ~bitMaskReg, t;
    vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplanePatternReg, 1)
    __asm__ volatile (
        "subq.w #1,%[h]\n\t"
        "1:\n\t"
        "ror.w #1,%[lineMask]\n\t"
        "bcc.s 2f\n\t"
        "mov.l %[bitMaskInverse],%[t]\n\t"
        "and.l (%[dst]),%[t]\n\t"
        "or.l d0,%[t]\n\t"
        "mov.l %[t],(%[dst])\n\t"
        "2:\n\t"
        "add.w %[dstStride],%[dst]\n\t"
        "dbra %[h],1b\n\t"
        : [dst]"+a"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h), [lineMask]"+d"(lineMask), [t]"=&d"(t) : [bitMaskInverse]"d"(bitMaskInverse), vdi_Soft_drawVerticalLineMultiplanePatternRegs1 : "cc"
    );
    return lineMask;
}
    
//
// More than 2 planes.
//

#define vdi_Soft_drawVerticalLineMultiplaneNReplaceSet(regIndex) \
            "mov.l %[bitMaskInverse],%[t]\n\t" \
            "and.l (%[dst]),%[t]\n\t" \
            "or.l d"#regIndex",%[t]\n\t" \
            "mov.l %[t],(%[dst])+\n\t"
#define vdi_Soft_drawVerticalLineMultiplaneNReplaceClear(regIndex) \
            "and.l %[bitMaskInverse],(%[dst])+\n\t"
#define vdi_Soft_drawVerticalLineMultiplaneNReplace0Template(planeNb, regNb) \
    forceinline UWORD vdi_Soft_drawVerticalLineMultiplane##planeNb##Replace0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) { \
        ULONG bitMaskReg = broadcastWord(bitMask), bitMaskInverse = ~bitMaskReg, t; \
        vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplanePatternReg, regNb) \
        dstStride -= regNb << 2; \
        __asm__ volatile ( \
            "subq.w #1,%[h]\n\t" \
            "1:\n\t" \
            "ror.w #1,%[lineMask]\n\t" \
            "bcc.s 2f\n\t" \
            vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplaneNReplaceSet, regNb) \
            "add.w %[dstStride],%[dst]\n\t" \
            "dbra %[h],1b\n\t" \
            "bra.s 3f\n\t" \
            "2:\n\t" \
            vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplaneNReplaceClear, regNb) \
            "add.w %[dstStride],%[dst]\n\t" \
            "dbra %[h],1b\n\t" \
            "3:\n\t" \
            : [dst]"+a"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h), [lineMask]"+d"(lineMask), [t]"=&d"(t) : [bitMaskInverse]"d"(bitMaskInverse), vdi_Soft_drawVerticalLineMultiplanePatternRegs##regNb : "cc" \
        ); \
        return lineMask; \
    }
vdi_Soft_drawVerticalLineMultiplaneNReplace0Template(4, 2)
vdi_Soft_drawVerticalLineMultiplaneNReplace0Template(8, 4)
// Replace and transparent are identical when solid.
#define vdi_Soft_drawVerticalLineMultiplaneNReplace1Template(planeNb, regNb) \
    forceinline UWORD vdi_Soft_drawVerticalLineMultiplane##planeNb##Replace1(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) { \
        (void)lineMask; \
        ULONG bitMaskReg = broadcastWord(bitMask), bitMaskInverse = ~bitMaskReg, t; \
        vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplanePatternReg, regNb) \
        dstStride -= regNb << 2; \
        __asm__ volatile ( \
            "subq.w #1,%[h]\n\t" \
            "1:\n\t" \
            vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplaneNReplaceSet, regNb) \
            "add.w %[dstStride],%[dst]\n\t" \
            "dbra %[h],1b\n\t" \
            : [dst]"+a"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h), [t]"=&d"(t) : [bitMaskInverse]"d"(bitMaskInverse), vdi_Soft_drawVerticalLineMultiplanePatternRegs##regNb : "cc" \
        ); \
        return lineMask; \
    }
vdi_Soft_drawVerticalLineMultiplaneNReplace1Template(4, 2)
vdi_Soft_drawVerticalLineMultiplaneNReplace1Template(8, 4)

#define vdi_Soft_drawVerticalLineMultiplaneNXorOp0(regIndex) \
            "eor.l %[bitMaskReg],(%[d])+\n\t"
#define vdi_Soft_drawVerticalLineMultiplaneNXor0Template(planeNb, regNb) \
    forceinline UWORD vdi_Soft_drawVerticalLineMultiplane##planeNb##Xor0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) { \
        ULONG bitMaskReg = broadcastWord(bitMask); \
        dstStride -= regNb << 2; \
        UBYTE *d; \
        __asm__ volatile ( \
            "subq.w #1,%[h]\n\t" \
            "1:\n\t" \
            "ror.w #1,%[lineMask]\n\t" \
            "bcc.s 2f\n\t" \
            "move.l %[dst],%[d]\n\t" \
            vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplaneNXorOp0, regNb) \
            "2:\n\t" \
            "add.w %[dstStride],%[dst]\n\t" \
            "dbra %[h],1b\n\t" \
            : [dst]"+r"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h), [lineMask]"+d"(lineMask), [d]"=&a"(d) : [bitMaskReg]"d"(bitMaskReg) : "cc" \
        ); \
        return lineMask; \
    }
vdi_Soft_drawVerticalLineMultiplaneNXor0Template(4, 2)
vdi_Soft_drawVerticalLineMultiplaneNXor0Template(8, 4)
#define vdi_Soft_drawVerticalLineMultiplaneNXorOp1(regIndex) \
            "eor.l %[bitMaskReg],(%[dst])+\n\t"
#define vdi_Soft_drawVerticalLineMultiplaneNXor1Template(planeNb, regNb) \
    forceinline UWORD vdi_Soft_drawVerticalLineMultiplane##planeNb##Xor1(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) { \
        (void)lineMask; \
        ULONG bitMaskReg = broadcastWord(bitMask); \
        dstStride -= regNb << 2; \
        __asm__ volatile ( \
            "subq.w #1,%[h]\n\t" \
            "1:\n\t" \
            vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplaneNXorOp1, regNb) \
            "add.w %[dstStride],%[dst]\n\t" \
            "dbra %[h],1b\n\t" \
            : [dst]"+a"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h) : [bitMaskReg]"d"(bitMaskReg) : "cc" \
        ); \
        return lineMask; \
    }
vdi_Soft_drawVerticalLineMultiplaneNXor1Template(4, 2)
vdi_Soft_drawVerticalLineMultiplaneNXor1Template(8, 4)

#define vdi_Soft_drawVerticalLineMultiplaneNTransOp(regIndex) \
            "mov.l %[bitMaskInverse],%[t]\n\t" \
            "and.l (%[d]),%[t]\n\t" \
            "or.l d"#regIndex",%[t]\n\t" \
            "mov.l %[t],(%[d])+\n\t"
#define vdi_Soft_drawVerticalLineMultiplaneNTrans0Template(planeNb, regNb) \
    forceinline UWORD vdi_Soft_drawVerticalLineMultiplane##planeNb##Trans0(UWORD color, UWORD lineMask, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h) { \
        ULONG bitMaskReg = broadcastWord(bitMask), bitMaskInverse = ~bitMaskReg, t; \
        vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplanePatternReg, regNb) \
        UBYTE *d; \
        __asm__ volatile ( \
            "subq.w #1,%[h]\n\t" \
            "1:\n\t" \
            "ror.w #1,%[lineMask]\n\t" \
            "bcc.s 2f\n\t" \
            "move.l %[dst],%[d]\n\t" \
            vdi_Soft_drawVerticalLineMultiplaneDuplicate(vdi_Soft_drawVerticalLineMultiplaneNTransOp, regNb) \
            "2:\n\t" \
            "add.w %[dstStride],%[dst]\n\t" \
            "dbra %[h],1b\n\t" \
            : [dst]"+r"(dst), [dstStride]"+r"(dstStride), [h]"+d"(h), [lineMask]"+d"(lineMask), [d]"=&a"(d), [t]"=&d"(t) : [bitMaskInverse]"d"(bitMaskInverse), vdi_Soft_drawVerticalLineMultiplanePatternRegs##regNb : "cc" \
        ); \
        return lineMask; \
    }
vdi_Soft_drawVerticalLineMultiplaneNTrans0Template(4, 2)
vdi_Soft_drawVerticalLineMultiplaneNTrans0Template(8, 4)
   
#define vdi_Soft_drawVerticalLineMultiplaneTemplate1(op, solid) \
    lineMask = vdi_Soft_drawVerticalLineMultiplane1##op##solid(color, lineMask, bitMask, dst, dstStride, h);
#define vdi_Soft_drawVerticalLineMultiplaneTemplate2(op, solid) \
    lineMask = vdi_Soft_drawVerticalLineMultiplane2##op##solid(color, lineMask, bitMask, dst, dstStride, h);
#define vdi_Soft_drawVerticalLineMultiplaneTemplate4(op, solid) \
    lineMask = vdi_Soft_drawVerticalLineMultiplane4##op##solid(color, lineMask, bitMask, dst, dstStride, h);
#define vdi_Soft_drawVerticalLineMultiplaneTemplate8(op, solid) \
    lineMask = vdi_Soft_drawVerticalLineMultiplane8##op##solid(color, lineMask, bitMask, dst, dstStride, h);

//--------------------------------------------------------------------------------
// Vertical line drawing - Multi plane - C.
//--------------------------------------------------------------------------------
#else

//
// Replace.
//

#define vdi_Soft_drawVerticalLineMultiplanePatternReplace(type, regIndex) \
    type pattern##regIndex = colorToMask##type(&color) & bitMaskReg;
#define vdi_Soft_drawVerticalLineMultiplaneBeginReplace(type, size, solid) \
    { \
        dstStride -= planeNb << 1; \
        type bitMaskInverse = ~bitMaskReg; \
        LOOP_DO(y, h) { \
            type * RESTRICT d = (type*)dst;
#define vdi_Soft_drawVerticalLineMultiplaneOpSetReplace(type, size, solid, regIndex) \
            { type dp = *d; *d++ = (dp & bitMaskInverse) | pattern##regIndex; }
#define vdi_Soft_drawVerticalLineMultiplaneOpClearReplace(type, size, solid, regIndex) \
            *d++ &= bitMaskInverse;
#define vdi_Soft_drawVerticalLineMultiplaneEndReplace(regNb, solid) \
            if (!solid) \
                rolw1(lineMask); \
            dst = (UBYTE*)d + dstStride; \
        } LOOP_WHILE(y); \
    }

//
// Xor.
//
    
#define vdi_Soft_drawVerticalLineMultiplanePatternXor(type, regIndex)
#define vdi_Soft_drawVerticalLineMultiplaneBeginXor(type, size, solid) \
    { \
        if (solid) \
            dstStride -= planeNb << 1; \
        LOOP_DO(y, h) { \
            type * RESTRICT d = (type*)dst;
#define vdi_Soft_drawVerticalLineMultiplaneOpSetXor(type, size, solid, regIndex) \
            *d++ ^= bitMaskReg;
#define vdi_Soft_drawVerticalLineMultiplaneOpClearXor(type, size, solid, regIndex)
#define vdi_Soft_drawVerticalLineMultiplaneEndXor(regNb, solid) \
            if (!solid) \
                rolw1(lineMask); \
            dst = solid ? (UBYTE*)d + dstStride : dst + dstStride ; \
        } LOOP_WHILE(y); \
    }

//
// Trans.
//

#define vdi_Soft_drawVerticalLineMultiplanePatternTrans(type, regIndex) \
    type pattern##regIndex = colorToMask##type(&color) & bitMaskReg;
#define vdi_Soft_drawVerticalLineMultiplaneBeginTrans(type, size, solid) \
    { \
        if (solid) \
            dstStride -= planeNb << 1; \
        type bitMaskInverse = ~bitMaskReg; \
        LOOP_DO(y, h) { \
            type * RESTRICT d = (type*)dst;
#define vdi_Soft_drawVerticalLineMultiplaneOpSetTrans(type, size, solid, regIndex) \
            { type dp = *d; *d++ = (dp & bitMaskInverse) | pattern##regIndex; }
#define vdi_Soft_drawVerticalLineMultiplaneOpClearTrans(type, size, solid, regIndex)
#define vdi_Soft_drawVerticalLineMultiplaneEndTrans(regNb, solid) \
            if (!solid) \
                rolw1(lineMask); \
            dst = solid ? (UBYTE*)d + dstStride : dst + dstStride ; \
        } LOOP_WHILE(y); \
    }

#define vdi_Soft_drawVerticalLineMultiplanePattern(op, type, regIndex) \
    vdi_Soft_drawVerticalLineMultiplanePattern##op(type, regIndex)

#define vdi_Soft_drawVerticalLineMultiplaneBegin(op, type, size, solid) \
    vdi_Soft_drawVerticalLineMultiplaneBegin##op(type, size, solid)
#define vdi_Soft_drawVerticalLineMultiplaneOpSet(op, type, size, solid, regIndex) \
    vdi_Soft_drawVerticalLineMultiplaneOpSet##op(type, size, solid, regIndex)
#define vdi_Soft_drawVerticalLineMultiplaneOpClear(op, type, size, solid, regIndex) \
    vdi_Soft_drawVerticalLineMultiplaneOpClear##op(type, size, solid, regIndex)
#define vdi_Soft_drawVerticalLineMultiplaneEnd(op, regNb, solid) \
    vdi_Soft_drawVerticalLineMultiplaneEnd##op(regNb, solid)
    
#define vdi_Soft_drawVerticalLineMultiplaneTemplate1(op, solid) \
    { \
        UWORD bitMaskReg = bitMask; \
        vdi_Soft_drawVerticalLineMultiplanePattern(op, UWORD, 0) \
        vdi_Soft_drawVerticalLineMultiplaneBegin(op, UWORD, w, solid) \
            if (solid || (lineMask & 1)) { \
                vdi_Soft_drawVerticalLineMultiplaneOpSet(op, UWORD, w, solid, 0) \
            } else { \
                vdi_Soft_drawVerticalLineMultiplaneOpClear(op, UWORD, w, solid, 0) \
            } \
        vdi_Soft_drawVerticalLineMultiplaneEnd(op, 1, solid) \
    }
#define vdi_Soft_drawVerticalLineMultiplaneTemplate2(op, solid) \
    { \
        ULONG bitMaskReg = broadcastWord(bitMask); \
        vdi_Soft_drawVerticalLineMultiplanePattern(op, ULONG, 0) \
        vdi_Soft_drawVerticalLineMultiplaneBegin(op, ULONG, l, solid) \
            if (solid || (lineMask & 1)) { \
                vdi_Soft_drawVerticalLineMultiplaneOpSet(op, ULONG, l, solid, 0) \
            } else { \
                vdi_Soft_drawVerticalLineMultiplaneOpClear(op, ULONG, l, solid, 0) \
            } \
        vdi_Soft_drawVerticalLineMultiplaneEnd(op, 1, solid) \
    }
#define vdi_Soft_drawVerticalLineMultiplaneTemplate4(op, solid) \
    { \
        ULONG bitMaskReg = broadcastWord(bitMask); \
        vdi_Soft_drawVerticalLineMultiplanePattern(op, ULONG, 0) \
        vdi_Soft_drawVerticalLineMultiplanePattern(op, ULONG, 1) \
        vdi_Soft_drawVerticalLineMultiplaneBegin(op, ULONG, l, solid) \
            if (solid || (lineMask & 1)) { \
                vdi_Soft_drawVerticalLineMultiplaneOpSet(op, ULONG, l, solid, 0) \
                vdi_Soft_drawVerticalLineMultiplaneOpSet(op, ULONG, l, solid, 1) \
            } else { \
                vdi_Soft_drawVerticalLineMultiplaneOpClear(op, ULONG, l, solid, 0) \
                vdi_Soft_drawVerticalLineMultiplaneOpClear(op, ULONG, l, solid, 1) \
            } \
        vdi_Soft_drawVerticalLineMultiplaneEnd(op, 2, solid) \
    }
#define vdi_Soft_drawVerticalLineMultiplaneTemplate8(op, solid) \
    { \
        ULONG bitMaskReg = broadcastWord(bitMask); \
        vdi_Soft_drawVerticalLineMultiplanePattern(op, ULONG, 0) \
        vdi_Soft_drawVerticalLineMultiplanePattern(op, ULONG, 1) \
        vdi_Soft_drawVerticalLineMultiplanePattern(op, ULONG, 2) \
        vdi_Soft_drawVerticalLineMultiplanePattern(op, ULONG, 3) \
        vdi_Soft_drawVerticalLineMultiplaneBegin(op, ULONG, l, solid) \
            if (solid || (lineMask & 1)) { \
                vdi_Soft_drawVerticalLineMultiplaneOpSet(op, ULONG, l, solid, 0) \
                vdi_Soft_drawVerticalLineMultiplaneOpSet(op, ULONG, l, solid, 1) \
                vdi_Soft_drawVerticalLineMultiplaneOpSet(op, ULONG, l, solid, 2) \
                vdi_Soft_drawVerticalLineMultiplaneOpSet(op, ULONG, l, solid, 3) \
            } else { \
                vdi_Soft_drawVerticalLineMultiplaneOpClear(op, ULONG, l, solid, 0) \
                vdi_Soft_drawVerticalLineMultiplaneOpClear(op, ULONG, l, solid, 1) \
                vdi_Soft_drawVerticalLineMultiplaneOpClear(op, ULONG, l, solid, 2) \
                vdi_Soft_drawVerticalLineMultiplaneOpClear(op, ULONG, l, solid, 3) \
            } \
        vdi_Soft_drawVerticalLineMultiplaneEnd(op, 4, solid) \
    }

#endif

//--------------------------------------------------------------------------------
// Vertical line drawing - Multi plane - Main.
//--------------------------------------------------------------------------------  
#define vdi_Soft_drawVerticalLineMultiplaneTemplate(op, solid) \
    switch (planeNb) { \
    default: \
    vdi_Soft_planar1(case 1: vdi_Soft_drawVerticalLineMultiplaneTemplate1(op, solid) break;) \
    vdi_Soft_planar2(case 2: vdi_Soft_drawVerticalLineMultiplaneTemplate2(op, solid) break;) \
    vdi_Soft_planar4(case 4: vdi_Soft_drawVerticalLineMultiplaneTemplate4(op, solid) break;) \
    vdi_Soft_planar8(case 8: vdi_Soft_drawVerticalLineMultiplaneTemplate8(op, solid) break;) \
    }

//********************************************************************************
// Vertical line drawing - Main.
//********************************************************************************
forceinline UWORD vdi_Soft_drawVerticalLineModeTemplate(WORD mode, UWORD color, UWORD lineMask, WORD planeNb, UWORD bitMask, UBYTE * RESTRICT dst, WORD dstStride, WORD h, bool lineMaskSolid, bool colorSpecialized, bool multiPlaneSpecialized, bool opSpecialized) {
    if (multiPlaneSpecialized) {
        if (lineMaskSolid) {
            switch (mode) {
            case WM_ERASE: // Erase does nothing when the line is solid.
                break;
            case WM_TRANS: // Transparent is the same as replace when the line is solid.
            default:
            case WM_REPLACE:
                vdi_Soft_drawVerticalLineMultiplaneTemplate(Replace, true);
                break;
            case WM_XOR:
                vdi_Soft_drawVerticalLineMultiplaneTemplate(Xor, true);
                break;
            }
        } else {
            switch (mode) {
            case WM_ERASE:
                lineMask = ~lineMask;
                // Fallthrough.
            case WM_TRANS:
                if (lineMask != 0x0000)
                    vdi_Soft_drawVerticalLineMultiplaneTemplate(Trans, false);
                if (mode == WM_ERASE)
                    lineMask = ~lineMask;
                break;
            default:
            case WM_REPLACE:
                vdi_Soft_drawVerticalLineMultiplaneTemplate(Replace, false);
                break;
            case WM_XOR:
                if (lineMask != 0x0000)
                    vdi_Soft_drawVerticalLineMultiplaneTemplate(Xor, false);
                break;
            }
        }
    } else if (opSpecialized) {
        if (lineMaskSolid) {
            switch (mode) {
            case WM_ERASE: // Erase does nothing when the line is solid.
                break;
            case WM_TRANS: // Transparent is the same as replace when the line is solid.
            default:
            case WM_REPLACE:
                lineMask = vdi_Soft_drawVerticalLinePlaneTemplate(WM_REPLACE, color, lineMask, planeNb, bitMask, dst, dstStride, h, lineMaskSolid, colorSpecialized);
                break;
            case WM_XOR:
                lineMask = vdi_Soft_drawVerticalLinePlaneTemplate(WM_XOR, color, lineMask, planeNb, bitMask, dst, dstStride, h, lineMaskSolid, colorSpecialized);
                break;
            }
        } else {
            switch (mode) {
            case WM_ERASE:
                lineMask = ~lineMask;
                // Fallthrough.
            case WM_TRANS:
                if (lineMask != 0x0000)
                    lineMask = vdi_Soft_drawVerticalLinePlaneTemplate(WM_TRANS, color, lineMask, planeNb, bitMask, dst, dstStride, h, lineMaskSolid, colorSpecialized);
                if (mode == WM_ERASE)
                    lineMask = ~lineMask;
                break;
            default:
            case WM_REPLACE:
                lineMask = vdi_Soft_drawVerticalLinePlaneTemplate(WM_REPLACE, color, lineMask, planeNb, bitMask, dst, dstStride, h, lineMaskSolid, colorSpecialized);
                break;
            case WM_XOR:
                if (lineMask != 0x0000)
                    lineMask = vdi_Soft_drawVerticalLinePlaneTemplate(WM_XOR, color, lineMask, planeNb, bitMask, dst, dstStride, h, lineMaskSolid, colorSpecialized);
                break;
            }
        }
    } else {
        if (mode == WM_ERASE)
            lineMask = ~lineMask;
        lineMask = vdi_Soft_drawVerticalLinePlaneTemplate(mode, color, lineMask, planeNb, bitMask, dst, dstStride, h, lineMaskSolid, colorSpecialized);
        if (mode == WM_ERASE)
            lineMask = ~lineMask;
    }
    return lineMask;
}

vdi_Soft_drawVerticalLineOpti
UWORD vdi_Soft_drawVerticalLine(const Line * RESTRICT line, WORD mode, UWORD color, UWORD lineMask, bool lastLineFlag) {
    vdi_FillingInfos fi;
    if (vdi_setupVerticalLine(line, lastLineFlag, mode, &fi))
        return lineMask;
    UWORD shift = line->x1 & 0xf;
    UWORD bitMask = 0x8000 >> shift;
    if (vdi_Soft_drawVerticalLine_version_lineMaskSpecialized && lineMask == 0xffff)
        vdi_Soft_drawVerticalLineModeTemplate(mode, color, 0xffff, fi.planeNb, bitMask, fi.addr, fi.stride, fi.height, true, vdi_Soft_drawVerticalLine_version_colorSpecialized, vdi_Soft_drawVerticalLine_version_multiPlaneSpecialized, vdi_Soft_drawVerticalLine_version_opSpecialized);
    else
        lineMask = vdi_Soft_drawVerticalLineModeTemplate(mode, color, lineMask, fi.planeNb, bitMask, fi.addr, fi.stride, fi.height, false, vdi_Soft_drawVerticalLine_version_colorSpecialized, vdi_Soft_drawVerticalLine_version_multiPlaneSpecialized, vdi_Soft_drawVerticalLine_version_opSpecialized);
    return lineMask;
}

#endif

//********************************************************************************
// General line drawing - Common.
//********************************************************************************
// TODO: Multiplane draw for speed.

// Versions from smallest to biggest and slowest to fastest.
#define vdi_Soft_drawGeneralLine_version_smallest 0 // Smallest. But very slow because there is no specialization at all.
#define vdi_Soft_drawGeneralLine_version_small 1 // Bigger and faster than "smallest".
#define vdi_Soft_drawGeneralLine_version_fast 2 // Bigger and faster than "small".
#define vdi_Soft_drawGeneralLine_version_faster 3 // Bigger and faster than "fast".
#define vdi_Soft_drawGeneralLine_version_fastest 4 // Largest. Bigger than "faster", fastest for all cases.

#if defined(TARGET_192)
#define vdi_Soft_drawGeneralLine_version vdi_Soft_drawGeneralLine_version_small
#else
#define vdi_Soft_drawGeneralLine_version vdi_Soft_drawGeneralLine_version_fastest
#endif

/*
Available optimization flags:
vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized // There are functions specialized for each plane number.
vdi_Soft_drawGeneralLine_version_opSpecialized // There are functions specialized for each op.
vdi_Soft_drawGeneralLine_version_colorSpecialized // There are functions specialized for each color bit value.
vdi_Soft_drawGeneralLine_version_lineMaskSpecialized // There are functions specialized for lineMask == 0xffff.
*/

#if vdi_Soft_drawGeneralLine_version == vdi_Soft_drawGeneralLine_version_smallest
    #define vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawGeneralLine_version_opSpecialized 0
    #define vdi_Soft_drawGeneralLine_version_colorSpecialized 0
    #define vdi_Soft_drawGeneralLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawGeneralLine_version == vdi_Soft_drawGeneralLine_version_small
    #define vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawGeneralLine_version_opSpecialized 1
    #define vdi_Soft_drawGeneralLine_version_colorSpecialized 0
    #define vdi_Soft_drawGeneralLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawGeneralLine_version == vdi_Soft_drawGeneralLine_version_fast
    #define vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawGeneralLine_version_opSpecialized 1
    #define vdi_Soft_drawGeneralLine_version_colorSpecialized 1
    #define vdi_Soft_drawGeneralLine_version_lineMaskSpecialized 0
#elif vdi_Soft_drawGeneralLine_version == vdi_Soft_drawGeneralLine_version_faster
    #define vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawGeneralLine_version_opSpecialized 1
    #define vdi_Soft_drawGeneralLine_version_colorSpecialized 1
    #define vdi_Soft_drawGeneralLine_version_lineMaskSpecialized 1
#elif vdi_Soft_drawGeneralLine_version == vdi_Soft_drawGeneralLine_version_fastest
//    #define vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized 1
    #define vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized 0
    #define vdi_Soft_drawGeneralLine_version_opSpecialized 1
    #define vdi_Soft_drawGeneralLine_version_colorSpecialized 1
    #define vdi_Soft_drawGeneralLine_version_lineMaskSpecialized 1
#endif

// - "-ftree-scev-cprop" must be disabled because Gcc generates absurd (large, slow and useless) code.
//    Mainly it generates stupid multiplications to compute the value at the end of a loop. 
//    They are completely useless, and furthermore, it uses a function call.
#define vdi_Soft_drawGeneralLineOpti __attribute__ ((optimize("Os"), optimize("no-tree-scev-cprop")))

#define vdi_Soft_drawGeneralLineAsmEnabled vdi_Soft_asmEnabled

//********************************************************************************
// General line drawing - Single plane.
//********************************************************************************
#if vdi_Soft_asmEnabled
#define vdi_Soft_drawGeneralLineRotate(bit, dst, dstStrideX) \
    __asm__ volatile ( \
        "ror.w #1,%[bit]\n\t" \
        "bcc 1f\n\t" \
        "add.w %[dstStrideX],%[dst]\n\t" \
        "1:\n\t" \
        : [bit]"+d"(bit), [dst]"+r"(dst) : [dstStrideX]"r"(dstStrideX) \
    );
#else
#define vdi_Soft_drawGeneralLineRotate(bit, dst, dstStrideX) \
    { \
        rorw1(bit); \
        if (bit & 0x8000) \
            dst += dstStrideX; \
    }
#endif
    
forceinline UWORD vdi_Soft_drawGeneralLineLoopTemplate(WORD mode, UWORD color, bool lastLineFlag, UWORD lineMask, UWORD bit, char *dst, WORD dstStrideX, WORD dstStrideY, WORD dx, WORD dy, bool lineMaskSolid) {
    if (dx >= dy) {
        WORD eps = -dx, e1 = 2*dy, e2 = 2*dx;
        dx -= !lastLineFlag;
        if (dx >= 0) {
            LOOP_DO(x, dx + 1) {
                if (!lineMaskSolid)
                    rolw1(lineMask);
                vdi_Soft_drawLinePointTemplate(mode, color, lineMask, bit, (UWORD*)dst);
                vdi_Soft_drawGeneralLineRotate(bit, dst, dstStrideX);
                eps += e1;
                if (eps >= 0) {
                    eps -= e2;
                    dst += dstStrideY;
                }
            } LOOP_WHILE(x);
        }
    } else {
        WORD eps = -dy, e1 = 2*dx, e2 = 2*dy;
        dy -= !lastLineFlag;
        if (dy >= 0) {
            LOOP_DO(y, dy + 1) {
                if (!lineMaskSolid)
                    rolw1(lineMask);
                vdi_Soft_drawLinePointTemplate(mode, color, lineMask, bit, (UWORD*)dst);
                dst += dstStrideY;
                eps += e1;
                if (eps >= 0) {
                    eps -= e2;
                    vdi_Soft_drawGeneralLineRotate(bit, dst, dstStrideX);
                }
            } LOOP_WHILE(y);
         }
    }
    return lineMask;
}

forceinline UWORD vdi_Soft_drawGeneralLinePlaneTemplate(WORD mode, UWORD color, bool lastLineFlag, UWORD lineMask, WORD planeNb, UWORD bit, char *dst, WORD dstStrideX, WORD dstStrideY, WORD dx, WORD dy, bool lineMaskSolid, bool colorSpecialized) {
    UWORD lineMaskOut;
    LOOP_DO(planeIndex, planeNb) {
        if (colorSpecialized) {
            if (color & 1)
                lineMaskOut = vdi_Soft_drawGeneralLineLoopTemplate(mode, 1, lastLineFlag, lineMask, bit, dst, dstStrideX, dstStrideY, dx, dy, lineMaskSolid);
            else
                lineMaskOut = vdi_Soft_drawGeneralLineLoopTemplate(mode, 0, lastLineFlag, lineMask, bit, dst, dstStrideX, dstStrideY, dx, dy, lineMaskSolid);
        } else
            lineMaskOut = vdi_Soft_drawGeneralLineLoopTemplate(mode, color, lastLineFlag, lineMask, bit, dst, dstStrideX, dstStrideY, dx, dy, lineMaskSolid);
        dst += 2;
        color >>= 1;
    } LOOP_WHILE(planeIndex);
    return lineMaskOut;
}

//********************************************************************************
// General line drawing - Multi plane.
//********************************************************************************
//--------------------------------------------------------------------------------
// General line drawing - Multi plane - Assembly.
//--------------------------------------------------------------------------------
#if vdi_Soft_drawGeneralLineAsmEnabled

//--------------------------------------------------------------------------------
// General line drawing - Multi plane - C.
//--------------------------------------------------------------------------------
#else

#define vdi_Soft_drawGeneralLineMultiplane() \
    if (dx >= dy) { \
        WORD eps = -dx, e1 = 2*dy, e2 = 2*dx; \
        dx -= !lastLineFlag; \
        if (dx >= 0) { \
            LOOP_DO(x, dx + 1) { \
                if (!lineMaskSolid) \
                    rolw1(lineMask); \
                vdi_Soft_drawLinePointTemplate(mode, color, lineMask, bit, (UWORD*)dst); \
                vdi_Soft_drawGeneralLineRotate(bit, dst, dstStrideX); \
                eps += e1; \
                if (eps >= 0) { \
                    eps -= e2; \
                    dst += dstStrideY; \
                } \
            } LOOP_WHILE(x); \
        } \
    } else { \
        WORD eps = -dy, e1 = 2*dx, e2 = 2*dy; \
        dy -= !lastLineFlag; \
        if (dy >= 0) { \
            LOOP_DO(y, dy + 1) { \
                if (!lineMaskSolid) \
                    rolw1(lineMask); \
                vdi_Soft_drawLinePointTemplate(mode, color, lineMask, bit, (UWORD*)dst); \
                dst += dstStrideY; \
                eps += e1; \
                if (eps >= 0) { \
                    eps -= e2; \
                    vdi_Soft_drawGeneralLineRotate(bit, dst, dstStrideX); \
                } \
            } LOOP_WHILE(y); \
         } \
    }


//
// Replace.
//

#define vdi_Soft_drawGeneralLineMultiplanePatternReplace(type, regIndex) \
    type pattern##regIndex = colorToMask##type(&color) & bitMaskReg;
#define vdi_Soft_drawGeneralLineMultiplaneBeginReplace(type, size, solid) \
    { \
        dstStride -= planeNb << 1; \
        type bitMaskInverse = ~bitMaskReg; \
        LOOP_DO(y, h) { \
            type * RESTRICT d = (type*)dst;
#define vdi_Soft_drawGeneralLineMultiplaneOpSetReplace(type, size, solid, regIndex) \
            { type dp = *d; *d++ = (dp & bitMaskInverse) | pattern##regIndex; }
#define vdi_Soft_drawGeneralLineMultiplaneOpClearReplace(type, size, solid, regIndex) \
            *d++ &= bitMaskInverse;
#define vdi_Soft_drawGeneralLineMultiplaneEndReplace(regNb, solid) \
            if (!solid) \
                rolw1(lineMask); \
            dst = (UBYTE*)d + dstStride; \
        } LOOP_WHILE(y); \
    }

//
// Xor.
//
    
#define vdi_Soft_drawGeneralLineMultiplanePatternXor(type, regIndex)
#define vdi_Soft_drawGeneralLineMultiplaneBeginXor(type, size, solid) \
    { \
        if (solid) \
            dstStride -= planeNb << 1; \
        LOOP_DO(y, h) { \
            type * RESTRICT d = (type*)dst;
#define vdi_Soft_drawGeneralLineMultiplaneOpSetXor(type, size, solid, regIndex) \
            *d++ ^= bitMaskReg;
#define vdi_Soft_drawGeneralLineMultiplaneOpClearXor(type, size, solid, regIndex)
#define vdi_Soft_drawGeneralLineMultiplaneEndXor(regNb, solid) \
            if (!solid) \
                rolw1(lineMask); \
            dst = solid ? (UBYTE*)d + dstStride : dst + dstStride ; \
        } LOOP_WHILE(y); \
    }

//
// Trans.
//

#define vdi_Soft_drawGeneralLineMultiplanePatternTrans(type, regIndex) \
    type pattern##regIndex = colorToMask##type(&color) & bitMaskReg;
#define vdi_Soft_drawGeneralLineMultiplaneBeginTrans(type, size, solid) \
    { \
        if (solid) \
            dstStride -= planeNb << 1; \
        type bitMaskInverse = ~bitMaskReg; \
        LOOP_DO(y, h) { \
            type * RESTRICT d = (type*)dst;
#define vdi_Soft_drawGeneralLineMultiplaneOpSetTrans(type, size, solid, regIndex) \
            { type dp = *d; *d++ = (dp & bitMaskInverse) | pattern##regIndex; }
#define vdi_Soft_drawGeneralLineMultiplaneOpClearTrans(type, size, solid, regIndex)
#define vdi_Soft_drawGeneralLineMultiplaneEndTrans(regNb, solid) \
            if (!solid) \
                rolw1(lineMask); \
            dst = solid ? (UBYTE*)d + dstStride : dst + dstStride ; \
        } LOOP_WHILE(y); \
    }

#define vdi_Soft_drawGeneralLineMultiplanePattern(op, type, regIndex) \
    vdi_Soft_drawGeneralLineMultiplanePattern##op(type, regIndex)

#define vdi_Soft_drawGeneralLineMultiplaneBegin(op, type, size, solid) \
    vdi_Soft_drawGeneralLineMultiplaneBegin##op(type, size, solid)
#define vdi_Soft_drawGeneralLineMultiplaneOpSet(op, type, size, solid, regIndex) \
    vdi_Soft_drawGeneralLineMultiplaneOpSet##op(type, size, solid, regIndex)
#define vdi_Soft_drawGeneralLineMultiplaneOpClear(op, type, size, solid, regIndex) \
    vdi_Soft_drawGeneralLineMultiplaneOpClear##op(type, size, solid, regIndex)
#define vdi_Soft_drawGeneralLineMultiplaneEnd(op, regNb, solid) \
    vdi_Soft_drawGeneralLineMultiplaneEnd##op(regNb, solid)
    
#define vdi_Soft_drawGeneralLineMultiplaneTemplate1(op, solid) \
    { \
        UWORD bitMaskReg = bitMask; \
        vdi_Soft_drawGeneralLineMultiplanePattern(op, UWORD, 0) \
        vdi_Soft_drawGeneralLineMultiplaneBegin(op, UWORD, w, solid) \
            if (solid || (lineMask & 1)) { \
                vdi_Soft_drawGeneralLineMultiplaneOpSet(op, UWORD, w, solid, 0) \
            } else { \
                vdi_Soft_drawGeneralLineMultiplaneOpClear(op, UWORD, w, solid, 0) \
            } \
        vdi_Soft_drawGeneralLineMultiplaneEnd(op, 1, solid) \
    }
#define vdi_Soft_drawGeneralLineMultiplaneTemplate2(op, solid) \
    { \
        ULONG bitMaskReg = broadcastWord(bitMask); \
        vdi_Soft_drawGeneralLineMultiplanePattern(op, ULONG, 0) \
        vdi_Soft_drawGeneralLineMultiplaneBegin(op, ULONG, l, solid) \
            if (solid || (lineMask & 1)) { \
                vdi_Soft_drawGeneralLineMultiplaneOpSet(op, ULONG, l, solid, 0) \
            } else { \
                vdi_Soft_drawGeneralLineMultiplaneOpClear(op, ULONG, l, solid, 0) \
            } \
        vdi_Soft_drawGeneralLineMultiplaneEnd(op, 1, solid) \
    }
#define vdi_Soft_drawGeneralLineMultiplaneTemplate4(op, solid) \
    { \
        ULONG bitMaskReg = broadcastWord(bitMask); \
        vdi_Soft_drawGeneralLineMultiplanePattern(op, ULONG, 0) \
        vdi_Soft_drawGeneralLineMultiplanePattern(op, ULONG, 1) \
        vdi_Soft_drawGeneralLineMultiplaneBegin(op, ULONG, l, solid) \
            if (solid || (lineMask & 1)) { \
                vdi_Soft_drawGeneralLineMultiplaneOpSet(op, ULONG, l, solid, 0) \
                vdi_Soft_drawGeneralLineMultiplaneOpSet(op, ULONG, l, solid, 1) \
            } else { \
                vdi_Soft_drawGeneralLineMultiplaneOpClear(op, ULONG, l, solid, 0) \
                vdi_Soft_drawGeneralLineMultiplaneOpClear(op, ULONG, l, solid, 1) \
            } \
        vdi_Soft_drawGeneralLineMultiplaneEnd(op, 2, solid) \
    }
#define vdi_Soft_drawGeneralLineMultiplaneTemplate8(op, solid) \
    { \
        ULONG bitMaskReg = broadcastWord(bitMask); \
        vdi_Soft_drawGeneralLineMultiplanePattern(op, ULONG, 0) \
        vdi_Soft_drawGeneralLineMultiplanePattern(op, ULONG, 1) \
        vdi_Soft_drawGeneralLineMultiplanePattern(op, ULONG, 2) \
        vdi_Soft_drawGeneralLineMultiplanePattern(op, ULONG, 3) \
        vdi_Soft_drawGeneralLineMultiplaneBegin(op, ULONG, l, solid) \
            if (solid || (lineMask & 1)) { \
                vdi_Soft_drawGeneralLineMultiplaneOpSet(op, ULONG, l, solid, 0) \
                vdi_Soft_drawGeneralLineMultiplaneOpSet(op, ULONG, l, solid, 1) \
                vdi_Soft_drawGeneralLineMultiplaneOpSet(op, ULONG, l, solid, 2) \
                vdi_Soft_drawGeneralLineMultiplaneOpSet(op, ULONG, l, solid, 3) \
            } else { \
                vdi_Soft_drawGeneralLineMultiplaneOpClear(op, ULONG, l, solid, 0) \
                vdi_Soft_drawGeneralLineMultiplaneOpClear(op, ULONG, l, solid, 1) \
                vdi_Soft_drawGeneralLineMultiplaneOpClear(op, ULONG, l, solid, 2) \
                vdi_Soft_drawGeneralLineMultiplaneOpClear(op, ULONG, l, solid, 3) \
            } \
        vdi_Soft_drawGeneralLineMultiplaneEnd(op, 4, solid) \
    }

#endif

//--------------------------------------------------------------------------------
// General line drawing - Multi plane - Main.
//--------------------------------------------------------------------------------  
#define vdi_Soft_drawGeneralLineMultiplaneTemplate(op, solid) \
    switch (planeNb) { \
    default: \
    vdi_Soft_planar1(case 1: vdi_Soft_drawGeneralLineMultiplaneTemplate1(op, solid) break;) \
    vdi_Soft_planar2(case 2: vdi_Soft_drawGeneralLineMultiplaneTemplate2(op, solid) break;) \
    vdi_Soft_planar4(case 4: vdi_Soft_drawGeneralLineMultiplaneTemplate4(op, solid) break;) \
    vdi_Soft_planar8(case 8: vdi_Soft_drawGeneralLineMultiplaneTemplate8(op, solid) break;) \
    }

//********************************************************************************
// General line drawing - Main.
//********************************************************************************
forceinline UWORD vdi_Soft_drawGeneralLineModeTemplate(WORD mode, UWORD color, bool lastLineFlag, UWORD lineMask, WORD planeNb, UWORD bit, char *dst, WORD dstStrideX, WORD dstStrideY, WORD dx, WORD dy, bool lineMaskSolid, bool colorSpecialized, bool multiPlaneSpecialized, bool opSpecialized) {
    if (multiPlaneSpecialized) {
        #if 0
        if (lineMaskSolid) {
            switch (mode) {
            case WM_ERASE: // Erase does nothing when the line is solid.
                break;
            case WM_TRANS: // Transparent is the same as replace when the line is solid.
            default:
            case WM_REPLACE:
                vdi_Soft_drawGeneralLineMultiplaneTemplate(Replace, true);
                break;
            case WM_XOR:
                vdi_Soft_drawGeneralLineMultiplaneTemplate(Xor, true);
                break;
            }
        } else {
            switch (mode) {
            case WM_ERASE:
                lineMask = ~lineMask;
                // Fallthrough.
            case WM_TRANS:
                if (lineMask != 0x0000)
                    vdi_Soft_drawGeneralLineMultiplaneTemplate(Trans, false);
                if (mode == WM_ERASE)
                    lineMask = ~lineMask;
                break;
            default:
            case WM_REPLACE:
                vdi_Soft_drawGeneralLineMultiplaneTemplate(Replace, false);
                break;
            case WM_XOR:
                if (lineMask != 0x0000)
                    vdi_Soft_drawGeneralLineMultiplaneTemplate(Xor, false);
                break;
            }
        }
        #endif
    } else if (opSpecialized) {
        switch (mode) {
        case WM_ERASE:
            lineMask = ~lineMask;
            // Fallthrough.
        case WM_TRANS:
            if (lineMask != 0x0000)
                lineMask = vdi_Soft_drawGeneralLinePlaneTemplate(WM_TRANS, color, lastLineFlag, lineMask, planeNb, bit, dst, dstStrideX, dstStrideY, dx, dy, lineMaskSolid, colorSpecialized);
            if (mode == WM_ERASE)
                lineMask = ~lineMask;
            break;
        default:
        case WM_REPLACE:
            lineMask = vdi_Soft_drawGeneralLinePlaneTemplate(WM_REPLACE, color, lastLineFlag, lineMask, planeNb, bit, dst, dstStrideX, dstStrideY, dx, dy, lineMaskSolid, colorSpecialized);
            break;
        case WM_XOR:
            if (lineMask != 0x0000)
                lineMask = vdi_Soft_drawGeneralLinePlaneTemplate(WM_XOR, color, lastLineFlag, lineMask, planeNb, bit, dst, dstStrideX, dstStrideY, dx, dy, lineMaskSolid, colorSpecialized);
            break;
        }
    } else {
        if (mode == WM_ERASE)
            lineMask = ~lineMask;
        lineMask = vdi_Soft_drawGeneralLinePlaneTemplate(mode, color, lastLineFlag, lineMask, planeNb, bit, dst, dstStrideX, dstStrideY, dx, dy, lineMaskSolid, colorSpecialized);
        if (mode == WM_ERASE)
            lineMask = ~lineMask;
    }
    return lineMask;
}

vdi_Soft_drawGeneralLineOpti
UWORD vdi_Soft_drawGeneralLine(const Line *line, WORD mode, UWORD color, UWORD lineMask, bool lastLineFlag) {
    WORD x1 = line->x1, x2 = line->x2, y1 = line->y1, y2 = line->y2;
    // Always draw from left to right.
    if (x1 > x2) {
        WORD xt = x1; x1 = x2; x2 = xt;
        WORD yt = y1; y1 = y2; y2 = yt;
    }

    #if vdi_drawLine_lastLineLegacy
    // Copy a DRI kludge: if we're in XOR mode, avoid XORing intermediate points in a polyline.
    // We do it slightly differently than DRI with slightly differing results - but it's a kludge in either case.
    if (mode == WM_XOR && !lastLineFlag) {
        if (x1 != x2)
            x2--;
        else if (y1 != y2)
            y2--;
    }
    lastLineFlag = false;
    #endif

    WORD dx = x2 - x1, dy = y2 - y1;
    WORD dstStrideY = lineaVars.screen_lineSize2;
    if (dy < 0) {
        dy = -dy;
        dstStrideY = -dstStrideY;
    }
    char *dst = (char*)vdi_getPixelAddress(x1, y1);
    UWORD bit = 0x8000 >> (x1 & 0xf); // Initial bit position.
    WORD planeNb = lineaVars.screen_planeNb, dstStrideX = planeNb << 1;
    if (vdi_Soft_drawGeneralLine_version_lineMaskSpecialized && lineMask == 0xffff)
        vdi_Soft_drawGeneralLineModeTemplate(mode, color, lastLineFlag, 0xffff, planeNb, bit, dst, dstStrideX, dstStrideY, dx, dy, true, vdi_Soft_drawGeneralLine_version_opSpecialized, vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized, vdi_Soft_drawGeneralLine_version_opSpecialized);
    else
        lineMask = vdi_Soft_drawGeneralLineModeTemplate(mode, color, lastLineFlag, lineMask, planeNb, bit, dst, dstStrideX, dstStrideY, dx, dy, false, vdi_Soft_drawGeneralLine_version_opSpecialized, vdi_Soft_drawGeneralLine_version_multiPlaneSpecialized, vdi_Soft_drawGeneralLine_version_opSpecialized);
    return lineMask;
}

//--------------------------------------------------------------------------------
// Line.
// It handles all cases and chooses a specialized version if possible.
// Can be replaced if a driver prefers to specialized by itself.
//--------------------------------------------------------------------------------
UWORD vdi_Soft_drawLine(const Line *line, WORD mode, UWORD color, UWORD lineMask, bool lastLineFlag) {
    const vdi_Driver *driver = vdi_getDriver();
    #if CONF_WITH_VDI_VERTLINE
    if (line->x1 == line->x2)
        lineMask = driver->drawVerticalLine(line, mode, color, lineMask, lastLineFlag);
    else
    #endif   
    #if CONF_WITH_VDI_HORILINE
    if (line->y1 == line->y2)
        lineMask = driver->drawHorizontalLine(line, mode, color, lineMask, lastLineFlag);
    else
    #endif
        lineMask = driver->drawGeneralLine(line, mode, color, lineMask, lastLineFlag);
    return lineMask;
}

//********************************************************************************
// Circle.
//********************************************************************************

//********************************************************************************
// Blitting.
//********************************************************************************
#if !vdi_blitterOnly

// Versions from smallest to biggest and slowest to fastest.
#define vdi_Soft_blit_version_smallest 0 // Smallest. But very slow because there is no specialization at all.
#define vdi_Soft_blit_version_small 1 // Bigger and faster than the "smallest".
#define vdi_Soft_blit_version_fast 2 // Bigger and faster than the "small".
#define vdi_Soft_blit_version_faster 4 // Bigger than "fast", fast for most cases.
#define vdi_Soft_blit_version_fastest 5 // Largest. Bigger than "faster", fastest for all cases.

#if defined(TARGET_192)
#define vdi_Soft_blit_version vdi_Soft_blit_version_small
#else
#define vdi_Soft_blit_version vdi_Soft_blit_version_fastest
#endif

typedef enum vdi_Soft_BlitOpSpecialization_ {
    vdi_Soft_BlitOpSpecialization_none, // No specialization.
    vdi_Soft_BlitOpSpecialization_partial, // There are functions specialized for most used ops.
    vdi_Soft_BlitOpSpecialization_all // There are functions specialized for each op.
} vdi_Soft_BlitOpSpecialization;

/*
Available optimization flags:
vdi_Soft_blit_version_opSpecialization // Level of specialization for each op, using vdi_Soft_BlitOpSpecialization enum.
vdi_Soft_blit_version_skewSpecialized // There are functions specialized for the case where skew == 0.
vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled // The end masks are manager outside the inner loop.
vdi_Soft_blit_version_directionSpecialized // There are functions specialized for each direction.
vdi_Soft_blit_version_allPlanesAtOnceEnabled // There are functions which process all planes at once when skew == 0, foreground and background == 0, words are contiguous.
TODO: For the simple cases, an unrolled inner loop using a jump table could be used to reduce the cost of dbra (similar to rectangle drawing one).
*/

#if vdi_Soft_blit_version == vdi_Soft_blit_version_smallest
    #define vdi_Soft_blit_version_opSpecialization vdi_Soft_BlitOpSpecialization_none
    #define vdi_Soft_blit_version_skewSpecialized 0
    #define vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled 0
    #define vdi_Soft_blit_version_directionSpecialized 0
    #define vdi_Soft_blit_version_allPlanesAtOnceEnabled 0
#elif vdi_Soft_blit_version == vdi_Soft_blit_version_small
    #define vdi_Soft_blit_version_opSpecialization vdi_Soft_BlitOpSpecialization_partial
    #define vdi_Soft_blit_version_skewSpecialized 0
    #define vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled 0
    #define vdi_Soft_blit_version_directionSpecialized 0
    #define vdi_Soft_blit_version_allPlanesAtOnceEnabled 0
#elif vdi_Soft_blit_version == vdi_Soft_blit_version_fast
    #define vdi_Soft_blit_version_opSpecialization vdi_Soft_BlitOpSpecialization_partial
    #define vdi_Soft_blit_version_skewSpecialized 1
    #define vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled 1
    #define vdi_Soft_blit_version_directionSpecialized 1
    #define vdi_Soft_blit_version_allPlanesAtOnceEnabled 0
#elif vdi_Soft_blit_version == vdi_Soft_blit_version_faster
    #define vdi_Soft_blit_version_opSpecialization vdi_Soft_BlitOpSpecialization_partial
    #define vdi_Soft_blit_version_skewSpecialized 1
    #define vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled 1
    #define vdi_Soft_blit_version_directionSpecialized 1
    #define vdi_Soft_blit_version_allPlanesAtOnceEnabled 1
#elif vdi_Soft_blit_version == vdi_Soft_blit_version_fastest
    #define vdi_Soft_blit_version_opSpecialization vdi_Soft_BlitOpSpecialization_all
    #define vdi_Soft_blit_version_skewSpecialized 1
    #define vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled 1
    #define vdi_Soft_blit_version_directionSpecialized 1
    #define vdi_Soft_blit_version_allPlanesAtOnceEnabled 1
#endif

// - With O3 or O2, gcc over-optimizes and generates rather large code. So Os is used.
// - "-ftree-scev-cprop" must be disabled because Gcc generates absurd (large, slow and useless) code.
//    Mainly it generates stupid multiplications to compute the value at the end of a loop. 
//    They are completely useless, and furthermore, it uses a function call.
#define vdi_Soft_blitOpti __attribute__ ((optimize("Os"), optimize("no-tree-scev-cprop")))

forceinline void vdi_Soft_blitWordOpLoop(char **src, char **dst, ULONG srcDeltaX, ULONG dstDeltaX, WORD planeNb, WORD wordNb, UWORD endMask, WORD op) {
    char *s = *src, *d = *dst;
    LOOP_DO(wordCount, wordNb) {
        *(UWORD*)d = doPixelOpWithMask(op, *(UWORD*)s, *(UWORD*)d, endMask);
        s += srcDeltaX;
        d += dstDeltaX;           
    } LOOP_WHILE(wordCount);
    *src = s; *dst = d;
}
 
forceinline ULONG vdi_Soft_blitWordWithSkew(ULONG srcWordAcc, char *src, char *dst, UWORD skew, UWORD endMask, bool rightToLeftFlag, WORD op) {
    ULONG srcWordTemp = *(UWORD*)src;
    if (rightToLeftFlag) {
        srcWordAcc >>= 16;
        srcWordAcc |= srcWordTemp << 16;
    } else {
        srcWordAcc <<= 16;
        srcWordAcc |= srcWordTemp;
    }
    UWORD srcWord = srcWordAcc >> skew;
    UWORD dstWord = *(UWORD*)dst;
    *(UWORD*)dst = doPixelOpWithMask(op, srcWord, dstWord, endMask);
    return srcWordAcc;
}

forceinline void vdi_Soft_blitTemplate(BLIT * restrict blt, WORD planeNb, bool skewFlag, bool planarFlag, bool rightToLeftFlag, bool endMasksOutsideInnerLoopFlag, WORD op) {
    ULONG srcDeltaX = blt->src_x_inc, srcDeltaY = blt->src_y_inc; char *src = (char*)blt->src_addr;
    ULONG dstDeltaX = blt->dst_x_inc, dstDeltaY = blt->dst_y_inc; char *dst = (char*)blt->dst_addr;
    UWORD endMask1 = blt->endmask_1, endMask2 = 0xffff, endMask3 = blt->endmask_3;
    WORD w = blt->x_count, h = blt->y_count;
    if (!endMasksOutsideInnerLoopFlag) {
        // Version with all end masks managed in the inner loop (smaller code but slow).
        srcDeltaY -= srcDeltaX;
        dstDeltaY -= dstDeltaX;
        if (skewFlag) {
            UWORD flags = blt->skew;
            UWORD skew = flags & SKEW;
            if (flags & NFSR)
                srcDeltaY -= srcDeltaX;
            LOOP_DO(y, h) {
                ULONG srcWordAcc = 0;
                if (flags & FXSR) {
                    srcWordAcc = *(UWORD*)src;
                    src += srcDeltaX;
                    if (rightToLeftFlag)
                        srcWordAcc <<= 16;
                }
                UWORD endMask = endMask1;
                WORD x = w;
                do {
                    srcWordAcc = vdi_Soft_blitWordWithSkew(srcWordAcc, src, dst, skew, endMask, rightToLeftFlag, op);
                    src += srcDeltaX;
                    dst += dstDeltaX;           
                    x--;
                    endMask = (x == 1) ? endMask3 : endMask2;
                } while (x > 0);
                src += srcDeltaY;
                dst += dstDeltaY;
            } LOOP_WHILE(y);
        } else {
            LOOP_DO(y, h) {
                UWORD endMask = endMask1;
                WORD x = w;
                do {
                    *(UWORD*)dst = doPixelOpWithMask(op, *(UWORD*)src, *(UWORD*)dst, endMask);
                    src += srcDeltaX;
                    dst += dstDeltaX;
                    x--;
                    endMask = (x == 1) ? endMask3 : endMask2;
                } while (x > 0);
                src += srcDeltaY;
                dst += dstDeltaY;
            } LOOP_WHILE(y);
        }
    } else {
        // Version with end masks managed separately for faster inner loop.
        if (skewFlag) {
            // The source must be shifted relative to the destination. Planes are processed independently of each other.
            w -= 2; // -1 for left fringe, -1 for right fringe.
            UWORD flags = blt->skew;
            UWORD skew = flags & SKEW;
            if (flags & NFSR)
                srcDeltaY -= srcDeltaX;
            LOOP_DO(y, h) {
                ULONG srcWordAcc = 0;
                if (flags & FXSR) {
                    srcWordAcc = *(UWORD*)src;
                    src += srcDeltaX;
                    if (rightToLeftFlag)
                        srcWordAcc <<= 16;
                }
                srcWordAcc = vdi_Soft_blitWordWithSkew(srcWordAcc, src, dst, skew, endMask1, rightToLeftFlag, op);
                if (w >= 0) {
                    src += srcDeltaX;
                    dst += dstDeltaX;
                    if (w > 0) {
                        LOOP_DO(x, w) {
                            srcWordAcc = vdi_Soft_blitWordWithSkew(srcWordAcc, src, dst, skew, endMask2, rightToLeftFlag, op);
                            src += srcDeltaX;
                            dst += dstDeltaX;           
                        } LOOP_WHILE(x);
                    }
                    srcWordAcc = vdi_Soft_blitWordWithSkew(srcWordAcc, src, dst, skew, endMask3, rightToLeftFlag, op);
                }
                src += srcDeltaY;
                dst += dstDeltaY;
            } LOOP_WHILE(y);
        } else if (planarFlag) {
            // Source and destination are aligned, no shift needed. Planes are processed independently of each other.
            w -= 2; // -1 for left fringe, -1 for right fringe.
            LOOP_DO(y, h) {
                *(UWORD*)dst = doPixelOpWithMask(op, *(UWORD*)src, *(UWORD*)dst, endMask1);
                if (w >= 0) {
                    src += srcDeltaX;
                    dst += dstDeltaX;
                    if (w > 0)
                        vdi_Soft_blitWordOpLoop(&src, &dst, srcDeltaX, dstDeltaX, planeNb, w, endMask2, op);
                    *(UWORD*)dst = doPixelOpWithMask(op, *(UWORD*)src, *(UWORD*)dst, endMask3);
                }
                src += srcDeltaY;
                dst += dstDeltaY;
            } LOOP_WHILE(y);
        } else {
            // Source and destination are aligned, no shift needed. Planes are processed together.
            // TODO: Remove plane loops and use movem.l ?
            // TODO: Specialize for 1 word, 2 words and multiple words per line ?
            // TODO: Use jump for large blocks ?
            // TODO: All the above for simple mode replace mode only ?
            w -= 2; // -1 for left fringe, -1 for right fringe.
            WORD wordNb = vdi_scaleWordByPlaneNb(w, planeNb);
            srcDeltaY -= srcDeltaX;
            dstDeltaY -= dstDeltaX;
            if (rightToLeftFlag) {
                srcDeltaX = -2;
                dstDeltaX = -2;
                WORD offset = (planeNb - 1) << 1;
                src += offset;
                dst += offset;
            } else {
                srcDeltaX = 2;
                dstDeltaX = 2;
            }
            LOOP_DO(y, h) {
                vdi_Soft_blitWordOpLoop(&src, &dst, srcDeltaX, dstDeltaX, planeNb, planeNb, endMask1, op);
                if (w >= 0) {
                    if (w > 0)
                        vdi_Soft_blitWordOpLoop(&src, &dst, srcDeltaX, dstDeltaX, planeNb, wordNb, endMask2, op);
                    vdi_Soft_blitWordOpLoop(&src, &dst, srcDeltaX, dstDeltaX, planeNb, planeNb, endMask3, op);
                }
                src += srcDeltaY;
                dst += dstDeltaY;
            } LOOP_WHILE(y);
        }
    }
}

forceinline void vdi_Soft_blitTemplateOp(BLIT * restrict blt, WORD planeNb, bool skewFlag, bool planarFlag, bool rightToLeftFlag, bool endMasksOutsideInnerLoopFlag, vdi_Soft_BlitOpSpecialization opSpecialization) {
    switch (opSpecialization) {
    default:
    case vdi_Soft_BlitOpSpecialization_none:
        vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, blt->op);
        break;
    case vdi_Soft_BlitOpSpecialization_partial:
        switch (blt->op) {
        default:            vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, blt->op); break;
        case BM_ALL_WHITE:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_ALL_WHITE); break;
        case BM_S_AND_D:    vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_AND_D); break;
        case BM_S_ONLY:     vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_ONLY); break;
        case BM_NOTS_AND_D: vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOTS_AND_D); break;
        case BM_S_XOR_D:    vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_XOR_D); break;
        case BM_S_OR_D:     vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_OR_D); break;
        case BM_NOT_S:      vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOT_S); break;
        case BM_NOTS_OR_D:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOTS_OR_D); break;
        case BM_ALL_BLACK:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_ALL_BLACK); break;
        }
        break;
    case vdi_Soft_BlitOpSpecialization_all:
        switch (blt->op) {
        default:
        case BM_ALL_WHITE:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_ALL_WHITE); break;
        case BM_S_AND_D:    vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_AND_D); break;
        case BM_S_AND_NOTD: vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_AND_NOTD); break;
        case BM_S_ONLY:     vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_ONLY); break;
        case BM_NOTS_AND_D: vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOTS_AND_D); break;
        case BM_D_ONLY:     vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_D_ONLY); break;
        case BM_S_XOR_D:    vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_XOR_D); break;
        case BM_S_OR_D:     vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_OR_D); break;
        case BM_NOT_SORD:   vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOT_SXORD); break;
        case BM_NOT_SXORD:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOT_SXORD); break;
        case BM_NOT_D:      vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOT_D); break;
        case BM_S_OR_NOTD:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_S_OR_NOTD); break;
        case BM_NOT_S:      vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOT_S); break;
        case BM_NOTS_OR_D:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOTS_OR_D); break;
        case BM_NOT_SANDD:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_NOT_SANDD); break;
        case BM_ALL_BLACK:  vdi_Soft_blitTemplate(blt, planeNb, skewFlag, planarFlag, rightToLeftFlag, endMasksOutsideInnerLoopFlag, BM_ALL_BLACK); break;
        }
        break;
    }
}

vdi_Soft_blitOpti
void vdi_Soft_blitPlane(BLIT * restrict blt) {
    #if vdi_Soft_blit_version_skewSpecialized
    if (blt->skew & SKEW) {
    #endif
        #if vdi_Soft_blit_version_directionSpecialized
        if (blt->src_x_inc < 0)
            vdi_Soft_blitTemplateOp(blt, 0, true, true, true, vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled, vdi_Soft_blit_version_opSpecialization);
        else
            vdi_Soft_blitTemplateOp(blt, 0, true, true, false, vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled, vdi_Soft_blit_version_opSpecialization);
        #else
        vdi_Soft_blitTemplateOp(blt, 0, true, true, blt->src_x_inc < 0, vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled, vdi_Soft_blit_version_opSpecialization);
        #endif
    #if vdi_Soft_blit_version_skewSpecialized
    } else
        vdi_Soft_blitTemplateOp(blt, 0, false, true, false, vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled, vdi_Soft_blit_version_opSpecialization);
    #endif
}

#if vdi_Soft_blit_version_allPlanesAtOnceEnabled
vdi_Soft_blitOpti
static void vdi_Soft_blitSimple(const vdi_BlitParameters *blit_info, BLIT * restrict blt) {
    WORD planeNb = blit_info->plane_ct;
    blt->op = blit_info->op_tab[0] & 0xf;
    #if vdi_Soft_blit_version_directionSpecialized
    if (blt->src_x_inc < 0)
        vdi_Soft_blitTemplateOp(blt, planeNb, false, false, true, vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled, vdi_Soft_blit_version_opSpecialization);
    else
        vdi_Soft_blitTemplateOp(blt, planeNb, false, false, false, vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled, vdi_Soft_blit_version_opSpecialization);
    #else
     vdi_Soft_blitTemplateOp(blt, planeNb, false, false, blt->src_x_inc < 0, vdi_Soft_blit_version_endMasksOutsideInnerLoopEnabled, vdi_Soft_blit_version_opSpecialization);
    #endif
}
#endif

#endif

// General (complex) cases.
void vdi_Soft_blitGeneral(const vdi_BlitParameters *blit_info, BLIT * restrict blt) {
    const vdi_Driver *driver = vdi_getDriver();
    WORD colorFg = blit_info->fg_col, colorBg = blit_info->bg_col;
    WORD plane = blit_info->plane_ct - 1;
    do {
        blt->op = blit_info->op_tab[((colorFg & 0x1) << 1) | (colorBg & 0x1)] & 0xf;
        colorFg >>= 1; colorBg >>= 1;
        driver->blitPlane(blt);
        blt->src_addr = (UWORD*)((UBYTE*)blt->src_addr + blit_info->s_nxpl);
        blt->dst_addr = (UWORD*)((UBYTE*)blt->dst_addr + blit_info->d_nxpl);
    } while (--plane != -1);
}

void vdi_Soft_blitAll(const vdi_BlitParameters *blit_info, BLIT * restrict blt) {
    #if vdi_Soft_blit_version_allPlanesAtOnceEnabled
    if ((blt->skew & SKEW) == 0 && blit_info->fg_col == 0 && blit_info->bg_col == 0 && blit_info->s_nxwd == blit_info->d_nxwd && blit_info->d_nxwd == (blit_info->plane_ct << 1) &&  blit_info->s_nxpl == blit_info->d_nxpl && blit_info->d_nxpl == 2)
        // Simple cases. Foreground, background and skew are 0, every words are contiguous, same format for source and destination.
        // Blitting an be done with all planes simutaneously.
        vdi_Soft_blitSimple(blit_info, blt);
    else
    #endif
/*
    #if vdi_Soft_blit_version_monoPlaneSourceEnabled
    if (!blitter_is_enabled && (blt->skew & SKEW) == 0 && blit_info->fg_col == 0 && blit_info->bg_col == 0 && blit_info->s_nxwd == 2 && blit_info->d_nxwd == (planeNb << 1) &&  blit_info->d_nxpl == 2) {
        // TODO: One source plane and multiple destination planes.
    else
    #endif
*/
        // General cases. Blitting is done one plane at a time.
        vdi_getDriver()->blitGeneral(blit_info, blt);
}

/*
 * Transfer a rectangular block of pixels located at an arbitrary X,Y
 * position in the source memory form to another arbitrary X,Y position
 * in the destination memory form, using replace mode (boolean operator 3).
 * This is used on ColdFire (where vdi_BitBlt_blitAsm() is not available) or if
 * configuring with the blitter on a 68K system, since vdi_BitBlt_blitAsm() does
 * not provide an interface to the hardware.
 *
 * In:
 *  blit_info   pointer to 76 byte input parameter block
 *
 * Note: This is a translation of the original assembler code in the Atari
 * blitter document, with the addition that source and destination are
 * allowed to overlap.  Original source code comments are mostly preserved.
 */
void vdi_Soft_blit(const vdi_BlitParameters *blit_info) {
    /* Calculate Xmax coordinates from Xmin coordinates and width */
    UWORD s_xmin = blit_info->s_xmin;               /* src Xmin */
    UWORD s_xmax = s_xmin + blit_info->b_wd - 1;    /* src Xmax = src Xmin + width-1 */
    UWORD d_xmin = blit_info->d_xmin;               /* dst Xmin */
    UWORD d_xmax = d_xmin + blit_info->b_wd - 1;    /* dst Xmax = dst Xmin + width-1 */

    WORD s_xmin_off = s_xmin >> 4;           /* word offset to src Xmin */
    WORD s_xmax_off = s_xmax >> 4;           /* word offset to src Xmax */
    WORD s_span = s_xmax_off - s_xmin_off;   /* src span - 1 */

    WORD d_xmin_off = d_xmin >> 4;           /* word offset to dst Xmin */
    WORD d_xmax_off = d_xmax >> 4;           /* word offset to dst Xmax */
    WORD d_span = d_xmax_off - d_xmin_off;   /* dst span - 1 */

    /* Calculate starting addresses */
    char *s_addr = (char *)blit_info->s_form
        + (ULONG)blit_info->s_ymin * (ULONG)blit_info->s_nxln
        + (ULONG)s_xmin_off * (ULONG)blit_info->s_nxwd;
    char *d_addr = (char *)blit_info->d_form
        + (ULONG)blit_info->d_ymin * (ULONG)blit_info->d_nxln
        + (ULONG)d_xmin_off * (ULONG)blit_info->d_nxwd;

    /* BLiTTER register block */
    BLIT blitter;
    BLIT * restrict blt = &blitter;

    /* Number of words in dst line */
    blt->x_count = d_span + 1; /* set value in BLiTTER */
    blt->y_count = blit_info->b_ht; /* load the line count */

    /* offset between consecutive words in planes */
    blt->src_x_inc = blit_info->s_nxwd;
    blt->dst_x_inc = blit_info->d_nxwd;

    /* offset from last word of a line to first word of next one */
    blt->src_y_inc = blit_info->s_nxln - blt->src_x_inc * s_span;
    blt->dst_y_inc = blit_info->d_nxln - blt->dst_x_inc * d_span;

    WORD srcShiftX = s_xmin & 0xf, dstMinShiftX = d_xmin & 0xf, dstMaxShiftX = d_xmax & 0xf;
    
    /* Endmasks derived from dst Xmin mod 16 and dst Xmax mod 16 */
    blt->endmask_1 = 0xffff >> dstMinShiftX; /* first write mask */
    blt->endmask_2 = 0xffff; /* center mask */
    blt->endmask_3 = ~(0x7fff >> dstMaxShiftX); /* last write mask */

    WORD skew = dstMinShiftX - srcShiftX;
    bool directionInverted = false;
    
    if (s_addr < d_addr || (s_addr == d_addr && skew >= 0)) {
        // Start from lower right corner, so add width+length.
        s_addr = (char *)blit_info->s_form
            + (ULONG)blit_info->s_ymax * (ULONG)blit_info->s_nxln
            + (ULONG)s_xmax_off * (ULONG)blit_info->s_nxwd;
        d_addr = (char *)blit_info->d_form
            + (ULONG)blit_info->d_ymax * (ULONG)blit_info->d_nxln
            + (ULONG)d_xmax_off * (ULONG)blit_info->d_nxwd;

        // Invert directions.
        blt->src_x_inc = -blt->src_x_inc;
        blt->dst_x_inc = -blt->dst_x_inc;
        blt->src_y_inc = -blt->src_y_inc;
        blt->dst_y_inc = -blt->dst_y_inc;

        // Swap endmasks.
        UWORD endmaskTemp = blt->endmask_3;
        blt->endmask_3 = blt->endmask_1;
        blt->endmask_1 = endmaskTemp;
        
        directionInverted = true;
    }
    blt->src_addr = (UWORD*)s_addr;
    blt->dst_addr = (UWORD*)d_addr;

    // Does destination just span a single word ?
    if (!d_span)
        blt->endmask_1 &= blt->endmask_3; // Merge both end masks into single word end mask endmask1. The other end masks will be ignored by the BLiTTER.

    /*
     * Set up the skew byte, which contains the FXSR/NFSR flags and the
     * skew value.  The skew value is the low nybble of the difference
     * in Source and Destination alignment.
     *
     * The main complication is setting the FXSR/NFSR flags.  Normally
     * we use the calculated skew_idx to obtain them from the skew_flags[]
     * array.  However, when the source and destination widths are both 1,
     * we do not set either flag unless the skew value is zero, in which
     * case we set the FXSR flag only.  Additionally, we must set the skew
     * direction in source x incr.
     *
     * Thank you blitter hardware designers ...
     */
    if (!s_span && !d_span) {
        blt->src_x_inc = skew; /* sets skew direction */
        blt->skew = skew ? (skew & SKEW) : FXSR;
    } else {
        /*
         * Skew value is (destination Xmin mod 16 - source Xmin mod 16) & 0x000F.
         * Three main discriminators are used to determine the states of the skew
         * flags (FXSR and NFSR):
         *
         * bit 0     0: Source Xmin mod 16 =< Destination Xmin mod 16
         *           1: Source Xmin mod 16 >  Destination Xmin mod 16
         *
         * bit 1     0: SrcXmax/16-SrcXmin/16 <> DstXmax/16-DstXmin/16
         *                       Source span      Destination span
         *           1: SrcXmax/16-SrcXmin/16 == DstXmax/16-DstXmin/16
         *
         * bit 2     0: Blit direction is from Right to Left
         *           1: Blit direction is from Left to Right
         *
         * These form an offset into a skew flag table yielding FXSR and NFSR flag
         * states for the given source and destination alignments.
         *
         * NOTE: this table lookup is overridden for the special case when both
         * the source & destination widths are one, and the skew is 0.  For this
         * case, the FXSR flag alone is always set.
         */
        WORD skew_idx = 0x0000; /* default */
        if (d_span == s_span) /* the last discriminator is the equality of src and dst spans */
            skew_idx |= 0x0002; /* d6[bit1]:1 => equal spans */
        /* d7<- Dst Xmin mod16 - Src Xmin mod16 */
        if (skew < 0)
            skew_idx |= 0x0001; /* d6[bit0]<- alignment flag */
         if (!directionInverted)
            skew_idx |= 0x0004; /* blitting left->right */

        /* setting of skew flags */
        /* ---QUALIFIERS--- -ACTIONS-
         * dirn equal Sx&F>
         * L->R spans Dx&F  FXSR NFSR
         *  0     0     0     0    1  |..ssssssssssssss|ssssssssssssss..|
         *                            |......dddddddddd|dddddddddddddddd|dd..............|
         *  0     0     1     1    0  |......ssssssssss|ssssssssssssssss|ss..............|
         *                            |..dddddddddddddd|dddddddddddddd..|
         *  0     1     0     1    1  |..ssssssssssssss|ssssssssssssss..|
         *                            |...ddddddddddddd|ddddddddddddddd.|
         *  0     1     1     0    0  |...sssssssssssss|sssssssssssssss.|
         *                            |..dddddddddddddd|dddddddddddddd..|
         *  1     0     0     0    1  |..ssssssssssssss|ssssssssssssss..|
         *                            |......dddddddddd|dddddddddddddddd|dd..............|
         *  1     0     1     1    0  |......ssssssssss|ssssssssssssssss|ss..............|
         *                            |..dddddddddddddd|dddddddddddddd..|
         *  1     1     0     0    0  |..ssssssssssssss|ssssssssssssss..|
         *                            |...ddddddddddddd|ddddddddddddddd.|
         *  1     1     1     1    1  |...sssssssssssss|sssssssssssssss.|
         *                            |..dddddddddddddd|dddddddddddddd..|
         */
        static const UBYTE skew_flags[8] = {
                       /* for blit direction Right->Left */
            NFSR,          /* Source span < Destination span */
            FXSR,          /* Source span > Destination span */
            NFSR+FXSR,     /* Spans equal, Shift Source right */
            0,             /* Spans equal, Shift Source left */
                       /* for blit direction Left->Right */
            NFSR,          /* Source span < Destination span */
            FXSR,          /* Source span > Destination span */
            0,             /* Spans equal, Shift Source right */
            NFSR+FXSR      /* Spans equal, Shift Source left */
        };
        blt->skew = (skew & SKEW) | skew_flags[skew_idx];
    }

    blt->hop = HOP_SOURCE_ONLY; /* set HOP to source only */
    
    vdi_getDriver()->blitAll(blit_info, blt);
}

//********************************************************************************
// Driver.
//********************************************************************************
const vdi_Driver vdi_Soft_driver = {
    fillRectangle: vdi_Soft_fillRectangle,

    drawLine: vdi_Soft_drawLine,
    drawGeneralLine: vdi_Soft_drawGeneralLine,
    #if CONF_WITH_VDI_VERTLINE
    drawVerticalLine: vdi_Soft_drawVerticalLine,
    #else
    drawVerticalLine: vdi_Soft_drawGeneralLine,
    #endif
    #if CONF_WITH_VDI_HORILINE
    drawHorizontalLine: vdi_Soft_drawHorizontalLine,
    #else
    drawHorizontalLine: vdi_Soft_drawGeneralLine,
    #endif

    blit: vdi_Soft_blit,
    blitAll: vdi_Soft_blitAll,
    blitGeneral: vdi_Soft_blitGeneral,
    blitPlane: vdi_Soft_blitPlane,
};
