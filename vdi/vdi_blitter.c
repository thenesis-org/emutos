#include "config.h"

#if CONF_WITH_BLITTER

#include "asm.h"
#include "biosext.h" // For cache control routines.
#include "intmath.h"
#include "vdi/vdi_internal.h"

//--------------------------------------------------------------------------------
// Common tools.
//--------------------------------------------------------------------------------
/*
 * Since the blitter doesn't see the data cache, and we may be in
 * copyback mode (e.g. the FireBee), we must flush the data cache
 * first to ensure that the screen memory is current.  the length
 * below should be correct, but note that the current cache control
 * routines ignore the length specification & act on the whole cache
 * anyway.
 */

// Bit mask for 'standard' values of patmsk
#define vdi_Blitter_standardPatternMask ((1u<<15) | (1u<<7) | (1u<<3) | (1u<<1) | (1u<<0))

// Blitter ops for draw/nodraw cases for wrt_mode 0-3.
static const UBYTE vdi_Blitter_op[4][2] = { { 0x00, 0x03 }, { 0x04, 0x07}, { 0x06, 0x06 }, { 0x01, 0x0d } };

// The blitter is run in the Atari-recommended way: use no-HOG mode, and manually restart the blitter until it's done.
forceinline void vdi_Blitter_wait(WORD startLine) {
    BLITTER->status = BUSY | startLine; /* no-HOG mode */
    __asm__ __volatile__(
    "lea    0xffff8a3c,a0\n\t"
    "0:\n\t"
    "tas    (a0)\n\t"
    "nop\n\t"
    "jbmi   0b\n\t"
    :
    :
    : "a0", "memory", "cc"
    );
}

//--------------------------------------------------------------------------------
// Rectangle filling.
//--------------------------------------------------------------------------------
OPTIMIZE_O3
static void vdi_Blitter_fillRectangle(const vdi_FillingInfos * RESTRICT fi, const VwkAttrib * RESTRICT attr) {
    UBYTE * RESTRICT dst = fi->addr;
    LONG size = muls(fi->height, fi->stride);
    flush_data_cache(dst, size); // Flush the data cache to ensure that the screen memory is updated.

    BLITTER->src_x_inc = 0;
    BLITTER->endmask_1 = fi->leftMask;
    BLITTER->endmask_2 = 0xffff;
    BLITTER->endmask_3 = fi->rightMask;
    BLITTER->dst_x_inc = fi->planeNb * sizeof(WORD);
    BLITTER->dst_y_inc = fi->stride - ((fi->wordNb - 1) << (vdi_planeNbToLeftShift[fi->planeNb] + 1));
    BLITTER->x_count = fi->wordNb;
    BLITTER->skew = 0;
    BLITTER->hop = HOP_HALFTONE_ONLY;

    UWORD patmsk = attr->patmsk;
    
    /*
     * Check for 'non-standard' values of patmsk:
     * - If multifill is set, patmsk must be 15
     * - If multifill is *not* set, patmsk must be 0, 1, 3, 7, or 15
     * If we have a non-standard value, we call a separate function.
     */
    bool nonstd = false;
    if (attr->multifill) {
        if (patmsk != 15)
            nonstd = true;
    } else {
        if (patmsk >= 16 || (vdi_Blitter_standardPatternMask & (1u << patmsk)) == 0)
            nonstd = true;
    }
    
    {
        const UWORD *patptr = attr->patptr;
        const UBYTE *blitterOp = vdi_Blitter_op[attr->wrt_mode];
        UWORD * RESTRICT dstPlane = (UWORD * RESTRICT)dst;
        UWORD color = attr->color;
        if (nonstd) {
            /*
             * Handle non-standard values of patmsk.
             * We do a line-at-a-time within the normal plane-at-a-time loop.
             */
            UWORD patindex = fi->y1 & patmsk;
            LOOP_DO(plane, fi->planeNb) {
                BLITTER->dst_addr = dstPlane++;
                BLITTER->op = blitterOp[color & 1];
                LOOP_DO(y, fi->height) {
                    BLITTER->halftone[0] = patptr[patindex++];
                    if (patindex > patmsk) // patmsk can be a non power of 2 in this function.
                        patindex = 0;
                    BLITTER->y_count = 1;
                    vdi_Blitter_wait(0);
                } LOOP_WHILE(y);
                if (attr->multifill)
                    patptr += 16;                   
                color >>= 1;
            } LOOP_WHILE(plane);
        } else {
            if (!attr->multifill) { /* only need to init halftone once */
                UWORD * RESTRICT p = BLITTER->halftone;
                for (WORD i = 0; i < 16; i++)
                    p[i] = patptr[i & patmsk];
            }

            {
                UBYTE startLine = fi->y1 & LINENO;
                LOOP_DO(plane, fi->planeNb) {
                    if (attr->multifill) { /* need to init halftone each time */
                        /* more efficient here because patmsk must be 15 */
                        UWORD * RESTRICT p = BLITTER->halftone;
                        for (WORD i = 0; i < 16; i++) // TODO: Use a movem ?
                            *p++ = *patptr++;
                    }
                    BLITTER->dst_addr = dstPlane++;
                    BLITTER->y_count = fi->height;
                    BLITTER->op = blitterOp[color & 1];
                    vdi_Blitter_wait(startLine);
                    color >>= 1;
                } LOOP_WHILE(plane);
            }       
        }
    }
    
    invalidate_data_cache(dst, size); // Invalidate any cached screen data.
}

//--------------------------------------------------------------------------------
// Vertical line.
//--------------------------------------------------------------------------------
#if CONF_WITH_VDI_VERTLINE

// The setup time is so high that is it worth using the blitter for a few words ?
static void vdi_Blitter_drawVerticalLine(const Line * RESTRICT line, WORD mode, UWORD color, bool lastLineFlag) {
    vdi_FillingInfos fi;
    if (vdi_setupVerticalLine(line, lastLineFlag, mode, &fi))
        return;

    WORD startLine;
    UWORD lineMask = lineaVars.line_mask;
    UBYTE hop;
    if (lineMask == 0xffff) {
        startLine = 0;
        hop = HOP_ALL_ONES;
    } else {
        if (fi.stride < 0) {
            UWORD mask = 0x0001;
            for (WORD i = 0; i < 16; i++, mask <<= 1)
                BLITTER->halftone[i] = (lineMask & mask) ? 0xffff : 0x0000;
            startLine = 15;
        } else {
            UWORD mask = 0x8000;
            for (WORD i = 0; i < 16; i++, mask >>= 1)
                BLITTER->halftone[i] = (lineMask & mask) ? 0xffff : 0x0000;
            startLine = 0;
        }
        rolw(lineMask, fi.height & 0xf);
        lineaVars.line_mask = lineMask; // Update lineaVars.line_mask for next time.
        hop = HOP_HALFTONE_ONLY;
    }

    UBYTE *dst = fi.addr;
    LONG size = muls(fi.height, fi.stride);
    if (size < 0) {
        dst += size;
        size = -size;
    }
    flush_data_cache(dst, size);

    BLITTER->endmask_1 = 0x8000 >> (line->x1 & 0x000f);
    BLITTER->endmask_2 = 0x0000;
    BLITTER->endmask_3 = 0x0000;
    BLITTER->dst_y_inc = fi.stride;
    BLITTER->x_count = 1;
    BLITTER->hop = hop;
    BLITTER->skew = 0;

    const UBYTE *blitterOp = vdi_Blitter_op[mode];
    UWORD *dstPlane = (UWORD*)fi.addr;
    LOOP_DO(planeIndex, fi.planeNb) {
        BLITTER->dst_addr = dstPlane++;
        BLITTER->y_count = fi.height;
        BLITTER->op = blitterOp[color & 1];
        color >>= 1;
        vdi_Blitter_wait(startLine);
    } LOOP_WHILE(planeIndex);
    
    invalidate_data_cache(dst, size);
}

#endif

//--------------------------------------------------------------------------------
// Horizontal line.
//--------------------------------------------------------------------------------
#if CONF_WITH_VDI_HORILINE

static void vdi_Blitter_drawHorizontalLine(const Line * RESTRICT line, WORD mode, UWORD color, bool lastLineFlag) {
    vdi_FillingInfos fi;
    if (vdi_setupHorizontalLine(line, lastLineFlag, mode, &fi))
        return;
    
    flush_data_cache(fi.addr, fi.stride);

    UWORD lineMask = lineaVars.line_mask;
    BLITTER->src_x_inc = 0;
    BLITTER->endmask_1 = fi.leftMask & lineMask;
    BLITTER->endmask_2 = lineMask;
    BLITTER->endmask_3 = fi.rightMask & lineMask;
    BLITTER->dst_x_inc = fi.planeNb  << 1;
    BLITTER->dst_y_inc = fi.stride - ((fi.wordNb - 1) << (vdi_planeNbToLeftShift[fi.planeNb] + 1));
    BLITTER->x_count = fi.wordNb;
    BLITTER->skew = 0;
    BLITTER->hop = HOP_ALL_ONES;
   
    const UBYTE *blitterOp = vdi_Blitter_op[mode];
    UWORD *dstPlane = fi.addr;
    LOOP_DO(plane, fi.planeNb) {
        BLITTER->dst_addr = dstPlane++;
        BLITTER->y_count = 1;
        BLITTER->op = blitterOp[color & 1];
        vdi_Blitter_wait(0);
        color >>= 1;
    } LOOP_WHILE(plane);
       
    invalidate_data_cache(fi.addr, fi.stride);
}

#endif

//--------------------------------------------------------------------------------
// Blitting.
//--------------------------------------------------------------------------------
static void vdi_Blitter_blitPlane(BLIT * RESTRICT blt) {
    LONG size = muls(blt->y_count, blt->dst_y_inc) + muls(blt->x_count, blt->dst_x_inc);
    void *dst = (void *)blt->dst_addr;
    flush_data_cache(dst, size);

    BLITTER->src_x_inc = blt->src_x_inc;
    BLITTER->src_y_inc = blt->src_y_inc;
    BLITTER->src_addr = blt->src_addr;
    BLITTER->endmask_1 = blt->endmask_1;
    BLITTER->endmask_2 = blt->endmask_2;
    BLITTER->endmask_3 = blt->endmask_3;
    BLITTER->dst_x_inc = blt->dst_x_inc;
    BLITTER->dst_y_inc = blt->dst_y_inc;
    BLITTER->dst_addr = blt->dst_addr;
    BLITTER->x_count = blt->x_count;
    BLITTER->y_count = blt->y_count;
    BLITTER->op = blt->op;
    BLITTER->hop = blt->hop;
    BLITTER->skew = blt->skew;
    vdi_Blitter_wait(0);

    invalidate_data_cache(dst, size);
}

//--------------------------------------------------------------------------------
// Driver.
//--------------------------------------------------------------------------------
const vdi_Driver vdi_Blitter_driver = {
    fillRectangle: vdi_Blitter_fillRectangle,
    
    drawLine: vdi_Soft_drawLine,
    drawGeneralLine: vdi_Soft_drawGeneralLine,
    #if CONF_WITH_VDI_VERTLINE
    drawVerticalLine: vdi_Blitter_drawVerticalLine,
    #else
    drawVerticalLine: vdi_Soft_drawGeneralLine,
    #endif
    #if CONF_WITH_VDI_HORILINE
    drawHorizontalLine: vdi_Blitter_drawHorizontalLine,
    #else
    drawHorizontalLine: vdi_Soft_drawGeneralLine,
    #endif
    
    blit: vdi_Soft_blit, // Software routine for blitting setup.
    blitAll: vdi_Soft_blitGeneral, // Same as blitGeneral because we do not use specially optimized code.
    blitGeneral: vdi_Soft_blitGeneral, // Not used in practice.
    blitPlane: vdi_Blitter_blitPlane,
};

#endif
