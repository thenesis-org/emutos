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

// Bit mask for 'standard' values of patternMask
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
static void vdi_Blitter_fillRectangle(vdi_DrawContext * RESTRICT dc) {
    vdi_Rect_sortCorners(&dc->rect);
    if (vdi_Rect_clip(&dc->rect, &dc->clipping.rect))
        return;

    vdi_FillingInfos fi;
    vdi_DrawContext_setupRectangle(dc, &fi);

    UBYTE * RESTRICT dst = fi.addr;
    LONG size = muls(fi.height, fi.stride);
    flush_data_cache(dst, size); // Flush the data cache to ensure that the screen memory is updated.

    BLITTER->src_x_inc = 0;
    BLITTER->endmask_1 = fi.leftMask;
    BLITTER->endmask_2 = 0xffff;
    BLITTER->endmask_3 = fi.rightMask;
    BLITTER->dst_x_inc = dc->planeNb * sizeof(UWORD);
    BLITTER->dst_y_inc = fi.stride - ((fi.wordNb - 1) << (vdi_planeNbToLeftShift[dc->planeNb] + 1));
    BLITTER->x_count = fi.wordNb;
    BLITTER->skew = 0;
    BLITTER->hop = HOP_HALFTONE_ONLY;

    UWORD patternMask = dc->pattern.mask;
    
    /*
     * Check for 'non-standard' values of patternMask:
     * - If multifill is set, patternMask must be 15
     * - If multifill is *not* set, patternMask must be 0, 1, 3, 7, or 15
     * If we have a non-standard value, we call a separate function.
     */
    bool patternNonStandard = false;
    if (dc->multiFill) {
        if (patternMask != 15)
            patternNonStandard = true;
    } else {
        if (patternMask >= 16 || (vdi_Blitter_standardPatternMask & (1u << patternMask)) == 0)
            patternNonStandard = true;
    }
    
    {
        const UWORD *patternData = dc->pattern.data;
        const UBYTE *blitterOp = vdi_Blitter_op[dc->mode];
        UWORD * RESTRICT dstPlane = (UWORD * RESTRICT)dst;
        UWORD color = dc->color;
        if (patternNonStandard) {
            /*
             * Handle non-standard values of patternMask.
             * We do a line-at-a-time within the normal plane-at-a-time loop.
             */
            UWORD patternLineIndex = dc->rect.y1 & patternMask;
            LOOP_DO(plane, dc->planeNb) {
                BLITTER->dst_addr = dstPlane++;
                BLITTER->op = blitterOp[color & 1];
                LOOP_DO(y, fi.height) {
                    BLITTER->halftone[0] = patternData[patternLineIndex++];
                    if (patternLineIndex > patternMask) // patternMask can be a non power of 2 in this function.
                        patternLineIndex = 0;
                    BLITTER->y_count = 1;
                    vdi_Blitter_wait(0);
                } LOOP_WHILE(y);
                if (dc->multiFill)
                    patternData += 16;                   
                color >>= 1;
            } LOOP_WHILE(plane);
        } else {
            if (!dc->multiFill) { /* only need to init halftone once */
                UWORD * RESTRICT p = BLITTER->halftone;
                for (WORD i = 0; i < 16; i++)
                    p[i] = patternData[i & patternMask];
            }

            {
                UBYTE startLine = dc->rect.y1 & LINENO;
                LOOP_DO(plane, dc->planeNb) {
                    if (dc->multiFill) { /* need to init halftone each time */
                        /* more efficient here because patternMask must be 15 */
                        UWORD * RESTRICT p = BLITTER->halftone;
                        for (WORD i = 0; i < 16; i++) // TODO: Use a movem ?
                            *p++ = *patternData++;
                    }
                    BLITTER->dst_addr = dstPlane++;
                    BLITTER->y_count = fi.height;
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
static void vdi_Blitter_drawVerticalLine(vdi_DrawContext * RESTRICT dc) {
    vdi_FillingInfos fi;
    if (vdi_DrawContext_setupVerticalLine(dc, &fi))
        return;

    WORD startLine;
    UBYTE hop;
    UWORD lineMask = dc->line.mask;
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
        dc->line.mask = lineMask;
        hop = HOP_HALFTONE_ONLY;
    }

    UBYTE *dst = fi.addr;
    LONG size = muls(fi.height, fi.stride);
    if (size < 0) {
        dst += size;
        size = -size;
    }
    flush_data_cache(dst, size);

    BLITTER->endmask_1 = 0x8000 >> (dc->line.line.x1 & 0x000f);
    BLITTER->endmask_2 = 0x0000;
    BLITTER->endmask_3 = 0x0000;
    BLITTER->dst_y_inc = fi.stride;
    BLITTER->x_count = 1;
    BLITTER->hop = hop;
    BLITTER->skew = 0;

    UWORD color = dc->color;
    const UBYTE *blitterOp = vdi_Blitter_op[dc->mode];
    UWORD *dstPlane = (UWORD*)fi.addr;
    LOOP_DO(planeIndex, dc->planeNb) {
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

static void vdi_Blitter_drawHorizontalLine(vdi_DrawContext * RESTRICT dc) {
    vdi_FillingInfos fi;
    if (vdi_DrawContext_setupHorizontalLine(dc, &fi))
        return;
    
    flush_data_cache(fi.addr, fi.stride);

    BLITTER->src_x_inc = 0;
    UWORD lineMask = dc->line.mask;
    BLITTER->endmask_1 = fi.leftMask & lineMask;
    BLITTER->endmask_2 = lineMask;
    BLITTER->endmask_3 = fi.rightMask & lineMask;
    rolw(lineMask, fi.width & 0xf);
    dc->line.mask = lineMask;
    BLITTER->dst_x_inc = dc->planeNb  << 1;
    BLITTER->dst_y_inc = fi.stride - ((fi.wordNb - 1) << (vdi_planeNbToLeftShift[dc->planeNb] + 1));
    BLITTER->x_count = fi.wordNb;
    BLITTER->skew = 0;
    BLITTER->hop = HOP_ALL_ONES;
   
    UWORD color = dc->color;
    const UBYTE *blitterOp = vdi_Blitter_op[dc->mode];
    UWORD *dstPlane = fi.addr;
    LOOP_DO(plane, dc->planeNb) {
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
    fillSpan: vdi_Blitter_fillRectangle,

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
    
    fillPolygonSpan: vdi_Soft_fillPolygonSpan,
    fillPolygon: vdi_Soft_fillPolygon,

    fillDisk: vdi_Soft_fillDisk,

    seedFill: vdi_Soft_seedFill,

    blit: vdi_Soft_blit, // Software routine for blitting setup.
    blitAll: vdi_Soft_blitGeneral, // Same as blitGeneral because we do not use specially optimized code.
    blitGeneral: vdi_Soft_blitGeneral, // Not used in practice.
    blitPlane: vdi_Blitter_blitPlane,
};

#endif
