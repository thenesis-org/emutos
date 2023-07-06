/*
 * VDI.
 *
 * Copyright 1982 by Digital Research Inc.  All rights reserved.
 * Copyright 1999 by Caldera, Inc. and Authors:
 * Copyright (C) 2002-2020 The EmuTOS development team
 *
 * This file is distributed under the GPL, version 2 or at your
 * option any later version.  See doc/license.txt for details.
 */
#include "emutos.h"
#include "bdosbind.h"
#include "blitter.h"
#include "biosbind.h"
#include "biosext.h"
#include "xbiosbind.h"
#include "vdi/vdi_internal.h"
#include "obdefs.h"
#include "has.h" /* for blitter-related items */
#include "bios/screen.h"
#include "string.h"
#if WITH_AES
// FIXME: Remove dependency to AES.
#include "../aes/aesstub.h"
#endif

/*
 * Mxalloc() mode used when allocating the virtual workstation.  This
 * is only significant when running under FreeMiNT, since EmuTOS ignores
 * these bits of the mode field.
 */
#define MX_SUPER (3<<4)

//********************************************************************************
// Tools.
//********************************************************************************
//--------------------------------------------------------------------------------
// Geometric tools.
//--------------------------------------------------------------------------------
bool vdi_ClipRect_clipSingleCoordinate(WORD xMin, WORD xMax, WORD *x) {
    WORD x0 = x[0], x1 = x[1];
    if (x0 > xMax || x1 < xMin)
        return true;
    if (x0 < xMin)
        x[0] = xMin;
    if (x1 > xMax)
        x[1] = xMax;
    return false;
}

void vdi_Rect_sortCorners(Rect *rect) {
    vdi_Rect_sortX(rect);
    vdi_Rect_sortY(rect);
}

bool vdi_Rect_clip(Rect * RESTRICT rect, const ClippingRect * RESTRICT clip) {
    for (WORD i = 0; i < 2; i++) {
        WORD x0 = rect->p[0].v[i], x1 = rect->p[1].v[i];
        WORD xMin = clip->p[0].v[i], xMax = clip->p[1].v[i];
        if (x0 > xMax || x1 < xMin)
            return true;
        if (x0 < xMin)
            rect->p[0].v[i] = xMin;
        if (x1 > xMax)
            rect->p[1].v[i] = xMax;
    }
    return false;
}

void vdi_Line_sortVertices(Line *line) {
    if (line->x1 > line->x2) {
        WORD temp = line->x1;
        line->x1 = line->x2;
        line->x2 = temp;
    }
    if (line->y1 < line->y2) {
        WORD temp = line->y1;
        line->y1 = line->y2;
        line->y2 = temp;
    }
}

//********************************************************************************
// VDI.
//********************************************************************************
//--------------------------------------------------------------------------------
// Shared buffer.
//--------------------------------------------------------------------------------
vdi_SharedBuffer vdi_sharedBuffer;

//--------------------------------------------------------------------------------
// Drawing tools.
//--------------------------------------------------------------------------------
// Precomputed value of log2(8/lineaVars.screen_planeNb), used to derive vdi_context.planeNbShift.
// Only the indexes 1, 2, 4 and 8 are meaningful.
const UBYTE vdi_planeNbToRightShift[9] = { 0, 3, 2, 0, 1, 0, 0, 0, 0 };

const UBYTE vdi_planeNbToLeftShift[9] = { 0, 0, 1, 0, 2, 0, 0, 0, 3 };

UWORD * vdi_getPixelAddress(WORD x, WORD y) {
    WORD x2 = x & 0xfff0; /* ensure that value to be shifted remains signed! */
    UBYTE *addr = v_bas_ad; /* start of screen */
    addr += x2 >> vdi_context.planeNbShift; /* add x coordinate part of addr */
    addr += muls(y, lineaVars.screen_lineSize2); /* add y coordinate part of addr */
    return (UWORD*)addr;
}

const uint8_t vdi_writingModeToOpTable[4*4 + 16*4]= {
//  00  01  10  11  fore/back
    // VDI modes
    0x0,0x0,0x3,0x3, // replace mode
    0x4,0x4,0x7,0x7, // transparent mode
    0x6,0x6,0x6,0x6, // XOR mode
    0x1,0x1,0xd,0xd, // inverse transparent mode
    // BitBlt modes
    0x0,0xf,0x0,0xf, // mode 0: all zeros
    0x0,0xe,0x1,0xf, // mode 1: source AND destination
    0x0,0xd,0x2,0xf, // mode 2: source AND (NOT destination)
    0x0,0xc,0x3,0xf, // mode 3: source
    0x0,0xb,0x4,0xf, // mode 4: (NOT source) AND destination
    0x0,0xa,0x5,0xf, // mode 5: destination
    0x0,0x9,0x6,0xf, // mode 6: source XOR destination
    0x0,0x8,0x7,0xf, // mode 7: source OR destination
    0x0,0x7,0x8,0xf, // mode 8: (NOT source) AND (NOT destination)
    0x0,0x6,0x9,0xf, // mode 9: (NOT source) XOR destination
    0x0,0x5,0xa,0xf, // mode A: NOT destination
    0x0,0x4,0xb,0xf, // mode B: source or (NOT destination)
    0x0,0x3,0xc,0xf, // mode C: NOT source
    0x0,0x2,0xd,0xf, // mode D: (NOT source) OR destination
    0x0,0x1,0xe,0xf, // mode E: (NOT source) OR (NOT destination)
    0x0,0x0,0xf,0xf, // mode F: all ones
};

static const UWORD vdi_fringeTable[16 + 1] = {
    0xFFFF, // lf_tab: origin for not left fringe lookup.
    0x7FFF, // rf_tab: origin for right fringe lookup.
    0x3FFF,
    0x1FFF,
    0x0FFF,
    0x07FF,
    0x03FF,
    0x01FF,
    0x00FF,
    0x007F,
    0x003F,
    0x001F,
    0x000F,
    0x0007,
    0x0003,
    0x0001,
    0x0000
};

//--------------------------------------------------------------------------------
// Palette.
//--------------------------------------------------------------------------------
static const WORD vdi_Palette_penToPaletteDefaultTable[] = { 0, 15, 1, 2, 4, 6, 3, 5, 7, 8, 9, 10, 12, 14, 11, 13 };

// The following factors are used in vdi_Palette_adjustMonoValues().
#define vdi_Palette_steMonoFudgeFactor   0x43
#define vdi_Palette_stMonoFudgeFactor    0x8e

/* Initial color palettes */
static const WORD vdi_Palette_defaultStPalette[16][3] =
{
    { 1000, 1000, 1000 },
    { 0, 0, 0 },
    { 1000, 0, 0 },
    { 0, 1000, 0 },
    { 0, 0, 1000 },
    { 0, 1000, 1000 },
    { 1000, 1000, 0 },
    { 1000, 0, 1000 },
    { 714, 714, 714 },
    { 428, 428, 428 },
    { 1000, 428, 428 },
    { 428, 1000, 428 },
    { 428, 428, 1000 },
    { 428, 1000, 1000 },
    { 1000, 1000, 428 },
    { 1000, 428, 1000 }
};
#if CONF_WITH_TT_SHIFTER
static const WORD vdi_Palette_defaultTtPalette1[16][3] =
{
    { 1000, 1000, 1000 }, { 0, 0, 0 }, { 1000, 0, 0 }, { 0, 1000, 0 },
    { 0, 0, 1000 }, { 0, 1000, 1000 }, { 1000, 1000, 0 }, { 1000, 0, 1000 },
    { 667, 667, 667 }, { 400, 400, 400 }, { 1000, 600, 600 }, { 600, 1000, 600 },
    { 600, 600, 1000 }, { 600, 1000, 1000 }, { 1000, 1000, 600 }, { 1000, 600, 1000 }
};
static const WORD vdi_Palette_defaultTtPalette2[240][3] =
{
    { 1000, 1000, 1000 }, { 933, 933, 933 }, { 867, 867, 867 }, { 800, 800, 800 },
    { 733, 733, 733 }, { 667, 667, 667 }, { 600, 600, 600 }, { 533, 533, 533 },
    { 467, 467, 467 }, { 400, 400, 400 }, { 333, 333, 333 }, { 267, 267, 267 },
    { 200, 200, 200 }, { 133, 133, 133 }, { 67, 67, 67 }, { 0, 0, 0 },
    { 1000, 0, 0 }, { 1000, 0, 67 }, { 1000, 0, 133 }, { 1000, 0, 200 },
    { 1000, 0, 267 }, { 1000, 0, 333 }, { 1000, 0, 400 }, { 1000, 0, 467 },
    { 1000, 0, 533 }, { 1000, 0, 600 }, { 1000, 0, 667 }, { 1000, 0, 733 },
    { 1000, 0, 800 }, { 1000, 0, 867 }, { 1000, 0, 933 }, { 1000, 0, 1000 },
    { 933, 0, 1000 }, { 867, 0, 1000 }, { 800, 0, 1000 }, { 733, 0, 1000 },
    { 667, 0, 1000 }, { 600, 0, 1000 }, { 533, 0, 1000 }, { 467, 0, 1000 },
    { 400, 0, 1000 }, { 333, 0, 1000 }, { 267, 0, 1000 }, { 200, 0, 1000 },
    { 133, 0, 1000 }, { 67, 0, 1000 }, { 0, 0, 1000 }, { 0, 67, 1000 },
    { 0, 133, 1000 }, { 0, 200, 1000 }, { 0, 267, 1000 }, { 0, 333, 1000 },
    { 0, 400, 1000 }, { 0, 467, 1000 }, { 0, 533, 1000 }, { 0, 600, 1000 },
    { 0, 667, 1000 }, { 0, 733, 1000 }, { 0, 800, 1000 }, { 0, 867, 1000 },
    { 0, 933, 1000 }, { 0, 1000, 1000 }, { 0, 1000, 933 }, { 0, 1000, 867 },
    { 0, 1000, 800 }, { 0, 1000, 733 }, { 0, 1000, 667 }, { 0, 1000, 600 },
    { 0, 1000, 533 }, { 0, 1000, 467 }, { 0, 1000, 400 }, { 0, 1000, 333 },
    { 0, 1000, 267 }, { 0, 1000, 200 }, { 0, 1000, 133 }, { 0, 1000, 67 },
    { 0, 1000, 0 }, { 67, 1000, 0 }, { 133, 1000, 0 }, { 200, 1000, 0 },
    { 267, 1000, 0 }, { 333, 1000, 0 }, { 400, 1000, 0 }, { 467, 1000, 0 },
    { 533, 1000, 0 }, { 600, 1000, 0 }, { 667, 1000, 0 }, { 733, 1000, 0 },
    { 800, 1000, 0 }, { 867, 1000, 0 }, { 933, 1000, 0 }, { 1000, 1000, 0 },
    { 1000, 933, 0 }, { 1000, 867, 0 }, { 1000, 800, 0 }, { 1000, 733, 0 },
    { 1000, 667, 0 }, { 1000, 600, 0 }, { 1000, 533, 0 }, { 1000, 467, 0 },
    { 1000, 400, 0 }, { 1000, 333, 0 }, { 1000, 267, 0 }, { 1000, 200, 0 },
    { 1000, 133, 0 }, { 1000, 67, 0 }, { 733, 0, 0 }, { 733, 0, 67 },
    { 733, 0, 133 }, { 733, 0, 200 }, { 733, 0, 267 }, { 733, 0, 333 },
    { 733, 0, 400 }, { 733, 0, 467 }, { 733, 0, 533 }, { 733, 0, 600 },
    { 733, 0, 667 }, { 733, 0, 733 }, { 667, 0, 733 }, { 600, 0, 733 },
    { 533, 0, 733 }, { 467, 0, 733 }, { 400, 0, 733 }, { 333, 0, 733 },
    { 267, 0, 733 }, { 200, 0, 733 }, { 133, 0, 733 }, { 67, 0, 733 },
    { 0, 0, 733 }, { 0, 67, 733 }, { 0, 133, 733 }, { 0, 200, 733 },
    { 0, 267, 733 }, { 0, 333, 733 }, { 0, 400, 733 }, { 0, 467, 733 },
    { 0, 533, 733 }, { 0, 600, 733 }, { 0, 667, 733 }, { 0, 733, 733 },
    { 0, 733, 667 }, { 0, 733, 600 }, { 0, 733, 533 }, { 0, 733, 467 },
    { 0, 733, 400 }, { 0, 733, 333 }, { 0, 733, 267 }, { 0, 733, 200 },
    { 0, 733, 133 }, { 0, 733, 67 }, { 0, 733, 0 }, { 67, 733, 0 },
    { 133, 733, 0 }, { 200, 733, 0 }, { 267, 733, 0 }, { 333, 733, 0 },
    { 400, 733, 0 }, { 467, 733, 0 }, { 533, 733, 0 }, { 600, 733, 0 },
    { 667, 733, 0 }, { 733, 733, 0 }, { 733, 667, 0 }, { 733, 600, 0 },
    { 733, 533, 0 }, { 733, 467, 0 }, { 733, 400, 0 }, { 733, 333, 0 },
    { 733, 267, 0 }, { 733, 200, 0 }, { 733, 133, 0 }, { 733, 67, 0 },
    { 467, 0, 0 }, { 467, 0, 67 }, { 467, 0, 133 }, { 467, 0, 200 },
    { 467, 0, 267 }, { 467, 0, 333 }, { 467, 0, 400 }, { 467, 0, 467 },
    { 400, 0, 467 }, { 333, 0, 467 }, { 267, 0, 467 }, { 200, 0, 467 },
    { 133, 0, 467 }, { 67, 0, 467 }, { 0, 0, 467 }, { 0, 67, 467 },
    { 0, 133, 467 }, { 0, 200, 467 }, { 0, 267, 467 }, { 0, 333, 467 },
    { 0, 400, 467 }, { 0, 467, 467 }, { 0, 467, 400 }, { 0, 467, 333 },
    { 0, 467, 267 }, { 0, 467, 200 }, { 0, 467, 133 }, { 0, 467, 67 },
    { 0, 467, 0 }, { 67, 467, 0 }, { 133, 467, 0 }, { 200, 467, 0 },
    { 267, 467, 0 }, { 333, 467, 0 }, { 400, 467, 0 }, { 467, 467, 0 },
    { 467, 400, 0 }, { 467, 333, 0 }, { 467, 267, 0 }, { 467, 200, 0 },
    { 467, 133, 0 }, { 467, 67, 0 }, { 267, 0, 0 }, { 267, 0, 67 },
    { 267, 0, 133 }, { 267, 0, 200 }, { 267, 0, 267 }, { 200, 0, 267 },
    { 133, 0, 267 }, { 67, 0, 267 }, { 0, 0, 267 }, { 0, 67, 267 },
    { 0, 133, 267 }, { 0, 200, 267 }, { 0, 267, 267 }, { 0, 267, 200 },
    { 0, 267, 133 }, { 0, 267, 67 }, { 0, 267, 0 }, { 67, 267, 0 },
    { 133, 267, 0 }, { 200, 267, 0 }, { 267, 267, 0 }, { 267, 200, 0 },
    { 267, 133, 0 }, { 267, 67, 0 }, { 1000, 1000, 1000 }, { 0, 0, 0 }
};
#endif
#if CONF_WITH_VIDEL
static const WORD vdi_Palette_defaultVidelPalette1[16][3] =
{
    { 1000, 1000, 1000 }, { 0, 0, 0 }, { 1000, 0, 0 }, { 0, 1000, 0 },
    { 0, 0, 1000 }, { 0, 1000, 1000 }, { 1000, 1000, 0 }, { 1000, 0, 1000 },
    { 733, 733, 733 }, { 533, 533, 533 }, { 667, 0, 0 }, { 0, 667, 0 },
    { 0, 0, 667 }, { 0, 667, 667 }, { 667, 667, 0 }, { 667, 0, 667 }
};
static const WORD vdi_Palette_defaultVidelPalette2[240][3] =
{
    { 1000, 1000, 1000 }, { 933, 933, 933 }, { 867, 867, 867 }, { 800, 800, 800 },
    { 733, 733, 733 }, { 667, 667, 667 }, { 600, 600, 600 }, { 533, 533, 533 },
    { 467, 467, 467 }, { 400, 400, 400 }, { 333, 333, 333 }, { 267, 267, 267 },
    { 200, 200, 200 }, { 133, 133, 133 }, { 67, 67, 67 }, { 0, 0, 0 },
    { 1000, 0, 0 }, { 1000, 0, 67 }, { 1000, 0, 133 }, { 1000, 0, 200 },
    { 1000, 0, 267 }, { 1000, 0, 333 }, { 1000, 0, 400 }, { 1000, 0, 467 },
    { 1000, 0, 533 }, { 1000, 0, 600 }, { 1000, 0, 667 }, { 1000, 0, 733 },
    { 1000, 0, 800 }, { 1000, 0, 867 }, { 1000, 0, 933 }, { 1000, 0, 1000 },
    { 933, 0, 1000 }, { 867, 0, 1000 }, { 800, 0, 1000 }, { 733, 0, 1000 },
    { 667, 0, 1000 }, { 600, 0, 1000 }, { 533, 0, 1000 }, { 467, 0, 1000 },
    { 400, 0, 1000 }, { 333, 0, 1000 }, { 267, 0, 1000 }, { 200, 0, 1000 },
    { 133, 0, 1000 }, { 67, 0, 1000 }, { 0, 0, 1000 }, { 0, 67, 1000 },
    { 0, 133, 1000 }, { 0, 200, 1000 }, { 0, 267, 1000 }, { 0, 333, 1000 },
    { 0, 400, 1000 }, { 0, 467, 1000 }, { 0, 533, 1000 }, { 0, 600, 1000 },
    { 0, 667, 1000 }, { 0, 733, 1000 }, { 0, 800, 1000 }, { 0, 867, 1000 },
    { 0, 933, 1000 }, { 0, 1000, 1000 }, { 0, 1000, 933 }, { 0, 1000, 867 },
    { 0, 1000, 800 }, { 0, 1000, 733 }, { 0, 1000, 667 }, { 0, 1000, 600 },
    { 0, 1000, 533 }, { 0, 1000, 467 }, { 0, 1000, 400 }, { 0, 1000, 333 },
    { 0, 1000, 267 }, { 0, 1000, 200 }, { 0, 1000, 133 }, { 0, 1000, 67 },
    { 0, 1000, 0 }, { 67, 1000, 0 }, { 133, 1000, 0 }, { 200, 1000, 0 },
    { 267, 1000, 0 }, { 333, 1000, 0 }, { 400, 1000, 0 }, { 467, 1000, 0 },
    { 533, 1000, 0 }, { 600, 1000, 0 }, { 667, 1000, 0 }, { 733, 1000, 0 },
    { 800, 1000, 0 }, { 867, 1000, 0 }, { 933, 1000, 0 }, { 1000, 1000, 0 },
    { 1000, 933, 0 }, { 1000, 867, 0 }, { 1000, 800, 0 }, { 1000, 733, 0 },
    { 1000, 667, 0 }, { 1000, 600, 0 }, { 1000, 533, 0 }, { 1000, 467, 0 },
    { 1000, 400, 0 }, { 1000, 333, 0 }, { 1000, 267, 0 }, { 1000, 200, 0 },
    { 1000, 133, 0 }, { 1000, 67, 0 }, { 733, 0, 0 }, { 733, 0, 67 },
    { 733, 0, 133 }, { 733, 0, 200 }, { 733, 0, 267 }, { 733, 0, 333 },
    { 733, 0, 400 }, { 733, 0, 467 }, { 733, 0, 533 }, { 733, 0, 600 },
    { 733, 0, 667 }, { 733, 0, 733 }, { 667, 0, 733 }, { 600, 0, 733 },
    { 533, 0, 733 }, { 467, 0, 733 }, { 400, 0, 733 }, { 333, 0, 733 },
    { 267, 0, 733 }, { 200, 0, 733 }, { 133, 0, 733 }, { 67, 0, 733 },
    { 0, 0, 733 }, { 0, 67, 733 }, { 0, 133, 733 }, { 0, 200, 733 },
    { 0, 267, 733 }, { 0, 333, 733 }, { 0, 400, 733 }, { 0, 467, 733 },
    { 0, 533, 733 }, { 0, 600, 733 }, { 0, 667, 733 }, { 0, 733, 733 },
    { 0, 733, 667 }, { 0, 733, 600 }, { 0, 733, 533 }, { 0, 733, 467 },
    { 0, 733, 400 }, { 0, 733, 333 }, { 0, 733, 267 }, { 0, 733, 200 },
    { 0, 733, 133 }, { 0, 733, 67 }, { 0, 733, 0 }, { 67, 733, 0 },
    { 133, 733, 0 }, { 200, 733, 0 }, { 267, 733, 0 }, { 333, 733, 0 },
    { 400, 733, 0 }, { 467, 733, 0 }, { 533, 733, 0 }, { 600, 733, 0 },
    { 667, 733, 0 }, { 733, 733, 0 }, { 733, 667, 0 }, { 733, 600, 0 },
    { 733, 533, 0 }, { 733, 467, 0 }, { 733, 400, 0 }, { 733, 333, 0 },
    { 733, 267, 0 }, { 733, 200, 0 }, { 733, 133, 0 }, { 733, 67, 0 },
    { 467, 0, 0 }, { 467, 0, 67 }, { 467, 0, 133 }, { 467, 0, 200 },
    { 467, 0, 267 }, { 467, 0, 333 }, { 467, 0, 400 }, { 467, 0, 467 },
    { 400, 0, 467 }, { 333, 0, 467 }, { 267, 0, 467 }, { 200, 0, 467 },
    { 133, 0, 467 }, { 67, 0, 467 }, { 0, 0, 467 }, { 0, 67, 467 },
    { 0, 133, 467 }, { 0, 200, 467 }, { 0, 267, 467 }, { 0, 333, 467 },
    { 0, 400, 467 }, { 0, 467, 467 }, { 0, 467, 400 }, { 0, 467, 333 },
    { 0, 467, 267 }, { 0, 467, 200 }, { 0, 467, 133 }, { 0, 467, 67 },
    { 0, 467, 0 }, { 67, 467, 0 }, { 133, 467, 0 }, { 200, 467, 0 },
    { 267, 467, 0 }, { 333, 467, 0 }, { 400, 467, 0 }, { 467, 467, 0 },
    { 467, 400, 0 }, { 467, 333, 0 }, { 467, 267, 0 }, { 467, 200, 0 },
    { 467, 133, 0 }, { 467, 67, 0 }, { 267, 0, 0 }, { 267, 0, 67 },
    { 267, 0, 133 }, { 267, 0, 200 }, { 267, 0, 267 }, { 200, 0, 267 },
    { 133, 0, 267 }, { 67, 0, 267 }, { 0, 0, 267 }, { 0, 67, 267 },
    { 0, 133, 267 }, { 0, 200, 267 }, { 0, 267, 267 }, { 0, 267, 200 },
    { 0, 267, 133 }, { 0, 267, 67 }, { 0, 267, 0 }, { 67, 267, 0 },
    { 133, 267, 0 }, { 200, 267, 0 }, { 267, 267, 0 }, { 267, 200, 0 },
    { 267, 133, 0 }, { 267, 67, 0 }, { 1000, 1000, 1000 }, { 0, 0, 0 }
};
#endif

/* Create an ST color value from VDI color */
static int vdi_Palette_convertVdiToSt(int col) {
    return (col + 72) / 143;
}

/* Create a VDI color value from ST color
 *
 * we use a lookup table for speed and space savings
 */
#define vdi_Palette_convertStToVdi(col) vdi_Palette_convertStToVdiTable[(col)&0x07]
/*
 * this table implements the following calculation, as used by ST TOS:
 *      VDI value = (ST palette hardware value * 1000) / 7
 */
static const WORD vdi_Palette_convertStToVdiTable[8] = { 0, 142, 285, 428, 571, 714, 857, 1000 };

#if CONF_WITH_STE_SHIFTER
// Create an STe color value from VDI color.
static int vdi_Palette_convertVdiToSte(int col) {
    col = (col * 3 + 100) / 200;
    col = ((col & 1) << 3) | (col >> 1);  /* Shift lowest bit to top */
    return col;
}

/* Create a VDI color value from STe color
 *
 * we use a lookup table for speed and space savings
 */
#define vdi_Palette_convertSteToVdi(col) vdi_Palette_convertSteToVdiTable[(col)&0x0f]
/*
 * This table implements the following calculation, as used by STe TOS:
 * VDI value = (STe palette hardware value * 1000 + 7) / 15
 * The sequence of numbers in the table reflects the arrangement of bits in the STe palette hardware: 0 3 2 1
 */
static const WORD vdi_Palette_convertSteToVdiTable[16] =
    { 0, 133, 267, 400, 533, 667, 800, 933, /* values 0,2 ... 14 */
      67, 200, 333, 467, 600, 733, 867, 1000 }; /* values 1,3 ... 15 */

#endif

#if CONF_WITH_TT_SHIFTER
// Create a TT color value from VDI color.
static int vdi_Palette_convertVdiToTt(int col) {
    return (col * 15 + 500) / 1000;
}

/* Create a VDI color value from TT color
 *
 * we use a lookup table for speed & space savings
 */
#define vdi_Palette_convertTtToVdi(col) vdi_Palette_convertTtToVdiTable[(col)&0x0f]
/*
 * this table implements the following calculation, as used by TT TOS:
 *      VDI value = (TT palette hardware value * 1000 + 7) / 15
 */
static const WORD vdi_Palette_convertTtToVdiTable[16] =
    { 0, 67, 133, 200, 267, 333, 400, 467, 533, 600, 667, 733, 800, 867, 933, 1000 };

/* Return adjusted VDI color number for TT systems
 * This ensures that we access the right save area
 * (lineaVars.palette_palette16 or lineaVars.palette_palette240) if bank switching is in effect
 */
static WORD vdi_Palette_adjustTtColorIndex(WORD colnum) {
    UWORD bank;
    UWORD tt_shifter = EgetShift();
    UWORD rez = (tt_shifter>>8) & 0x07;
    switch(rez) {
    case ST_LOW:
    case ST_MEDIUM:
    case TT_MEDIUM:
        bank = tt_shifter & 0x000f;
        colnum += bank * 16;
    }
    return colnum;
}

/* Set an entry in the TT hardware palette
 * TT video hardware has several obscure features which complicate
 * the VDI handler; we try to be TOS3-compatible
 * Input is VDI-style: colnum is VDI pen#, rgb[] entries are 0-1000
 */
static void vdi_Palette_setTtColor(WORD colnum, WORD *rgb) {
    // First we determine which h/w palette register to update
    WORD hwreg = vdi_context.palette.penToPaletteTable[colnum];    /* default, can be modified below */

    UWORD tt_shifter = EgetShift();
    UWORD rez = (tt_shifter>>8) & 0x07;
    UWORD bank = tt_shifter & 0x000f;
    UWORD mask = vdi_deviceColorNum - 1;

    switch(rez) {
    case ST_LOW:
    case ST_MEDIUM:
    case TT_MEDIUM:
        hwreg &= mask;      /* mask out unwanted bits */
        hwreg += bank * 16; /* allow for bank number */
        break;
    case ST_HIGH:   /* also known as duochrome on a TT */
        /*
         * set register 254 or 255 depending on the VDI pen#
         * and the invert bit in palette register 0
         */
        {
            WORD hwvalue = EsetColor(0,-1);
            if (hwvalue & TT_DUOCHROME_INVERT)
                hwreg = 255 - colnum;
            else hwreg = 254 + colnum;
        }
        break;
    case TT_HIGH:
        return;
    }

    /*
     * then we determine what value to put in it
     */
    WORD hwvalue;
    WORD r = rgb[0], g = rgb[1], b = rgb[2];
    if (tt_shifter & TT_HYPER_MONO) {
        /* we do what TOS3 does: first, derive a weighted value 0-1000
         * based on input RGB values; then, scale it to a value 0-255
         * (which the h/w applies to all 3 guns)
         */
        hwvalue = mul_div(30,r,100) + mul_div(59,g,100) + mul_div(11,b,100);
        hwvalue = mul_div(255,hwvalue,1000);
    } else
        hwvalue = (vdi_Palette_convertVdiToTt(r) << 8) | (vdi_Palette_convertVdiToTt(g) << 4) | vdi_Palette_convertVdiToTt(b);
    EsetColor(hwreg, hwvalue);
}

/*
 * Return VDI values for hyper mono mode
 * In hyper mono mode, the original VDI values are not even approximately
 * preserved in the hardware palette registers.  So we do what TOS3 does:
 * we fake the return values based on the previously saved values.
 */
static void vdi_Palette_getTtHyperMonoColor(WORD colnum, WORD *retval) {
    WORD *save = (colnum < 16) ? lineaVars.palette_palette16[colnum] : lineaVars.palette_palette240[colnum-16];
    for (WORD i = 0; i < 3; i++, save++, retval++) {
        // First clamp the raw values.
        WORD vditemp;
        if (*save > 1000)
            vditemp = 1000;
        else if (*save < 0)
            vditemp = 0;
        else vditemp = *save;
        // Convert to h/w value then back to the equivalent VDI value, just as if we had actually written them in a normal mode
        *retval = vdi_Palette_convertTtToVdi(vdi_Palette_convertVdiToTt(vditemp));
    }
}

/* Query an entry in the TT hardware palette
 * TT video hardware has several obscure features which complicate
 * the VDI handler; we try to be TOS3-compatible
 * Input colnum is VDI pen#, returned values are nominally 0-1000
 */
static void vdi_Palette_queryTtColor(WORD colnum,WORD *retval) {
    /*
     * first we determine which h/w palette register to read
     */
    WORD hwreg = vdi_context.palette.penToPaletteTable[colnum];    /* default, can be modified below */

    UWORD tt_shifter = EgetShift();
    UWORD rez = (tt_shifter>>8) & 0x07;
    UWORD bank = tt_shifter & 0x000f;
    UWORD mask = vdi_deviceColorNum - 1;

    switch(rez) {
    case ST_LOW:
    case ST_MEDIUM:
    case TT_MEDIUM:
        hwreg &= mask;      /* mask out unwanted bits */
        hwreg += bank * 16; /* allow for bank number */
        break;
    case ST_HIGH:   /* also known as duochrome on a TT */
        /*
         * set register 254 or 255 depending on the VDI pen#
         * and the invert bit in palette register 0
         */
        {
            WORD hwvalue = EsetColor(0,-1);
            if (hwvalue & TT_DUOCHROME_INVERT)
                hwreg = 255 - colnum;
            else hwreg = 254 + colnum;
        }
        break;
    case TT_HIGH:
        retval[0] = retval[1] = retval[2] = colnum ? 0 : 1000;
        return;
    }

    if (tt_shifter & TT_HYPER_MONO)
        vdi_Palette_getTtHyperMonoColor(colnum,retval);
    else {
        WORD hwvalue = EsetColor(hwreg,-1);
        retval[0] = vdi_Palette_convertTtToVdi(hwvalue >> 8);
        retval[1] = vdi_Palette_convertTtToVdi(hwvalue >> 4);
        retval[2] = vdi_Palette_convertTtToVdi(hwvalue);
    }
}
#endif

#if CONF_WITH_VIDEL
/* Create videl colour value from VDI colour */
static LONG vdi_Palette_convertVdiToVidel(WORD col) {
    return divu((ULONG)col*255+500, 1000);      /* scale 1000 -> 255 */
}

/* Create VDI colour value from videl colour */
static WORD vdi_Palette_convertVidelToVdi(LONG col) {
    return divu((col&0xff)*1000+128, 255);      /* scale 255 -> 1000 */
}
#endif

/*
 * Monochrome screens get special handling because they don't use the
 * regular palette setup; instead, bit 0 of h/w palette register 0
 * controls whether the screen background is white (bit0=0) or black
 * (bit0=1).
 *
 * Also, from a VDI standpoint, you would expect that setting pen 0 to
 * (1000,1000,1000) would get white and setting pen 0 to (0,0,0) would
 * get black; but what should happen with intermediate values?
 *
 * Atari TOS handles this as follows:
 * 1. each RGB value less than a fudge factor F is converted to 0 and
 *    each value greater than or equal to 1000-F is converted to 1000
 * 2. if the sum of the values is neither 0 nor 3000, nothing is done
 * 3. if asked to change pen 1, it sets pen 0 to white, irrespective of
 *    RGB values
 * 4. if changing pen 0, it respects the RGB values
 *
 * We do the same ...
 */
static WORD vdi_Palette_adjustMonoValues(WORD colnum,WORD *rgb) {
    WORD fudge = vdi_Palette_stMonoFudgeFactor;
#if CONF_WITH_STE_SHIFTER
    if (has_ste_shifter)
        fudge = vdi_Palette_steMonoFudgeFactor;
#endif

    WORD sum = 0;
    for (WORD i = 0; i < 3; i++) {
        if (rgb[i] < fudge)
            rgb[i] = 0;
        else if (rgb[i] >= (1000-fudge))
            rgb[i] = 1000;
        sum += rgb[i];
    }

    if ((sum > 0) && (sum < 3000))
        return -1;      /* 'do nothing' indicator */

    if (colnum == 1) {
        colnum = 0;     /* set pen 0 to white */
        rgb[0] = rgb[1] = rgb[2] = 1000;
    }

    return colnum;
}

/* Set an entry in the hardware color palette.
 * Input is VDI-style: colnum is VDI pen#, rgb[] entries are 0-1000
 */
static void vdi_Palette_setColor(WORD colnum, WORD *rgb) {
    WORD hwreg = vdi_context.palette.penToPaletteTable[colnum] & (vdi_deviceColorNum - 1); /* get hardware register */
    WORD r = rgb[0];
    WORD g = rgb[1];
    WORD b = rgb[2];

#if CONF_WITH_VIDEL
    if (has_videl) {
        LONG videlrgb = (vdi_Palette_convertVdiToVidel(r) << 16) | (vdi_Palette_convertVdiToVidel(g) << 8) | vdi_Palette_convertVdiToVidel(b);
        VsetRGB(hwreg,1,(LONG)&videlrgb);
        return;
    }
#endif

#if CONF_WITH_TT_SHIFTER
    if (has_tt_shifter) {
        vdi_Palette_setTtColor(colnum,rgb);
        return;
    }
#endif

    if (lineaVars.screen_planeNb == 1) { /* special handling for monochrome screens */
        hwreg = vdi_Palette_adjustMonoValues(hwreg,rgb);  /* may update rgb[] */
        if (hwreg < 0)                          /* 'do nothing' */
            return;
        r = rgb[0];
        g = rgb[1];
        b = rgb[2];
    }

#if CONF_WITH_STE_SHIFTER
    if (has_ste_shifter) {
        r = vdi_Palette_convertVdiToSte(r);
        g = vdi_Palette_convertVdiToSte(g);
        b = vdi_Palette_convertVdiToSte(b);
    } else
#endif
    {
        r = vdi_Palette_convertVdiToSt(r);
        g = vdi_Palette_convertVdiToSt(g);
        b = vdi_Palette_convertVdiToSt(b);
    }

    Setcolor(hwreg, (r << 8) | (g << 4) | b);
}

/*
 * Set color index table
 */
static void vdi_vs_color(vdi_VirtualWorkstation *vwk) {
    WORD *intin = lineaVars.INTIN;
    WORD colnum = intin[0];
    /* Check for valid color index */
    if (colnum < 0 || colnum >= vdi_deviceColorNum)
        return; /* Out of range */

#if CONF_WITH_TT_SHIFTER
    if (has_tt_shifter)
        colnum = vdi_Palette_adjustTtColorIndex(colnum);  /* handles palette bank issues */
#endif

    // Copy raw values to the "requested colour" arrays, then clamp them to 0-1000 before calling vdi_Palette_setColor().
    WORD *rgbSrc = &intin[1], rgbDst[3], *palette;
#if vdi_Palette_extendedEnabled
    if (colnum < 16)
#endif
        palette = lineaVars.palette_palette16[colnum];
#if vdi_Palette_extendedEnabled
    else
        palette = lineaVars.palette_palette240[colnum-16];
#endif   
    for (WORD i = 0; i < 3; i++) {
        WORD c = rgbSrc[i];
        palette[i] = c;
        if (c > 1000)
            c = 1000;
        if (c < 0)
            c = 0;
        rgbDst[i] = c;
    }

    colnum = intin[0]; /* may have been munged on TT system, see above */
    vdi_Palette_setColor(colnum, rgbDst);
}

/* Set the default palette etc. */
static void vdi_Palette_initialize(void) {
    WORD (*palette16)[3] = lineaVars.palette_palette16;
#if vdi_Palette_extendedEnabled
    WORD (*palette240)[3] = lineaVars.palette_palette240;
#endif

    /* set up palette */
    memcpy(palette16, vdi_Palette_defaultStPalette, sizeof(vdi_Palette_defaultStPalette)); /* use ST as default */

#if CONF_WITH_VIDEL
    if (has_videl) {
        memcpy(palette16, vdi_Palette_defaultVidelPalette1, sizeof(vdi_Palette_defaultVidelPalette1));
        memcpy(palette240, vdi_Palette_defaultVidelPalette2, sizeof(vdi_Palette_defaultVidelPalette2));
    } else
#endif
#if CONF_WITH_TT_SHIFTER
    if (has_tt_shifter) {
        memcpy(palette16, vdi_Palette_defaultTtPalette1, sizeof(vdi_Palette_defaultTtPalette1));
        memcpy(palette240, vdi_Palette_defaultTtPalette2, sizeof(vdi_Palette_defaultTtPalette2));
    } else
#endif
    {
        /* Nothing */
    }

    WORD *penToPaletteTable = vdi_context.palette.penToPaletteTable;
    
    /* set up vdi pen -> hardware colour register mapping */
    memcpy(penToPaletteTable, vdi_Palette_penToPaletteDefaultTable, sizeof(vdi_Palette_penToPaletteDefaultTable));
    penToPaletteTable[1] = vdi_Palette_maxColours - 1;

#if vdi_Palette_extendedEnabled
    {
        int i;
        for (i = 16; i < vdi_Palette_maxColours-1; i++)
            penToPaletteTable[i] = i;
        penToPaletteTable[i] = 15;
    }
#endif

    /* set up reverse mapping (hardware colour register -> vdi pen) */
    {
        WORD *paletteToPenTable = vdi_context.palette.paletteToPenTable;
        for (int i = 0; i < vdi_Palette_maxColours; i++)
            paletteToPenTable[penToPaletteTable[i]] = i;
    }
    
    /* now initialise the hardware */
    WORD deviceColorNum = vdi_deviceColorNum;
#if vdi_Palette_extendedEnabled
    palette240 -= 16;
#endif
    for (int i = 0; i < deviceColorNum; i++) {
        if (i < 16)
            vdi_Palette_setColor(i, palette16[i]);
#if vdi_Palette_extendedEnabled
        else
            vdi_Palette_setColor(i, palette240[i]);
#endif
    }

#ifdef HATARI_DUOCHROME_WORKAROUND
    /*
     * The following is a workaround for a bug in Hatari v1.9 and earlier.
     * It is disabled by default, and should be removed once the Hatari
     * bug is fixed.
     *
     * In Duochrome mode (ST high on a TT), Hatari uses palette register
     * 0 as the background colour and register 255 as the foreground
     * colour; this is not how real hardware behaves.  However, there
     * was a compensating bug in EmuTOS that set register 0 to white and
     * register 255 to black, so everything displayed OK.
     *
     * On real hardware, register 0 contains the inversion bit (bit 1),
     * and the foreground/background colours are in registers 254/255.
     * For both TOS3 and EmuTOS, the initial value for VDI pens 0/254/255
     * are white/white/black for all resolutions, which causes hardware
     * registers 0/254/255 to be set to 0x0fff/0x0fff/0x000.  Without any
     * compensation, this would cause problems when switching to duochrome
     * mode: since the inversion bit in register 0 is set, the display
     * would show as white on black.
     *
     * Since it's desirable for other reasons to leave register 0 as
     * white, TOS3 (and EmuTOS) compensate as follows: if the inversion
     * bit is set, registers 254/255 are set to 0x0000/0x0fff.  This
     * produces the correct black on white display on real hardware, but
     * a display of white on white under Hatari (both EmuTOS and TOS3).
     *
     * The following workaround preserves the inversion bit, but sets a
     * value of "almost black" in register 0.  The output will be visible
     * on both Hatari and real TT hardware, but Hatari will display black
     * on white, while real hardware displays white on black.  Another
     * consequence of this workaround is that a vq_color() for the actual
     * value of pen 0 will return an unexpected result.
     */
#if CONF_WITH_TT_SHIFTER
    if (has_tt_shifter && (sshiftmod == ST_HIGH))
        EsetColor(0,0x0002);
#endif
#endif
}

/*
 * Query color index table.
 */
static void vdi_vq_color(vdi_VirtualWorkstation *vwk) {
    WORD *intin = lineaVars.INTIN, *intout = lineaVars.INTOUT;
    intout[1] = intout[2] = intout[3] = 0;  /* Default values */

    /* Check for valid color index */
    WORD colnum = intin[0];
    if (colnum < 0 || colnum >= vdi_deviceColorNum) {
        /* It was out of range */
        intout[0] = -1;
        return;
    }
    intout[0] = colnum;

#if CONF_WITH_TT_SHIFTER
    if (has_tt_shifter)
        colnum = vdi_Palette_adjustTtColorIndex(colnum);  /* handles palette bank issues */
#endif

    if (intin[1] == 0) { /* return last-requested value */
        WORD *palette;
#if vdi_Palette_extendedEnabled
        if (colnum < 16)
#endif
            palette = lineaVars.palette_palette16[colnum];
#if vdi_Palette_extendedEnabled
        else
            palette = lineaVars.palette_palette240[colnum - 16];
#endif
        intout[1] = palette[0];
        intout[2] = palette[1];
        intout[3] = palette[2];
        return;
    }

    /*
     * return actual current value
     */
    colnum = intin[0]; /* may have been munged on TT system, see above */
    WORD hwreg = vdi_context.palette.penToPaletteTable[colnum] & (vdi_deviceColorNum - 1); /* get hardware register */

#if CONF_WITH_VIDEL
    if (has_videl) {
        LONG rgb;
        VgetRGB(hwreg, 1, (LONG)&rgb);
        intout[1] = vdi_Palette_convertVidelToVdi(rgb >> 16);
        intout[2] = vdi_Palette_convertVidelToVdi(rgb >> 8);
        intout[3] = vdi_Palette_convertVidelToVdi(rgb);
        return;
    }
#endif
#if CONF_WITH_TT_SHIFTER
    if (has_tt_shifter) {
        vdi_Palette_queryTtColor(colnum, &intout[1]);
        return;
    }
#endif
#if CONF_WITH_STE_SHIFTER
    if (has_ste_shifter) {
        WORD c = Setcolor(hwreg, -1);
        intout[1] = vdi_Palette_convertSteToVdi(c >> 8);
        intout[2] = vdi_Palette_convertSteToVdi(c >> 4);
        intout[3] = vdi_Palette_convertSteToVdi(c);
        return;
    }
#endif
    /* ST shifter */
    {
        WORD c = Setcolor(hwreg, -1);
        intout[1] = vdi_Palette_convertStToVdi(c >> 8);
        intout[2] = vdi_Palette_convertStToVdi(c >> 4);
        intout[3] = vdi_Palette_convertStToVdi(c);
    }
}

//--------------------------------------------------------------------------------
// Clipping.
//--------------------------------------------------------------------------------
static void vdi_vs_clip(vdi_VirtualWorkstation * vwk) {
    // TODO: Clipping can disabled for performance purpose but is this acceptable ?
    vwk->clippingEnabled = lineaVars.INTIN[0];
    if (vwk->clippingEnabled) {
        Rect * rect = (Rect*)lineaVars.PTSIN;
        vdi_Rect_sortCorners(rect);
        vwk->clippingRect.xMin = max(0, rect->x1);
        vwk->clippingRect.yMin = max(0, rect->y1);
        vwk->clippingRect.xMax = min(vdi_deviceResolutionX, rect->x2);
        vwk->clippingRect.yMax = min(vdi_deviceResolutionY, rect->y2);
    } else {
        vwk->clippingRect.xMin = 0;
        vwk->clippingRect.yMin = 0;
        vwk->clippingRect.xMax = vdi_deviceResolutionX;
        vwk->clippingRect.yMax = vdi_deviceResolutionY;
    }
}

//--------------------------------------------------------------------------------
// Writing mode.
//--------------------------------------------------------------------------------
static void vdi_vswr_mode(vdi_VirtualWorkstation * vwk) {
    WORD wm = checkRangeWord(lineaVars.INTIN[0], vdi_WritingMode_min, vdi_WritingMode_max, vdi_WritingMode_default);
    lineaVars.INTOUT[0] = wm;
    vwk->wrt_mode = wm - 1;
}

//--------------------------------------------------------------------------------
// Color.
//--------------------------------------------------------------------------------
/*
 * validate colour index
 * checks the supplied colour index and, if valid, returns it;
 * otherwise returns 1 (which by default maps to black)
 */
static WORD vdi_Color_validateIndex(WORD colnum) {
    return (colnum < 0 || colnum >= vdi_deviceColorNum) ? 1 : colnum;
}

//--------------------------------------------------------------------------------
// Fill pattern.
//--------------------------------------------------------------------------------
/* These are still needed for text blitting */
static const UWORD vdi_FillPattern_solid = 0xFFFF;

/* the storage for the used defined fill pattern */
static const UWORD vdi_FillPattern_defaultUserDefined[16] = {
    0x07E0, 0x0FF0, 0x1FD8, 0x1808, 0x1808, 0x1008, 0x1E78, 0x1348,
    0x1108, 0x0810, 0x0B70, 0x0650, 0x07A0, 0x1E20, 0x1BC0, 0x1800
};

#define vdi_FillPattern_oemMask 7
static const UWORD vdi_FillPattern_oem[128] = {
    /* Brick */
    0xFFFF, 0x8080, 0x8080, 0x8080, 0xFFFF, 0x0808, 0x0808, 0x0808,
    /* Diagonal Bricks */
    0x2020, 0x4040, 0x8080, 0x4141, 0x2222, 0x1414, 0x0808, 0x1010,
    /* Grass */
    0x0000, 0x0000, 0x1010, 0x2828, 0x0000, 0x0000, 0x0101, 0x8282,
    /* Trees */
    0x0202, 0x0202, 0xAAAA, 0x5050, 0x2020, 0x2020, 0xAAAA, 0x0505,
    /* Dashed x's */
    0x4040, 0x8080, 0x0000, 0x0808, 0x0404, 0x0202, 0x0000, 0x2020,
    /* Cobble Stones */
    0x6606, 0xC6C6, 0xD8D8, 0x1818, 0x8181, 0x8DB1, 0x0C33, 0x6000,
    /* Sand */
    0x0000, 0x0000, 0x0400, 0x0000, 0x0010, 0x0000, 0x8000, 0x0000,
    /* Rough Weave */
    0xF8F8, 0x6C6C, 0xC6C6, 0x8F8F, 0x1F1F, 0x3636, 0x6363, 0xF1F1,
    /* Quilt */
    0xAAAA, 0x0000, 0x8888, 0x1414, 0x2222, 0x4141, 0x8888, 0x0000,
    /* Patterned Cross */
    0x0808, 0x0000, 0xAAAA, 0x0000, 0x0808, 0x0000, 0x8888, 0x0000,
    /* Balls */
    0x7777, 0x9898, 0xF8F8, 0xF8F8, 0x7777, 0x8989, 0x8F8F, 0x8F8F,
    /* Vertical Scales */
    0x8080, 0x8080, 0x4141, 0x3E3E, 0x0808, 0x0808, 0x1414, 0xE3E3,
    /* Diagonal scales */
    0x8181, 0x4242, 0x2424, 0x1818, 0x0606, 0x0101, 0x8080, 0x8080,
    /* Checker Board */
    0xF0F0, 0xF0F0, 0xF0F0, 0xF0F0, 0x0F0F, 0x0F0F, 0x0F0F, 0x0F0F,
    /* Filled Diamond */
    0x0808, 0x1C1C, 0x3E3E, 0x7F7F, 0xFFFF, 0x7F7F, 0x3E3E, 0x1C1C,
    /* Herringbone */
    0x1111, 0x2222, 0x4444, 0xFFFF, 0x8888, 0x4444, 0x2222, 0xFFFF
};

#define vdi_FillPattern_ditherMask 3 /* mask off all but four scans */
static const UWORD vdi_FillPattern_dither[32] = {
    0x0000, 0x4444, 0x0000, 0x1111,     /* intensity level 2 */
    0x0000, 0x5555, 0x0000, 0x5555,     /* intensity level 4 */
    0x8888, 0x5555, 0x2222, 0x5555,     /* intensity level 6 */
    0xAAAA, 0x5555, 0xAAAA, 0x5555,     /* intensity level 8 */
    0xAAAA, 0xDDDD, 0xAAAA, 0x7777,     /* intensity level 10 */
    0xAAAA, 0xFFFF, 0xAAAA, 0xFFFF,     /* intensity level 12 */
    0xEEEE, 0xFFFF, 0xBBBB, 0xFFFF,     /* intensity level 14 */
    0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF      /* intensity level 16 */
};

#define vdi_HatchStyle_0Mask 7
static const UWORD vdi_HatchStyle_0[48] = {
    /* narrow spaced + 45 */
    0x0101, 0x0202, 0x0404, 0x0808, 0x1010, 0x2020, 0x4040, 0x8080,
    /* medium spaced thick 45 deg */
    0x6060, 0xC0C0, 0x8181, 0x0303, 0x0606, 0x0C0C, 0x1818, 0x3030,
    /* medium +-45 deg */
    0x4242, 0x8181, 0x8181, 0x4242, 0x2424, 0x1818, 0x1818, 0x2424,
    /* medium spaced vertical */
    0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080,
    /* medium spaced horizontal */
    0xFFFF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* medium spaced cross */
    0xFFFF, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080
};

#define vdi_HatchStyle_1Mask 0xF
static const UWORD vdi_HatchStyle_1[96] = {
    /* wide +45 deg */
    0x0001, 0x0002, 0x0004, 0x0008, 0x0010, 0x0020, 0x0040, 0x0080,
    0x0100, 0x0200, 0x0400, 0x0800, 0x1000, 0x2000, 0x4000, 0x8000,
    /* widely spaced thick 45 deg */
    0x8003, 0x0007, 0x000E, 0x001C, 0x0038, 0x0070, 0x00E0, 0x01C0,
    0x0380, 0x0700, 0x0E00, 0x1C00, 0x3800, 0x7000, 0x0E000, 0x0C001,
    /* widely +- 45 deg */
    0x8001, 0x4002, 0x2004, 0x1008, 0x0810, 0x0420, 0x0240, 0x0180,
    0x0180, 0x0240, 0x0420, 0x0810, 0x1008, 0x2004, 0x4002, 0x8001,
    /* widely spaced vertical */
    0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
    0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
    /* widely spaced horizontal */
    0xFFFF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    /* widely spaced horizontal/vert cross */
    0xFFFF, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080,
    0xFFFF, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080,
};

static const UWORD vdi_FillPattern_hollow = 0;

static void vdi_FillPattern_update(vdi_VirtualWorkstation * vwk) {
    const UWORD *pp = NULL;
    WORD fi = vwk->fill_index;
    WORD pm = 0;
    bool multiFillAvailable = false;
    switch (vwk->fill_style) {
    case vdi_FillingStyle_hollow:
        pp = &vdi_FillPattern_hollow;
        break;

    case vdi_FillingStyle_solid:
        pp = &vdi_FillPattern_solid;
        break;

    case vdi_FillingStyle_pattern:
        if (fi < 8) {
            pm = vdi_FillPattern_ditherMask;
            pp = &vdi_FillPattern_dither[fi * (pm + 1)];
        } else {
            pm = vdi_FillPattern_oemMask;
            pp = &vdi_FillPattern_oem[(fi - 8) * (pm + 1)];
        }
        break;
    case vdi_FillingStyle_hatch:
        if (fi < 6) {
            pm = vdi_HatchStyle_0Mask;
            pp = &vdi_HatchStyle_0[fi * (pm + 1)];
        } else {
            pm = vdi_HatchStyle_1Mask;
            pp = &vdi_HatchStyle_1[(fi - 6) * (pm + 1)];
        }
        break;
    case vdi_FillingStyle_user:
        pm = 0x000f;
        pp = (UWORD *)&vwk->ud_patrn[0];
        multiFillAvailable = vwk->multiFillAvailable;
        break;
    }
    vwk->patptr = (UWORD *)pp;
    vwk->patmsk = pm;
    vwk->multiFillEnabled = multiFillAvailable;
}

static void vdi_FillPattern_setUserData(vdi_VirtualWorkstation * vwk, WORD count, const UWORD *src) {
    if (count == 16)
        vwk->multiFillAvailable = false; // Single Plane Pattern.
    else if (count == (lineaVars.workstation_inquireTable[4] * 16))
        vwk->multiFillAvailable = true; // Valid Multi-plane pattern.
    else
        return; // Invalid pattern, do not change anything.
    copyWords(count, (const WORD*)src, (WORD*)vwk->ud_patrn);
}

static WORD vdi_FillPattern_checkStyle(WORD fs) {
    return checkRangeWord(fs, vdi_FillingStyle_min, vdi_FillingStyle_max, vdi_FillingStyle_default);
}

static WORD vdi_FillPattern_checkPattern(WORD fs, WORD fi) {
    if (fs == vdi_FillingStyle_pattern)
        fi = checkRangeWord(fi, vdi_PatternStyle_min, vdi_PatternStyle_max, vdi_PatternStyle_default);
    else
        fi = checkRangeWord(fi, vdi_HatchStyle_min, vdi_HatchStyle_max, vdi_HatchStyle_default);
    return fi;
}

static void vdi_vsf_udpat(vdi_VirtualWorkstation * vwk) {
    vdi_FillPattern_setUserData(vwk, lineaVars.parameters.contrl->inputIntNb, (const UWORD*)lineaVars.INTIN);
}

static void vdi_vsf_interior(vdi_VirtualWorkstation * vwk) {
    WORD fs = vdi_FillPattern_checkStyle(lineaVars.INTIN[0]);
    vwk->fill_style = fs;
    lineaVars.INTOUT[0] = fs;
    vdi_FillPattern_update(vwk);
}

static void vdi_vsf_style(vdi_VirtualWorkstation * vwk) {
    WORD fi = vdi_FillPattern_checkPattern(vwk->fill_style, lineaVars.INTIN[0]);
    vwk->fill_index = fi - 1;
    lineaVars.INTOUT[0] = fi;
    vdi_FillPattern_update(vwk);
}

static void vdi_vsf_color(vdi_VirtualWorkstation * vwk) {
    WORD fc = vdi_Color_validateIndex(lineaVars.INTIN[0]);
    lineaVars.INTOUT[0] = fc;
    vwk->fill_color = vdi_context.palette.penToPaletteTable[fc];
}

static void vdi_vsf_perimeter(vdi_VirtualWorkstation * vwk) {
    WORD flag = lineaVars.INTIN[0] == 0;
    lineaVars.INTOUT[0] = flag;
    vwk->fill_per = flag;
}

static void vdi_vqf_attributes(vdi_VirtualWorkstation * vwk) {
    WORD * RESTRICT pointer = lineaVars.INTOUT;
    *pointer++ = vwk->fill_style;
    *pointer++ = vdi_context.palette.paletteToPenTable[vwk->fill_color];
    *pointer++ = vwk->fill_index + 1;
    *pointer++ = vwk->wrt_mode + 1;
    *pointer = vwk->fill_per;
}

//--------------------------------------------------------------------------------
// Pixel read and write.
//--------------------------------------------------------------------------------
UWORD vdi_Pixel_read(WORD x, WORD y) {
    if (vdi_checkPixelCoordinates(x, y))
        return vdi_getPixelColor(lineaVars.screen_planeNb, 0x8000 >> (x & 0xf), vdi_getPixelAddress(x, y));
    else
        return 0;
}

static void vdi_Pixel_write(WORD x, WORD y, UWORD color) {
    if (vdi_checkPixelCoordinates(x, y))
        vdi_setPixelColor(lineaVars.screen_planeNb, 0x8000 >> (x & 0xf), vdi_getPixelAddress(x, y), color);
}

/*
 * Gets a pixel's color index value.
 *
 * input:
 *     lineaVars.PTSIN(0) = x coordinate.
 *     lineaVars.PTSIN(1) = y coordinate.
 * output:
 *     pixel value
 */
static void vdi_v_get_pixel(vdi_VirtualWorkstation * vwk) {
    WORD *ptsin = lineaVars.PTSIN;
    WORD x = ptsin[0], y = ptsin[1];
    WORD pel = (WORD)vdi_Pixel_read(x,y);
    WORD *intout = lineaVars.INTOUT;
    intout[0] = pel;
    intout[1] = vdi_context.palette.paletteToPenTable[pel];
}

//--------------------------------------------------------------------------------
// Rectangle filling.
//--------------------------------------------------------------------------------
static void vdi_vr_recfl(vdi_VirtualWorkstation * vwk) {
    vdi_DrawContext dc;
    vdi_DrawContext_setupPlaneNb(&dc);
    vdi_DrawContext_setupClipping(&dc, vwk);
    vdi_DrawContext_setupFilling(&dc, vwk, vwk->fill_color);
    dc.rect = *(Rect *)lineaVars.PTSIN;
    vdi_getDriver()->fillRectangle(&dc);
}

//--------------------------------------------------------------------------------
// Line.
//--------------------------------------------------------------------------------
/* the six predefined line styles */
const UWORD vdi_Line_predefinedStyles[6] = { 0xFFFF, 0xFFF0, 0xC0C0, 0xFF18, 0xFF00, 0xF191 };

/*
 * Set lineaVars.line_mask from virtual workstation values.
 */
static void vdi_Line_setMask(const vdi_VirtualWorkstation * RESTRICT vwk) {
    WORD l = vwk->line_index;
    lineaVars.line_mask = (l < 6) ? vdi_Line_predefinedStyles[l] : vwk->ud_ls;
}

//--------------------------------------------------------------------------------
// General line.
//--------------------------------------------------------------------------------
/*
 * Draw a general line.
 *
 * This routine draws a line defined by the Line structure, using
 * Bresenham's algorithm.  The line is modified by the lineaVars.line_mask
 * variable and the mode parameter.  This routine handles
 * all interleaved-bitplane video resolutions.
 *
 * Note that for line-drawing the background color is always 0
 * (i.e., there is no user-settable background color).  This fact
 * allows coding short-cuts in the implementation of "replace" and
 * "not" modes, resulting in faster execution of their inner loops.
 */

static void vdi_Line_drawStart(vdi_DrawContext * RESTRICT dc, vdi_VirtualWorkstation * RESTRICT vwk, bool clippingEnabled, WORD mode, UWORD color) {
    vdi_DrawContext_setupPlaneNb(dc);

    dc->clipping.enabled = clippingEnabled;
    if (clippingEnabled)
        dc->clipping.rect = vwk->clippingRect;
    else
        dc->clipping.rect = vdi_context.clippingRectFull;

    dc->mode = mode;
    dc->color = color;
    dc->line.mask = lineaVars.line_mask;
}

static void vdi_Line_drawEnd(vdi_DrawContext * RESTRICT dc) {
    lineaVars.line_mask = dc->line.mask;
}

/**
 * Draw a poly-line.
 * We pass the colour, since this routine is also used for perimeters, which are drawn in the fill colour.
 */
static void vdi_Line_drawPoly(vdi_VirtualWorkstation * RESTRICT vwk, const Point * RESTRICT point, WORD count, WORD color, bool closed) {
    if (count < 2)
        return;
    vdi_DrawContext dc;
    vdi_Line_drawStart(&dc, vwk, vwk->clippingEnabled, vwk->wrt_mode, color);
    if (closed) {
        dc.line.line.p[0] = point[count - 1];
        dc.line.line.p[1] = *point;
        dc.line.lastFlag = false;
        vdi_getDriver()->drawLine(&dc);        
    }
    do {
        dc.line.line.p[0] = *point++;
        dc.line.line.p[1] = *point;
        dc.line.lastFlag = count == 2;
        vdi_getDriver()->drawLine(&dc);
    } while (count-- > 2);
    vdi_Line_drawEnd(&dc);
}

static void vdi_vsl_udsty(vdi_VirtualWorkstation * vwk) {
    vwk->ud_ls = lineaVars.INTIN[0];
}

static void vdi_vsl_type(vdi_VirtualWorkstation * vwk) {
    WORD li = checkRangeWord(lineaVars.INTIN[0], vdi_LineStyle_min, vdi_LineStyle_max, vdi_LineStyle_default);
    lineaVars.INTOUT[0] = li;
    vwk->line_index = li - 1;
}

static void vdi_vsl_width(vdi_VirtualWorkstation * vwk) {
    /* Limit the requested line width to a reasonable value. */
    WORD w = clipWord(lineaVars.PTSIN[0], 1, lineaVars.workstation_sizeTable.lineWidthMax);
    w -= (~w) & 1; /* If the line width is even, make it odd by decreasing it by one */
    /* Set the line width internals and return parameters */
    WORD *ptsout = lineaVars.PTSOUT;
    ptsout[0] = vwk->line_width = w;
    ptsout[1] = 0;
}

static void vdi_vsl_ends(vdi_VirtualWorkstation * vwk) {
    WORD *intin = lineaVars.INTIN;
    WORD lb = checkRangeWord(intin[0], vdi_LineEndStyle_min, vdi_LineEndStyle_max, vdi_LineEndStyle_default);
    WORD le = checkRangeWord(intin[1], vdi_LineEndStyle_min, vdi_LineEndStyle_max, vdi_LineEndStyle_default);
    WORD *intout = lineaVars.INTOUT;
    intout[0] = vwk->line_beg = lb;
    intout[1] = vwk->line_end = le;
}

static void vdi_vsl_color(vdi_VirtualWorkstation * vwk) {
    WORD lc = vdi_Color_validateIndex(lineaVars.INTIN[0]);
    lineaVars.INTOUT[0] = lc;
    vwk->line_color = vdi_context.palette.penToPaletteTable[lc];
}

static void vdi_vql_attributes(vdi_VirtualWorkstation * vwk) {
    WORD *inout = lineaVars.INTOUT;
    inout[0] = vwk->line_index + 1;
    inout[1] = vdi_context.palette.paletteToPenTable[vwk->line_color];
    inout[2] = vwk->wrt_mode + 1;

    WORD *ptsout = lineaVars.PTSOUT;
    ptsout[0] = vwk->line_width;
    ptsout[1] = 0;
}

//--------------------------------------------------------------------------------
// Polygon.
//--------------------------------------------------------------------------------
static void vdi_Polygon_fill(vdi_VirtualWorkstation * RESTRICT vwk, Point *ptsin, WORD pointNb) {
    vdi_DrawContext dc;
    vdi_DrawContext_setupPlaneNb(&dc);
    vdi_DrawContext_setupFilling(&dc, vwk, vwk->fill_color);
    vdi_DrawContext_setupClipping(&dc, vwk);
    dc.polygon.points = ptsin;
    dc.polygon.pointNb = pointNb;
    vdi_getDriver()->fillPolygon(&dc);
    if (vwk->fill_per) {
        lineaVars.line_mask = 0xffff;
        vdi_Line_drawPoly(vwk, ptsin, pointNb, vwk->fill_color, true);
    }
}

//--------------------------------------------------------------------------------
// Fill polygon or bezier surface.
//--------------------------------------------------------------------------------
static void vdi_v_fillarea(vdi_VirtualWorkstation * vwk) {
    Point * point = (Point*)lineaVars.PTSIN;
    WORD count = lineaVars.parameters.contrl->inputVertexNb;
#if vdi_Bezier_enabled
    /* check, if we want to draw a filled bezier curve */
    if (lineaVars.parameters.contrl->subOpcode == 13 && vwk->bez_qual )
        v_bez_fill(vwk, point, count);
    else
#endif
        vdi_Polygon_fill(vwk, point, count);
}

//--------------------------------------------------------------------------------
// Disk.
//--------------------------------------------------------------------------------
/*
 * Fill a disk.
 * This is used by vdi_WideLine_draw():
 *  a) to round the ends of the line if not vdi_LineEndStyle_square
 *  b) to make a smooth join between line segments of a polyline
 */
static void vdi_Circle_draw(vdi_VirtualWorkstation * vwk, vdi_Circle *circle, WORD cx, WORD cy) {
    vdi_DrawContext dc;
    vdi_DrawContext_setupPlaneNb(&dc);
    vdi_DrawContext_setupFilling(&dc, vwk, vwk->fill_color);
    vdi_DrawContext_setupClipping(&dc, vwk);
    dc.disk.center.x = cx;
    dc.disk.center.y = cy;
    dc.disk.dda = circle;
    vdi_getDriver()->fillDisk(&dc);
}

//--------------------------------------------------------------------------------
// Fill area attributes.
//--------------------------------------------------------------------------------
typedef struct {
    WORD beginningStyle, endingStyle;
    WORD fillingColor;
    bool perimeterFilling;
} FillAreaAttributes;

static void vdi_saveFillAreaAttributes(vdi_VirtualWorkstation * RESTRICT vwk, FillAreaAttributes * RESTRICT fillArea) {
    // Set up the fill area attribute environment.
    fillArea->fillingColor = vwk->fill_color;
    fillArea->perimeterFilling = vwk->fill_per;
    fillArea->beginningStyle = vwk->line_beg;
    fillArea->endingStyle = vwk->line_end;

    lineaVars.line_mask = vdi_Line_predefinedStyles[0];
    vwk->fill_color = vwk->line_color;
    vwk->line_beg = vdi_LineEndStyle_square;
    vwk->line_end = vdi_LineEndStyle_square;
    vwk->fill_per = true;
    vwk->patptr = (UWORD *)&vdi_FillPattern_solid;
    vwk->patmsk = 0;
}

static void vdi_restoreFillAreaAttributes(vdi_VirtualWorkstation * RESTRICT vwk, FillAreaAttributes * RESTRICT fillArea) {
    // Restore the fill area attribute environment.
    vwk->fill_color = fillArea->fillingColor;
    vwk->fill_per = fillArea->perimeterFilling;
    vwk->line_beg = fillArea->beginningStyle;
    vwk->line_end = fillArea->endingStyle;
}

//--------------------------------------------------------------------------------
// Arrow.
//--------------------------------------------------------------------------------
/*
 * Helper function for vdi_Arrow_draw().
 * It performs the actual drawing.
 */
static void vdi_Arrow_drawInternal(vdi_VirtualWorkstation * RESTRICT vwk, Point * RESTRICT point, int count, int inc) {
    WORD dx, dy;
    LONG line_len2 = dx = dy = 0;
    /* Set up the arrow-head length and width as a function of line width. */
    WORD arrow_len = (vwk->line_width < 4) ? 8 : (3 * vwk->line_width - 1);
    WORD arrow_wid = arrow_len / 2;
    /* Initialize the beginning pointer. */
    Point *ptr1 = point, *ptr2 = point;

    /* Find the first point which is not so close to the end point that it */
    /* will be obscured by the arrowhead.                                  */
    for (WORD i = 1; i < count; i++) {
        /* Find the deltas between the next point and the end point. Transform */
        /* to a space such that the aspect ratio is uniform and the x axis */
        /* distance is preserved. */

        ptr1 += inc;
        dx = ptr2->x - ptr1->x;
        dy = mul_div_round(ptr2->y - ptr1->y, vdi_deviceSizeY, vdi_deviceSizeX);

        /* Get length of vector connecting the point with the end point. */
        /* If the vector is of sufficient length, the search is over. */
        line_len2 = (LONG)dx*dx + (LONG)dy*dy;
        if (line_len2 >= (LONG)arrow_len*arrow_len)
            break;
    }                           /* End for:  over i. */
    WORD line_len = Isqrt(line_len2);

    /* Set xybeg to the point we found */
    Point *xybeg = ptr1;

    /* If the longest vector is insufficiently long, don't draw an arrow. */
    if (line_len < arrow_len)
        return;

    /* Rotate the arrow-head height and base vectors.  Perform calculations */
    /* in 1000x space.                                                      */

    WORD dxfactor = mul_div_round(dx, 1000, line_len);
    WORD dyfactor = mul_div_round(dy, 1000, line_len);
    WORD ht_x = mul_div_round(arrow_len, dxfactor, 1000);
    WORD ht_y = mul_div_round(arrow_len, dyfactor, 1000);
    WORD base_x = mul_div_round(arrow_wid, -dyfactor, 1000);
    WORD base_y = mul_div_round(arrow_wid, dxfactor, 1000);

    /* Transform the y offsets back to the correct aspect ratio space. */

    ht_y = mul_div_round(ht_y, vdi_deviceSizeX, vdi_deviceSizeY);
    base_y = mul_div_round(base_y, vdi_deviceSizeX, vdi_deviceSizeY);

    /* Build a polygon into a local array first */
    Point triangle[4]; /* allow room for polygon() to close triangle */
    ptr1 = triangle;
    ptr2 = point;

    ptr1->x = ptr2->x + base_x - ht_x;
    ptr1->y = ptr2->y + base_y - ht_y;
    ptr1++;
    ptr1->x = ptr2->x - base_x - ht_x;
    ptr1->y = ptr2->y - base_y - ht_y;
    ptr1++;
    ptr1->x = ptr2->x;
    ptr1->y = ptr2->y;

    vdi_Polygon_fill(vwk, triangle, 3);

    /* Adjust the end point and all points skipped. */
    ptr1 = point;
    ptr2 = xybeg;

    ptr1->x -= ht_x;
    ptr1->y -= ht_y;

    while ((ptr2 -= inc) != ptr1) {
        ptr2->x = ptr1->x;
        ptr2->y = ptr1->y;
    }
}

/**
 * Draw arrow(s) at the end(s) of the line.
 * It will alter the end of the line segment.
 */
static void vdi_Arrow_draw(vdi_VirtualWorkstation * RESTRICT vwk, Point * RESTRICT point, int count) {
    FillAreaAttributes fillArea;
    vdi_saveFillAreaAttributes(vwk, &fillArea);
    /* beginning point is arrowed. */
    if (fillArea.beginningStyle & vdi_LineEndStyle_arrow)
        vdi_Arrow_drawInternal(vwk, point, count, 1);
    /* ending point is arrowed. */
    if (fillArea.endingStyle & vdi_LineEndStyle_arrow)
        vdi_Arrow_drawInternal(vwk, point+count-1, count, -1);
    vdi_restoreFillAreaAttributes(vwk, &fillArea);
}

//--------------------------------------------------------------------------------
// Draw primitives with line width greater than 1.
//--------------------------------------------------------------------------------
/*
 * Helper function for vdi_WideLine_computePerpendicularOffset().
 * Converts input (x,y) to output (x,y) according to the value in 'quad':
 * - 1 ("south-east" quadrant):  x ->  x,  y ->  y
 * - 2 ("south-west" quadrant):  x -> -x,  y ->  y
 * - 3 ("north-west" quadrant):  x -> -x,  y -> -y
 * - 4 ("north-east" quadrant):  x ->  x,  y -> -y
 */
static void vdi_WideLine_convertToFirstQuadrant(WORD quad, WORD x, WORD y, WORD *tx, WORD *ty) {
    switch (quad) {
    default:
    case 1: case 4:
        *tx = x;
        break;
    case 2: case 3:
        *tx = -x;
        break;
    }
    switch (quad) {
    default:
    case 1: case 2:
        *ty = y;
        break;
    case 3: case 4:
        *ty = -y;
        break;
    }
}

/*
 * Calculate the perpendicular offsets.
 * Given a vector (vx,vy) which specifies the length and direction of
 * a line segment, this function returns x & y offsets to add/subtract
 * to the endpoints of the line segment.  The four points thereby
 * specified form a box which is the wideline segment.
 */
static void vdi_WideLine_computePerpendicularOffset(vdi_Circle *circle, WORD * px, WORD * py) {
    WORD *vx = px, *vy = py;

    /* Mirror transform the vector so that it is in the first quadrant. */
    WORD quad;
    if (*vx >= 0)
        quad = (*vy >= 0) ? 1 : 4;
    else
        quad = (*vy >= 0) ? 2 : 3;

    WORD x, y;
    vdi_WideLine_convertToFirstQuadrant(quad, *vx, *vy, &x, &y);

    /*
     * Traverse the circle in a dda-like manner and find the coordinate pair (u, v) such that the magnitude of (u*y - v*x) is minimized.
     * In case of a tie, choose the value which causes (u - v) to be minimized.  If not possible, do something.
     */
    WORD min_val = 32767;
    WORD *offsets = circle->offsets;
    WORD u = *offsets, v = 0;
    WORD x_val = 0, y_val = 0;
    while (true) {
        /* Check for new minimum, same minimum, or finished. */
        WORD magnitude = abs(u * y - v * x);
        if ((magnitude < min_val) || ((magnitude == min_val) && (abs(x_val - y_val) > abs(u - v)))) {
            min_val = magnitude;
            x_val = u;
            y_val = v;
        } else
            break;

        /* Step to the next pixel. */
        if (v == circle->lineNb - 1) {
            if (u == 1)
                break;
            else
                u--;
        } else {
            if (offsets[v + 1] >= u - 1) {
                v++;
                u = offsets[v];
            } else
                u--;
        }
    }

    /* Transform the solution according to the quadrant. */
    vdi_WideLine_convertToFirstQuadrant(quad, x_val, y_val, vx, vy);
}

/*
 * Populate vdi_context.q_circle[] array.
 * This is called by vdi_WideLine_draw() when the current wideline width (lineaVars.circle_currentLineWidth)
 * changes, in order to reinitialise vdi_context.q_circle[]. It uses Bresenham's
 * circle algorithm.
 */
static void vdi_WideLine_computeCircle(vdi_Circle *circle) {
    /* Calculate the number of vertical pixels required. Circles will be flattened if too many lines. */
    circle->lineNb = min((circle->lineWidth * vdi_deviceSizeX / vdi_deviceSizeY) / 2 + 1, vdi_Circle_maxLines);

    /* Initialize the circle DDA.  "y" is set to the radius. */
    WORD x = 0;
    WORD y = (circle->lineWidth + 1) / 2;
    WORD d = 3 - 2 * y;

    /* Do an octant, starting at north.  The values for the next octant */
    /* (clockwise) will be filled by transposing x and y.               */
    while (x <= y) {
        circle->offsets[y] = x;
        circle->offsets[x] = y;
        if (d < 0)
            d += 4 * x + 6;
        else {
            d += 4 * (x - y) + 10;
            y--;
        }
        x++;
    }

    if (vdi_deviceSizeX == vdi_deviceSizeY)     /* square pixels, e.g. ST high */
        return;

    /*
     * handle tall pixels, e.g. ST medium, Falcon 640x240
     *
     * note that this can also include ST Low, which has "slightly tall"
     * pixels on pre-TOS3 machines
     */
    if (vdi_deviceSizeX < vdi_deviceSizeY) {
        // Fake pixel averaging.
        x = 0;
        WORD *yptr = circle->offsets;
        WORD lineNb = circle->lineNb;
        for (WORD i = 0; i < lineNb; i++) {
            y = ((2 * i + 1) * vdi_deviceSizeY / vdi_deviceSizeX) / 2;
            d = 0;
            WORD *xptr = &circle->offsets[x];
            for (WORD j = x; j <= y; j++)
                d += *xptr++;
            *yptr++ = d / (y - x + 1);
            x = y + 1;
        }
        return;
    }

    /*
     * handle vdi_deviceSizeX > vdi_deviceSizeY (wide pixels, e.g. Falcon 320x480)
     *
     * we interpolate from the previously-generated table.  for now we
     * greatly simplify things by assuming that vdi_deviceSizeX = 2 * vdi_deviceSizeY (true
     * for the Falcon).  the following code sets:
     *  q_circle[i] = q_circle[i/2]                         (for even i)
     *  q_circle[i] = (q_circle[i/2] + q_circle[i/2+1])/2   (for odd i)
     */
    WORD n = 0;
    for (WORD i = circle->lineNb - 1; i > 0; i--) {
        WORD m = circle->offsets[i >> 1];
        circle->offsets[i] = (m + n) / 2;
        n = m;
    }
}

/**
 * Draw a line with width > 1.
 */
static void vdi_WideLine_draw(vdi_VirtualWorkstation * RESTRICT vwk, Point * RESTRICT point, WORD count) {
    /* Don't attempt wide lining on a degenerate polyline */
    if (count < 2)
        return;

    vdi_Circle *circle = &vdi_sharedBuffer.circle;
        
    /* See if we need to rebuild vdi_context.q_circle[] */
    if (circle->lineWidth != vwk->line_width) {
        circle->lineWidth = vwk->line_width;
        vdi_WideLine_computeCircle(circle);
    }

    /* If the ends are arrowed, output them. */
    if ((vwk->line_beg | vwk->line_end) & vdi_LineEndStyle_arrow)
        vdi_Arrow_draw(vwk, point, count);

    FillAreaAttributes fillArea;
    vdi_saveFillAreaAttributes(vwk, &fillArea);

    /* Initialize the starting point for the loop. */
    WORD wx1 = point->x, wy1 = point->y;

    /* Determine if the line is a closed polyline */
    Point *ptr = point + count - 1;    /* point to last vertex */
    bool closed = false;
    if ((ptr->x == wx1) && (ptr->y == wy1))
        closed = true;

    /*
     * If the end style for the first point is not squared,
     * or the polyline is closed, output a circle
     */
    if ((fillArea.beginningStyle != vdi_LineEndStyle_square) || closed)
        vdi_Circle_draw(vwk, circle, wx1, wy1);

    /* Loop over the number of points passed in. */
    WORD k;
    for (WORD i = 1; i < count; i++) {
        /* Get ending point for line segment */
        point++;
        WORD wx2 = point->x;
        WORD wy2 = point->y;

        /* Get vector from start to end of the segment. */
        WORD vx = wx2 - wx1;
        WORD vy = wy2 - wy1;

        /* Ignore lines of zero length. */
        if (vx == 0 && vy == 0)
            continue;

        /* Calculate offsets to fatten the line. */
        if (vx == 0) { /* line is vertical - do it the simple way */
            vx = circle->offsets[0];
            vy = 0;
        } else if (vy == 0) { /* line is horizontal - even simpler */
            vx = 0;
            vy = circle->lineNb - 1;
        } else {              /* neither */
            /* Find the offsets in x and y for a point perpendicular */
            /* to the line segment at the appropriate distance. */
            k = mul_div(-vy, vdi_deviceSizeY, vdi_deviceSizeX);
            vy = mul_div(vx, vdi_deviceSizeX, vdi_deviceSizeY);
            vx = k;
            vdi_WideLine_computePerpendicularOffset(circle, &vx, &vy);
        }

        /* Prepare the control and points parameters for the polygon call. */
        Point box[5]; /* box must be large enough to close polygon */
        ptr = box;
        ptr->x = wx1 + vx;
        ptr->y = wy1 + vy;

        ptr++;
        ptr->x = wx1 - vx;
        ptr->y = wy1 - vy;

        ptr++;
        ptr->x = wx2 - vx;
        ptr->y = wy2 - vy;

        ptr++;
        ptr->x = wx2 + vx;
        ptr->y = wy2 + vy;

        vdi_Polygon_fill(vwk, box, 4);

        /*
         * If the terminal point of the line segment is an internal joint,
         * or the end style for the last point is not squared,
         * or the polyline is closed, output a filled circle
         */
        if ((i < count - 1) || (fillArea.endingStyle != vdi_LineEndStyle_square) || closed)
            vdi_Circle_draw(vwk, circle, wx2, wy2);

        /* end point becomes the starting point for the next line segment. */
        wx1 = wx2;
        wy1 = wy2;
    }

    /* Restore the attribute environment. */
    vdi_restoreFillAreaAttributes(vwk, &fillArea);
}

static void vdi_v_pline(vdi_VirtualWorkstation * vwk) {
    Point * point = (Point*)lineaVars.PTSIN;
    WORD count = lineaVars.parameters.contrl->inputVertexNb;

    vdi_Line_setMask(vwk);

#if vdi_Bezier_enabled
    /* check, if we want to draw a bezier curve */
    if (lineaVars.parameters.contrl->subOpcode == 13 && vwk->bez_qual ) /* FIXME: bez_qual ok?? */
        v_bez(vwk, point, count);
    else
#endif
    {
        if (vwk->line_width == 1) {
            vdi_Line_drawPoly(vwk, point, count, vwk->line_color, false);
            if ((vwk->line_beg | vwk->line_end) & vdi_LineEndStyle_arrow)
                vdi_Arrow_draw(vwk, point, count);
        } else
            vdi_WideLine_draw(vwk, point, count);
    }
}

//--------------------------------------------------------------------------------
// Bezier.
//--------------------------------------------------------------------------------
#if vdi_Bezier_enabled

/*
 * We conform to the PC-GEM/3 file standard with the inclusion of bezier
 * curve rendering capability with the v_bez() and v_bez_fill() calls.
 *
 * In 2D geometry, a vertex is a corner of a polygon (where two sides meet)
 *
 * Quadratic Bézier curves
 *
 * A quadratic Bézier curve is the path traced by the function P(t). For
 * points A, B, and C.
 *
 * Points on a quadratic Bézier curve can be computed recursively:
 *
 * 1. Let A, B and C be the startpoint, control point and endpoint of the curve.
 * 2. Let D be the midpoint of the line AB.
 * 3. Let E be the midpoint of the line BC.
 * 4. Let F be the midpoint of the line DE. F is a point on the curve.
 * 5. Recurse with A = A, B = D and C = F.
 * 6. Recurse with A = F, B = E and C = C.
 *
 * You can easily try this out on a piece of paper.
 */

#define vdi_Bezier_qualityMin 0
#define vdi_Bezier_isBezierPoint(f) ((f&1)!=0)
#define vdi_Bezier_isJumpPoint(f) ((f&2)!=0)

/*
 * Compute bezier function by difference method
 * last point is included
 * one dimension only, so use alternate elements of array & px
 * array[0] : anchor 1
 * array[2] : control 1
 * array[4] : control 2
 * array[6] : anchor 2
 */
static void vdi_Bezier_generateSegments(WORD *const array, WORD *px, const int bez_qual, WORD * pmin, WORD * pmax, WORD fill) {
    /*** calculate 1st, 2nd & 3rd order differences ***/
    LONG d1x = (LONG)array[2]-array[0];
    LONG d2x = (LONG)array[4]-array[2];
    LONG d3x = -array[0] - 3*d2x + array[6];
    d2x -= d1x;

    int q = 3 * bez_qual;
    if (!fill && q >= 3) {
        d1x>>=1;
        d2x>>=1;
        d3x>>=1;
        q--;
    }

    d1x = ((3L * d1x) << (2 * bez_qual)) + ((3L * d2x) << bez_qual) + d3x;
    /* assert( d1x <=  0x5f408000 ); */
    /* assert( d1x >= -0x5f408000 ); */

    d3x = 6L*d3x;
    /* assert( d3x <=  0xbffe8 ); */
    /* assert( d3x >= -0xbffe8 ); */

    d2x = ((6L*d2x)<<bez_qual) + d3x;
    /* assert( d2x <=  0x2f6fa12 ); */
    /* assert( d2x >= -0x2f6fa12 ); */

    LONG x0 = absLong(array[0]);
    int qd = 0;
    while( x0 >= (0x7fffffffL>>q) )
        q--, qd++;

    x0 = (((LONG)array[0]) << q) + (1L << (q - 1));

    for (int i = 1 << bez_qual; i > 0; i--) {
            WORD x = (WORD)(x0 >> q);
        *px = x;
        if ( x < *pmin )
            *pmin = x;
        if ( x > *pmax )
            *pmax = x;
        px+=2;

        if (absLong( (x0 >> 1) + (d1x >> (qd + 1)) ) >= 0x3ffffffeL) {
            /** halve scale to avoid overflow **/
            x0 >>= 1;
            q--;
            qd++;
            /* assert( absLong(x0+(d1x>>qd)) >= 0x40000000L ); */
        }

        x0 += d1x >> qd;
        if ( qd > 0 && absLong(x0) < 0x40000000L ) {
            /** double scale to maximise accuracy **/
            x0 = (x0 << 1)|1;
            q++, qd--;
        }

        /* assert( d2x<0 || d1x <=  0x7fffffff-d2x ); */
        /* assert( d2x>0 || d1x >= -0x7fffffff-d2x ); */
        d1x += d2x;
        d2x += d3x;
        /* assert( d2x <=  0x30bf9e8 ); */
        /* assert( d2x >= -0x30bf9e8 ); */
    }

    /** add the last point .. */
    {
        WORD x = array[6];
        *px = x;
        if ( x < *pmin )
            *pmin = x;
        if ( x > *pmax )
            *pmax = x;
    }
}

/*
 * Draw segments in xptsin array
 * do piece-wise if input array is too big
 * not guaranteed to work if vdi_Bezier_Mode_fill, but still better than anything else
 */

/* Mode values for vdi_Bezier_drawSegments() */
#define vdi_Bezier_Mode_fill 9 /* v_pline() */
#define vdi_Bezier_Mode_noFill 6 /* v_fill_area() */

static void vdi_Bezier_drawSegments(vdi_VirtualWorkstation * vwk, WORD nr_vertices, Point * point, WORD mode) {
    if (nr_vertices >= 2) {
        nr_vertices = min(nr_vertices, lineaVars.workstation_inquireTable[14]);

        /* output to driver, converting ndc if necessary */
        if (mode == vdi_Bezier_Mode_fill) {
            vdi_Polygon_fill(vwk, point, nr_vertices);
        } else {
            vdi_DrawContext dc;
            vdi_Line_drawStart(&dc, vwk, vwk->clippingEnabled, vwk->wrt_mode, vwk->line_color);
            dc.line.lastLineFlag = false;
            for (WORD i = nr_vertices - 1; i > 0; i--) {
                dc.line.line.p[0] = *point++;
                dc.line.line.p[1] = *point;
                vdi_getDriver()->drawLine(&dc);
            }
            vdi_Line_drawEnd(&dc);
        }
    }
}

/*
 * Draw a bezier curve
 *
 * outputs a (possibly disjoint) series of bezier curves & polylines.
 *
 * Each element in bezarr[] is a flag that controls the behaviour of the
 * corresponding input point.
 *
 * If bit 0 (ie ls bit) of the flag is set to one, the corresponding
 * point and the next three points define a bezier curve:
 *         1. start point
 *         2. 1st control point
 *         3. 2nd control point
 *         4. end point
 *
 * If bit 0 is zero, the corresponding point is part of a polyline.
 *
 * If bit 1 is set, the corresponding point starts a new disconnected
 * bezier curve.
 *
 * Note: The C function calls are as described here, but internally the C
 * libraries byte swap bezarr[] for intel compatible format.
 * If you are not using the C library, but directly programming the VDI
 * interface, you need to do the byte swapping yourself.
 */
void vdi_v_bez(vdi_VirtualWorkstation * vwk, Point * ptsget, int nr_ptsin) {
    /* WORD  const nr_ptsin = lineaVars.parameters.contrl->inputVertexNb; */
    UBYTE * bezarr = (UBYTE*)lineaVars.INTIN;
    WORD total_vertices = nr_ptsin;
    WORD total_jumps = 0;
    Point ptsbuf[CONF_VDI_MAX_VERTICES];
    /* Point * ptsget = (Point*)lineaVars.PTSIN; */
    Point * ptsput = ptsbuf;

    WORD bez_qual = vwk->bez_qual;
    UWORD vertices_per_bez = 1 << bez_qual;
    WORD xmin = 32767, ymin = 32767;
    WORD xmax = 0, ymax = 0;

    int i = 0;
    while(i < nr_ptsin) {
        int flag = bezarr[i^1];         /* index with xor 1 to byte swap !! */

        if (vdi_Bezier_isBezierPoint(flag)) {
            /* bezier start point found */
            if (i+3 >= nr_ptsin)
                break;                  /* incomlete curve, omit it */

            if (vdi_Bezier_isJumpPoint(flag))
                total_jumps++;          /* count jump point */

            /* generate line segments from bez points */
            vdi_Bezier_generateSegments(&ptsget->x, &ptsput->x, bez_qual, &xmin, &xmax, vwk->xfm_mode);    /* x coords */
            vdi_Bezier_generateSegments(&ptsget->y, &ptsput->y, bez_qual, &ymin, &ymax, vwk->xfm_mode);    /* y coords */

            /* skip to coord pairs at end of bez curve */
            i += 3;
            ptsget += 3;
            total_vertices += vertices_per_bez-3;
            vdi_Bezier_drawSegments(vwk, vertices_per_bez+1, ptsbuf, vdi_Bezier_Mode_noFill);
        }
        else {
            /* polyline */
            WORD output_vertices = 0;
            Point * point = ptsget;
            do {
                int t;

                t = point->x;
                if ( t < xmin )
                    xmin = t;
                if ( t > xmax )
                    xmax = t;

                t = point->y;
                if ( t < ymin )
                    ymin = t;
                if ( t > ymax )
                    ymax = t;

                output_vertices++;
                if ( vdi_Bezier_isBezierPoint(flag) )
                    break;              /* stop if a jump point is next */

                /* continue polyline */
                i++;
                if (i >= nr_ptsin)
                    break;

                ptsget += 1;
                {
                    int old_flag = flag;
                    flag = bezarr[i^1];
                    if (!vdi_Bezier_isJumpPoint(old_flag) && vdi_Bezier_isJumpPoint(flag))
                        total_jumps++;   /* count jump point */
                }
            } while( !vdi_Bezier_isJumpPoint(flag) );
            vdi_Bezier_drawSegments(vwk, output_vertices, point, vdi_Bezier_Mode_noFill);
        }
    }

    lineaVars.INTOUT[0] = total_vertices; /* total nr points */
    lineaVars.INTOUT[1] = total_jumps;    /* total moves */
    lineaVars.parameters.contrl->outputIntNb = 2;
    lineaVars.parameters.contrl->outputVertexNb = 2;
    lineaVars.PTSOUT[0] = xmin;
    lineaVars.PTSOUT[1] = ymin;
    lineaVars.PTSOUT[2] = xmax;
    lineaVars.PTSOUT[3] = ymax;
}

/*
 * Draw a filled bezier curve
 *
 * It is similar to v_bez(), but it forms a closed contour and fills
 * it with the current fill pattern.
 */
#if 1

void vdi_v_bez_fill(vdi_VirtualWorkstation * vwk, Point * ptsget, int nr_ptsin) {
    /* WORD  const nr_ptsin = lineaVars.parameters.contrl->inputVertexNb; */
    UBYTE * bezarr = (UBYTE*)lineaVars.INTIN;
    WORD total_vertices = nr_ptsin;
    WORD total_jumps = 0;
    WORD output_vertices = 0;
    Point ptsbuf[CONF_VDI_MAX_VERTICES];
    /* Point * ptsget = (Point*)lineaVars.PTSIN; */
    Point * ptsput = ptsbuf;

    WORD bez_qual = vwk->bez_qual;
    UWORD vertices_per_bez = 1 << bez_qual;
    WORD xmin = 32767, ymin = 32767;
    WORD xmax = 0, ymax = 0;

    int i = 0;
    while(i < nr_ptsin) {
        int flag = bezarr[i^1]; /* index with xor 1 to byte swap !! */

        if (vdi_Bezier_isBezierPoint(flag)) {
            /* bezier start point found */
            if (i+3 >= nr_ptsin)
                break;

            if (vdi_Bezier_isJumpPoint(flag))
                total_jumps++;   /* count jump point */

            /* generate line segments from bez points */
            vdi_Bezier_generateSegments(&ptsget->x, &ptsput->x, bez_qual, &xmin, &xmax, vwk->xfm_mode);
            vdi_Bezier_generateSegments(&ptsget->y, &ptsput->y, bez_qual, &ymin, &ymax, vwk->xfm_mode);

            /* skip to coord pairs at end of bez curve */
            i += 3;
            ptsget += 3;
            total_vertices += vertices_per_bez-3;

            output_vertices += vertices_per_bez+1;
            ptsput = ptsbuf + output_vertices;
            /* vdi_Bezier_drawSegments(vwk, vertices_per_bez+1, ptsbuf, vdi_Bezier_Mode_fill ); */
        }
        else {
            /* polyline */
            /* WORD output_vertices = 0; */
            Point * point = ptsget;
            do {
                int t;

                t = point->x;
                if ( t < xmin )
                    xmin = t;
                if ( t > xmax )
                    xmax = t;

                t = point->y;
                if ( t < ymin )
                    ymin = t;
                if ( t > ymax )
                    ymax = t;

                output_vertices++;
                if ( vdi_Bezier_isBezierPoint(flag) )
                    break;              /* stop if a jump point is next */

                /* continue polyline */
                i++;
                if (i >= nr_ptsin)
                    break;

                ptsget += 1;
                {
                    int old_flag = flag;
                    flag = bezarr[i^1];
                    if (!vdi_Bezier_isJumpPoint(old_flag) && vdi_Bezier_isJumpPoint(flag))
                        total_jumps++;   /* count jump point */
                }
            } while( !vdi_Bezier_isJumpPoint(flag) );
        }

        /* draw segments and reset all vertex information */
        vdi_Bezier_drawSegments(vwk, output_vertices, ptsbuf, vdi_Bezier_Mode_fill);
        bez_qual = vwk->bez_qual;
        /* ptsget0 = ptsget; */
        ptsput = ptsbuf;
        output_vertices = 0;
    }

    lineaVars.INTOUT[0] = total_vertices; /* total nr points */
    lineaVars.INTOUT[1] = total_jumps;    /* total moves */
    lineaVars.parameters.contrl->outputIntNb = 2;
    lineaVars.parameters.contrl->outputVertexNb = 2;
    lineaVars.PTSOUT[0] = xmin;
    lineaVars.PTSOUT[1] = ymin;
    lineaVars.PTSOUT[2] = xmax;
    lineaVars.PTSOUT[3] = ymax;
}

#else

void vdi_v_bez_fill(vdi_VirtualWorkstation * vwk, Point * ptsget, int nr_ptsin) {
    /* WORD  const nr_ptsin = lineaVars.parameters.contrl->inputVertexNb; */
    const UBYTE *const bezarr = (UBYTE*)lineaVars.INTIN;  /* index with xor 1 to byte swap !! */
    WORD total_vertices = nr_ptsin;
    WORD total_jumps = 0;
    WORD output_vertices = 0;
    Point ptsbuf[CONF_VDI_MAX_VERTICES];
    /* Point * ptsget = (Point*)lineaVars.PTSIN; */
    Point * ptsget0 = ptsget;
    Point * ptsput = ptsbuf;

    WORD bez_qual = vwk->bez_qual;
    WORD vertices_per_bez = 1 << bez_qual;
    WORD xmin = 32767, ymin = 32767;
    WORD xmax = 0, ymax = 0;

    WORD i = 0, i0 = 0;
    while (i < nr_ptsin) {
        int flag = bezarr[i^1];

        if (vdi_Bezier_isBezierPoint( flag )) {
            if (i+3 >= nr_ptsin)
                break;   /* incomplete curve, omit it */

            if (vdi_Bezier_isJumpPoint(flag))
                total_jumps++;   /* count jump point */

            /* keep this curve within nr vertices for the driver's ptsget[]
             ** with one spare for the end point */
            if ( output_vertices+vertices_per_bez+1 > lineaVars.workstation_inquireTable[14] ) {
                if ( bez_qual > vdi_Bezier_qualityMin ) {
                    /* reduce bezier quality & start this polygon again */
                    bez_qual--;
                    i = i0;
                    ptsget = (Point*)lineaVars.PTSIN;
                    ptsget0 = ptsget;
                    ptsput = ptsbuf;
                    output_vertices = 0;
                    continue;
                }
                /* too bad if we get here. refuse to add vertices to output */
            }
            else {
                if ( i != i0 ) {
                    /* the end point will be copied in again */
                    ptsput -= 1;
                    output_vertices--;
                }

                output_vertices += vertices_per_bez+1;
                total_vertices += vertices_per_bez-3;
                vdi_Bezier_generateSegments(&ptsget->x, &ptsput->x, bez_qual, &xmin, &xmax, vwk->xfm_mode);        /* x coords */
                vdi_Bezier_generateSegments(&ptsget->y, &ptsput->y, bez_qual, &ymin, &ymax, vwk->xfm_mode);        /* y coords */
                ptsput = ptsbuf + output_vertices;
            }
            /* assert( lineaVars.PTSIN + 2*i == ptsget ); */
            i+=3;
            ptsget += 3;
            flag = bezarr[i^1];
        }
        else {
            /** polyline **/
            if (i != i0) {
                /* the end point will be copied in again */
                ptsput -= 1;
                output_vertices--;
            }

            do {
                int t;
                if ( output_vertices < lineaVars.workstation_inquireTable[14] ) {  /* need room for at least one more */
                    t = ptsget->x;
                    if ( t < xmin )
                        xmin = t;
                    if ( t > xmax )
                        xmax = t;
                    ptsput->x = t;

                    t = ptsget->y;
                    if ( t < ymin )
                        ymin = t;
                    if ( t > ymax )
                        ymax = t;
                    ptsput->y = t;

                    ptsput++;
                    output_vertices++;
                }

                /* assert( ptsbuf + output_vertices == ptsput ); */
                /* assert( lineaVars.PTSIN + 2*i == ptsget ); */
                if ( vdi_Bezier_isBezierPoint(flag) )
                    break;
                i++;
                if (i >= nr_ptsin)
                    break;

                ptsget += 1;
                /* continue polyline, stop if a jump point is next */
                {
                    int old_flag = flag;
                    flag = bezarr[i^1];
                    if (!vdi_Bezier_isJumpPoint(old_flag) && vdi_Bezier_isJumpPoint(flag))
                        total_jumps++;   /* count jump point */
                }
            } while( !vdi_Bezier_isJumpPoint(flag) );
        }

        if ( i >= nr_ptsin || vdi_Bezier_isJumpPoint(flag) ) {
            vdi_Bezier_drawSegments(vwk, output_vertices, ptsbuf, vdi_Bezier_Mode_fill);
            bez_qual = vwk->bez_qual;
            i0 = i;
            ptsget0 = ptsget;
            ptsput = ptsbuf;
            output_vertices = 0;
        }
    }

    lineaVars.INTOUT[0] = total_vertices; /* total nr points */
    lineaVars.INTOUT[1] = total_jumps;    /* total moves */
    lineaVars.parameters.contrl->outputIntNb = 2;
    lineaVars.parameters.contrl->outputVertexNb = 2;
    lineaVars.PTSOUT[0] = xmin;
    lineaVars.PTSOUT[1] = ymin;
    lineaVars.PTSOUT[2] = xmax;
    lineaVars.PTSOUT[3] = ymax;
}
#endif

/*
 * Implement vdi function v_bez_control
 * simply returns the bezier quality set for the current workstation.
 * It can be used by your application to test if the GDOS supports
 * bezier functions. We do not support the use of this function to
 * enable or disable bezier curves.
 */
void vdi_v_bez_control(vdi_VirtualWorkstation * vwk) {
    lineaVars.INTOUT[0] = vwk->bez_qual;
    lineaVars.parameters.contrl->outputIntNb = 1;
}

/*
 * Set bezier quality
 * sets the quality of a bezier curve.  A bezier curve is generated
 * as a series of short straight line segments.  A high quality
 * bezier curve is made from many short straight lines, whereas a
 * lower quality bezier curve has fewer longer straight line segments.
 * Higher quality bezier curves thus appear smoother, but are slower.
 *
 * note: bez_qual > 7 will cause overflow in vdi_Bezier_generateSegments()
 */
#define vdi_Bezier_qualityMin 0
void vdi_v_bez_qual(vdi_VirtualWorkstation * vwk) {
    static const UBYTE pcarr[] = {0, 10, 23, 39, 55, 71, 86, 100};
    
    int q = lineaVars.INTIN[2];
    if ( q >= 95 )
        q = 7;
    else if ( q<5 )
        q = vdi_Bezier_qualityMin;
    else
        q = (q>>4) + 1;

    vwk->bez_qual = q;
    lineaVars.INTOUT[0] = pcarr[q - vdi_Bezier_qualityMin];
    lineaVars.parameters.contrl->outputIntNb = 1;
}

static void vdi_Bezier_initialize(vdi_VirtualWorkstation * vwk) {
    vwk->bez_qual = 7;
    vwk->bezier.available = 1;
    vwk->bezier.depth_scale.min = 9;
    vwk->bezier.depth_scale.max = 0;
    vwk->bezier.depth.min = 2;
    vwk->bezier.depth.max = 7;
}

#endif

//--------------------------------------------------------------------------------
// Seed filling.
//--------------------------------------------------------------------------------
/*
 * Calls the routine pointed to by lineaVars.seedFilling_abortCallback
 * on a regular basis to determine whether to prematurely abort the fill.
 * we initialise lineaVars.seedFilling_abortCallback to point to the routine below, which never
 * requests an early abort.
 */
static WORD vdi_SeedFilling_noAbort(void) {
    return 0;
}

static void vdi_v_contourfill(vdi_VirtualWorkstation * vwk) {
    vdi_DrawContext dc;
    vdi_DrawContext_setupPlaneNb(&dc);
    vdi_DrawContext_setupFilling(&dc, vwk, vwk->fill_color);
    vdi_DrawContext_setupClipping(&dc, vwk);
    dc.seedFilling.abort = vdi_SeedFilling_noAbort;
    dc.seedFilling.startX = lineaVars.PTSIN[0];
    dc.seedFilling.startY = lineaVars.PTSIN[1];
    dc.seedFilling.searchColor = lineaVars.INTIN[0];
    vdi_getDriver()->seedFill(&dc);
}

//--------------------------------------------------------------------------------
// Blitting.
//--------------------------------------------------------------------------------
/*
 * The following is a modified version of a blitter emulator, with the HOP
 * processing removed since it is always called with a HOP value of 2 (source).
 */
static void vdi_BitBlt_blit(vdi_BlitParameters *blit_info) {
    vdi_getDriver()->blit(blit_info);
}

//--------------------------------------------------------------------------------
// Raster copy.
//--------------------------------------------------------------------------------
/* flag:1 SOURCE and PATTERN   flag:0 SOURCE only */
#define vdi_Raster_patternFlag 16

/* lineaVars.PTSIN ARRAY OFFSETs */
#define vdi_Raster_ptsin_XMIN_S  0       /* x of upper left of source rectangle */
#define vdi_Raster_ptsin_YMIN_S  1       /* y of upper left of source rectangle */
#define vdi_Raster_ptsin_XMAX_S  2       /* x of lower right of source rectangle */
#define vdi_Raster_ptsin_YMAX_S  3       /* y of lower right of source rectangle */

#define vdi_Raster_ptsin_XMIN_D  4       /* x of upper left of destination rectangle */
#define vdi_Raster_ptsin_YMIN_D  5       /* y of upper left of destination rectangle */
#define vdi_Raster_ptsin_XMAX_D  6       /* x of lower right of destination rectangle */
#define vdi_Raster_ptsin_YMAX_D  7       /* y of lower right of destination rectangle */

/*
 * If bit 5 of mode is set, use pattern with blit
 */
static void vdi_Raster_setupPattern(struct vdi_RasterInfos *raster, vdi_BlitParameters *info) {   
    info->p_nxpl = raster->multifill ? 32 : 0; /* multi-plane pattern? */
    info->p_nxln = 2; /* offset to next line in pattern */
    info->p_mask = 0xf; /* pattern index mask */
}

/*
 * Clip, if dest is screen and clipping is wanted
 * return true, if clipping took away everything
 */
/* not fully optimized yet*/
static bool vdi_Raster_doClip(ClippingRect *clipRect, vdi_BlitParameters *info) {
    WORD *ptsin = lineaVars.PTSIN;

    /* clip Xmin source and destination to window */
    WORD s_xmin = ptsin[vdi_Raster_ptsin_XMIN_S];
    WORD d_xmin = ptsin[vdi_Raster_ptsin_XMIN_D];
    WORD clip = clipRect->xMin;

    /* Xmin dest < Xmin clip */
    if (d_xmin < clip) { /* Xmin dest > Xmin clip => branch */
        s_xmin -= d_xmin - clip; /* subtract amount clipped in x */
        d_xmin = clip; /* clip Xmin dest */
    }
    info->s_xmin = s_xmin; /* clipped Xmin source */
    info->d_xmin = d_xmin; /* clipped Xmin destination */

    /* clip Xmax destination to window */
    WORD d_xmax = ptsin[vdi_Raster_ptsin_XMAX_S] - s_xmin + d_xmin;
    clip = clipRect->xMax;

    /* Xmax dest > Xmax clip */
    if (d_xmax > clip)
        d_xmax = clip; /* clip Xmax dest */
    info->d_xmax = d_xmax;

    /* match source and destination rectangles */
    WORD deltax = d_xmax - d_xmin;
    if (deltax < 0)
        return true; /* block entirely clipped */
    info->b_wd = deltax + 1;
    info->s_xmax = s_xmin + deltax; /* d4 <- Xmax Source */

    /* clip Ymin source and destination to window */
    WORD s_ymin = ptsin[vdi_Raster_ptsin_YMIN_S];
    WORD d_ymin = ptsin[vdi_Raster_ptsin_YMIN_D];
    clip = clipRect->yMin;

    /* Ymin dest < Ymin clip => clip Ymin */
    if (d_ymin < clip) {
        s_ymin -= d_ymin - clip; /* subtract amount clipped in y */
        d_ymin = clip; /* clip Ymin dest */
    }
    info->s_ymin = s_ymin; /* Dy Source */
    info->d_ymin = d_ymin; /* Ymax destination */

    /* clip Ymax destination to window */
    WORD d_ymax = ptsin[vdi_Raster_ptsin_YMAX_S] - s_ymin + d_ymin;
    clip = clipRect->yMax;

    /* if Ymax dest > Ymax clip */
    if (d_ymax > clip)
        d_ymax = clip; // clip Ymax dest: Ymax dest = Ymax clip
    info->d_ymax = d_ymax;

    /* match source and destination rectangles */
    WORD deltay = d_ymax - d_ymin;
    if ( deltay < 0 )
        return true; /* block entirely clipped */
    info->b_ht = deltay + 1;
    info->s_ymax = s_ymin + deltay; /* Ymax Source */

    return false;
}

/*
 * Clip, if dest is screen and clipping is wanted.
 */
static void vdi_Raster_dontClip(vdi_BlitParameters * RESTRICT info) {
    WORD * RESTRICT ptsin = lineaVars.PTSIN;

    /* source */
    info->s_xmin = ptsin[vdi_Raster_ptsin_XMIN_S]; /* d0 x of upper left of source */
    info->s_ymin = ptsin[vdi_Raster_ptsin_YMIN_S]; /* d1 y of upper left of source */
    info->s_xmax = ptsin[vdi_Raster_ptsin_XMAX_S]; /* d4 x of lower right of source */
    info->s_ymax = ptsin[vdi_Raster_ptsin_YMAX_S]; /* d5 y of lower right of source */

    /* width and height of block in pixels */
    info->b_wd = info->s_xmax - info->s_xmin + 1;
    info->b_ht = info->s_ymax - info->s_ymin + 1;

    /* destination */
    info->d_xmin = ptsin[vdi_Raster_ptsin_XMIN_D]; /* d2 x of upper left of dest. */
    info->d_ymin = ptsin[vdi_Raster_ptsin_YMIN_D]; /* d3 y of upper left of dest. */
    info->d_xmax = ptsin[vdi_Raster_ptsin_XMAX_D]; /* d6 x of lower right of dest. */
    info->d_ymax = ptsin[vdi_Raster_ptsin_YMAX_D]; /* d7 y of lower right of dest. */
}

/*
 * Fill the info structure with vdi_RasterData values.
 */
static bool vdi_Raster_setupInfo(struct vdi_RasterInfos *raster, vdi_BlitParameters * info) {
    /* Get the pointers to the MFDBs */
    vdi_RasterData *src = *(vdi_RasterData **)&lineaVars.CONTRL[7];
    vdi_RasterData *dst = *(vdi_RasterData **)&lineaVars.CONTRL[9];

    /* setup plane info for source vdi_RasterData */
    if (src->fd_addr) {
        /* for a positive source address */
        info->s_form = src->fd_addr;
        info->s_nxwd = src->fd_nplanes * 2;
        info->s_nxln = src->fd_wdwidth * info->s_nxwd;
    } else {
        /* source form is screen */
        info->s_form = (UWORD*) v_bas_ad;
        info->s_nxwd = lineaVars.screen_planeNb * 2;
        info->s_nxln = lineaVars.screen_lineSize2;
    }

    /* setup plane info for destination vdi_RasterData */
    bool use_clip = false;
    if (dst->fd_addr) {
        /* for a positive address */
        info->d_form = dst->fd_addr;
        info->plane_ct = dst->fd_nplanes;
        info->d_nxwd = dst->fd_nplanes * 2;
        info->d_nxln = dst->fd_wdwidth * info->d_nxwd;
    } else {
        /* destination form is screen */
        info->d_form = (UWORD*) v_bas_ad;
        info->plane_ct = lineaVars.screen_planeNb;
        info->d_nxwd = lineaVars.screen_planeNb * 2;
        info->d_nxln = lineaVars.screen_lineSize2;

        /* check if clipping is enabled, when destination is screen */
        if (raster->clippingEnabled)
            use_clip = true;
    }

    if (use_clip) {
        if (vdi_Raster_doClip(raster->clippingRect, info))
            return true; /* clipping took away everything */
    } else
        vdi_Raster_dontClip(info);

    info->s_nxpl = 2; /* next plane offset (source) */
    info->d_nxpl = 2; /* next plane offset (destination) */

    /* only 8, 4, 2 and 1 planes are valid (destination) */
    return info->plane_ct & ~0x000f;
}

/* common functionality for vdi_vro_cpyfm, vdi_vrt_cpyfm, linea_raster */
static void vdi_Raster_copy(struct vdi_RasterInfos *raster) {
    vdi_Rect_sortCorners((Rect*)lineaVars.PTSIN);
    vdi_Rect_sortCorners((Rect*)(lineaVars.PTSIN+4));
    WORD mode = lineaVars.INTIN[0];

    /* if mode is made up of more than the first 5 bits */
    if (mode & ~0x001f)
        return; /* mode is invalid */

    /* check the pattern flag (bit 5) and revert to log op # */
    vdi_BlitParameters blitInfos;
    vdi_BlitParameters *info = &blitInfos;
    info->p_addr = NULL; /* get pattern pointer */
    if (mode & vdi_Raster_patternFlag) {
        mode &= ~vdi_Raster_patternFlag; /* set bit to 0! */
        vdi_Raster_setupPattern(raster, info); /* fill in pattern related stuff */
    }

    /* if true, the plane count is invalid or clipping took all! */
    if (vdi_Raster_setupInfo(raster, info))
        return;

    if (!raster->transparent) {
        /* COPY RASTER OPAQUE */
        /* planes of source and destination equal in number? */
        if (info->s_nxwd != info->d_nxwd)
            return;
        info->op_tab[0] = mode; /* fg:0 bg:0 */
        info->bg_col = 0;       /* bg:0 & fg:0 => only first OP_TAB */
        info->fg_col = 0;       /* entry will be referenced */
    } else {
        /*
         * COPY RASTER TRANSPARENT - copies a monochrome raster area
         * from source form to a color area. A writing mode and color
         * indices for both 0's and 1's are specified in the lineaVars.INTIN array.
         */
        /* is source area one plane? */
        if (info->s_nxwd != 2)
            return; /* source must be mono plane */
        info->s_nxpl = 0; /* use only one plane of source */
        /* background color */
        WORD fg_col = vdi_Color_validateIndex(lineaVars.INTIN[1]);
        fg_col = vdi_context.palette.penToPaletteTable[fg_col];
        /* foreground color */
        WORD bg_col = vdi_Color_validateIndex(lineaVars.INTIN[2]);
        bg_col = vdi_context.palette.penToPaletteTable[bg_col];
        switch (mode) {
        case vdi_WritingMode_transparent:
            info->op_tab[0] = BM_NOTS_AND_D; /* fg:0 bg:0  D' <- [not S] and D */
            info->op_tab[2] = BM_S_OR_D; /* fg:1 bg:0  D' <- S or D */
            info->fg_col = fg_col; /* were only interested in one color */
            info->bg_col = 0; /* save the color of interest */
            break;
        case vdi_WritingMode_replace:
            info->op_tab[0] = BM_ALL_WHITE; /* fg:0 bg:0  D' <- 0 */
            info->op_tab[1] = BM_NOT_S;     /* fg:0 bg:1  D' <- not S */
            info->op_tab[2] = BM_S_ONLY;    /* fg:1 bg:0  D' <- S */
            info->op_tab[3] = BM_ALL_BLACK; /* fg:1 bg:1  D' <- 1 */
            info->bg_col = bg_col;   /* save fore and background colors */
            info->fg_col = fg_col;
            break;
        case vdi_WritingMode_xor:
            info->op_tab[0] = BM_S_XOR_D; /* fg:0 bg:0  D' <- S xor D */
            info->bg_col = 0;
            info->fg_col = 0;
            break;
        case vdi_WritingMode_erasing:
            info->op_tab[0] = BM_S_AND_D;   /* fg:0 bg:0  D' <- S and D */
            info->op_tab[1] = BM_NOTS_OR_D; /* fg:0 bg:1  D' <- [not S] or D */
            info->fg_col = 0; /* were only interested in one color */
            info->bg_col = bg_col; /* save the color of interest */
            break;
        default:
            return; /* unsupported mode */
        }
    }

    vdi_BitBlt_blit(info);
}

static void vdi_vro_cpyfm(vdi_VirtualWorkstation * vwk) {
    struct vdi_RasterInfos raster;
    raster.clippingRect = &vwk->clippingRect;
    raster.clippingEnabled = vwk->clippingEnabled;
    raster.multifill = vwk->multiFillEnabled;
    raster.transparent = false;
    vdi_Raster_copy(&raster);
}

static void vdi_vrt_cpyfm(vdi_VirtualWorkstation * vwk) {
    struct vdi_RasterInfos raster;
    raster.clippingRect = &vwk->clippingRect;
    raster.clippingEnabled = vwk->clippingEnabled;
    raster.multifill = vwk->multiFillEnabled;
    raster.transparent = true;
    vdi_Raster_copy(&raster);
}

static void vdi_vr_trnfm(vdi_VirtualWorkstation * vwk) {
    LONG inner, outer;

    /* Get the pointers to the MFDBs */
    vdi_RasterData *src_mfdb = *(vdi_RasterData **)&lineaVars.CONTRL[7];
    vdi_RasterData *dst_mfdb = *(vdi_RasterData **)&lineaVars.CONTRL[9];

    WORD *src = src_mfdb->fd_addr;
    WORD *dst = dst_mfdb->fd_addr;
    WORD planes = src_mfdb->fd_nplanes;
    LONG size = (LONG)src_mfdb->fd_h * src_mfdb->fd_wdwidth; /* size of plane in words */
    bool inplace = (src==dst);

    if (src_mfdb->fd_stand) { /* source is standard format */
        dst_mfdb->fd_stand = 0; /* force dest to device-dependent */
        outer = planes; /* set outer & inner loop counts */
        inner = size;
    } else { /* source is device-dependent format */
        dst_mfdb->fd_stand = 1; /* force dest to standard */
        outer = size; /* set loop counts */
        inner = planes;
    }

    if (!inplace) { /* the simple option */
        for (LONG i = 0; i < outer; i++, dst++) {
            WORD *work = dst;
            for (LONG j = 0; j < inner; j++) {
                *work = *src++;
                work += outer;
            }
        }
        return;
    }

    /* handle in-place transform - can be slow (on Atari TOS too) */
    if (planes == 1)            /* for mono, there is no difference    */
        return;                 /* between standard & device-dependent */

    if (--outer <= 0)
        return;

    for (--inner; inner >= 0; inner--) {
        WORD *work;
        for (LONG i = 0, count = 0L; i < outer; i++) {
            src += inner + 1;
            WORD temp = *src;
            dst = src;
            work = src;
            count += inner;
            for (LONG j = 0; j < count; j++) {
                work = dst--;
                *work = *dst;
            }
            *dst = temp;
        }
        src = work;
    }
}

//--------------------------------------------------------------------------------
// Marker.
//--------------------------------------------------------------------------------
/* Marker definitions */
static const WORD vdi_Marker_dot[] = { 1, 2, 0, 0, 0, 0 };
static const WORD vdi_Marker_plus[] = { 2, 2, 0, -3, 0, 3, 2, -4, 0, 4, 0 };
static const WORD vdi_Marker_star[] = { 3, 2, 0, -3, 0, 3, 2, 3, 2, -3, -2, 2, 3, -2, -3, 2};
static const WORD vdi_Marker_square[] = { 1, 5, -4, -3, 4, -3, 4, 3, -4, 3, -4, -3 };
static const WORD vdi_Marker_cross[] = { 2, 2, -4, -3, 4, 3, 2, -4, 3, 4, -3 };
static const WORD vdi_Marker_diamond[] = { 1, 5, -4, 0, 0, -3, 4, 0, 0, 3, -4, 0 };

// Sets the height of markers
static void vdi_vsm_height(vdi_VirtualWorkstation * vwk) {
    /* Limit the requested marker height to a reasonable value. */
    WORD h = clipWord(lineaVars.PTSIN[1], lineaVars.workstation_sizeTable.markerHeightMin, lineaVars.workstation_sizeTable.markerHeightMax);

    /* Set the marker height internals and the return parameters. */
    vwk->mark_height = h;
    h = (h + lineaVars.workstation_sizeTable.markerHeightMin / 2) / lineaVars.workstation_sizeTable.markerHeightMin;
    vwk->mark_scale = h;
    WORD *pts_out = lineaVars.PTSOUT;
    *pts_out++ = h * lineaVars.workstation_sizeTable.markerWidthMin;
    *pts_out = h * lineaVars.workstation_sizeTable.markerHeightMin;
    vwk->yFlipped = true;
}

/*
 * Sets the current type of marker
 */
static void vdi_vsm_type(vdi_VirtualWorkstation * vwk) {
    WORD mk = checkRangeWord(lineaVars.INTIN[0], vdi_MarkerStyle_min, vdi_MarkerStyle_max, vdi_MarkerStyle_default);
    vwk->mark_index = mk - 1;
    lineaVars.INTOUT[0] = mk;
}

/*
 * Set marker color
 */
static void vdi_vsm_color(vdi_VirtualWorkstation * vwk) {
    WORD i = vdi_Color_validateIndex(lineaVars.INTIN[0]);
    lineaVars.INTOUT[0] = i;
    vwk->mark_color = vdi_context.palette.penToPaletteTable[i];
}

/*
 * Polymarker
 */
static void vdi_v_pmarker(vdi_VirtualWorkstation * vwk) {
    /* If this constant goes greater than 5, you must increase size of sav_points */
    #define MARKSEGMAX 5

    static const WORD * const markhead[] = {
        vdi_Marker_dot, vdi_Marker_plus, vdi_Marker_star, vdi_Marker_square, vdi_Marker_cross, vdi_Marker_diamond
    };

    /* Save the current polyline attributes which will be used. */
    WORD sav_index = vwk->line_index;
    WORD sav_color = vwk->line_color;
    WORD sav_width = vwk->line_width;
    WORD sav_beg = vwk->line_beg;
    WORD sav_end = vwk->line_end;

    /* Set the appropriate polyline attributes. */
    vwk->line_index = 0;
    vwk->line_color = vwk->mark_color;
    vwk->line_width = 1;
    vwk->line_beg = vdi_LineEndStyle_square;
    vwk->line_end = vdi_LineEndStyle_square;
    vwk->clippingEnabled = 1;

    WORD scale = vwk->mark_scale;

    /* Copy the lineaVars.PTSIN pointer since we will be doing polylines */
    WORD num_vert = lineaVars.parameters.contrl->inputVertexNb;
    WORD *old_ptsin = lineaVars.PTSIN;
    WORD *src_ptr = lineaVars.PTSIN;
    WORD sav_points[10];
    lineaVars.PTSIN = sav_points;

    /* Loop over the number of points. */
    for (WORD i = 0; i < num_vert; i++) {
        WORD *pts_in = src_ptr;
        WORD x_center = *pts_in++;
        WORD y_center = *pts_in++;
        src_ptr = pts_in;

        /* Get the pointer to the appropriate marker type definition. */
        const WORD *m_ptr = markhead[vwk->mark_index];
        WORD num_lines = *m_ptr++;

        /* Loop over the number of polylines which define the marker. */
        for (WORD j = 0; j < num_lines; j++) {
            WORD num_points = *m_ptr++;  /* How many points? Get them.  */
            lineaVars.parameters.contrl->inputVertexNb = num_points;
            pts_in = sav_points;
            for (WORD h = 0; h < num_points; h++) {
                *pts_in++ = x_center + scale * (*m_ptr++);
                *pts_in++ = y_center + scale * (*m_ptr++);
            }                   /* End for:  extract points. */

            /* Output the polyline. */
            const WORD *mrk_ptr = m_ptr;    /* Save for next pass */
            vdi_v_pline(vwk);
            m_ptr = mrk_ptr;
        }                       /* End for:  over the number of polylines
                                   defining the marker. */

    }                           /* End for:  over marker points. */

    /* Restore the lineaVars.PTSIN pointer */
    lineaVars.PTSIN = old_ptsin;

    /* Restore the current polyline attributes. */
    vwk->line_index = sav_index;
    vwk->line_color = sav_color;
    vwk->line_width = sav_width;
    vwk->line_beg = sav_beg;
    vwk->line_end = sav_end;
}

// Inquire current polymarker attributes
static void vdi_vqm_attributes(vdi_VirtualWorkstation * vwk) {
    WORD *intout = lineaVars.INTOUT;
    intout[0] = vwk->mark_index;
    intout[1] = vdi_context.palette.paletteToPenTable[vwk->mark_color];
    intout[2] = vwk->wrt_mode + 1;

    WORD *ptsout = lineaVars.PTSOUT;
    ptsout[0] = 0;
    ptsout[1] = vwk->mark_height;

    vwk->yFlipped = true;
}

//--------------------------------------------------------------------------------
// Text.
//--------------------------------------------------------------------------------
/*
 * the following structure mimics the format of the stack frame
 * containing the local variables used by the lower-level assembler
 * routines.  comments are taken from the assembler source.
 *
 * this could (and should) be cleaned up at some time, but any changes
 * MUST be synchronised with corresponding changes to the assembler code.
 */
typedef struct {
    WORD blt_flag;
    WORD destinationY;
    WORD characterSizeY;
    WORD destinationX;
    WORD characterSizeX;
    WORD writingMode;
    WORD font_style;
                        /* temps for arbitrary text scaling */
    WORD swap_tmps;         /* nonzero if temps are swapped */
    WORD tmp_dely;          /* temp characterSizeY,characterSizeX used by scaling */
    WORD tmp_delx;
                        /* colour, planes, etc */
    WORD nextwrd;           /* offset to next word in same plane */
    WORD nbrplane;          /* # planes */
    WORD forecol;           /* foreground colour */
                        /* masks for special effects */
    WORD thknover;          /* overflow for word thicken */
    WORD skew_msk;          /* vdi_Text_rotate this to check shift */
    WORD lite_msk;          /* AND with this to get light effect */
    WORD ambient;           /* background colour */
    WORD smear;             /* amount to increase width */
                        /* other general-usage stuff */
    WORD wrd_cnt;           /* number inner loop words for left/right */
    WORD shif_cnt;          /* shift count for use by left/right shift routines */
    WORD rota_msk;          /* overlap between words in inner loop */
    WORD left_msk;          /* fringes of destination to be affected */
    WORD rite_msk;
    WORD thk_msk;           /* right fringe mask, before thicken */
    WORD src_wthk;
    WORD src_wrd;           /* # full words between fringes (source) (before thicken) */
    WORD dest_wrd;          /* # full words between fringes (destination) */
    WORD tddad;             /* destination dot address */
    WORD tsdad;             /* source dot address (pixel address, 0-15 word offset) */
    WORD height;            /* height of area in pixels */
    WORD width;             /* width of area in pixels */
    WORD d_next;            /* width of dest form (_linea.screen_lineSize2 formerly used) */
    WORD s_next;            /* width of source form (formerly s_width) */
    UBYTE *dform;           /* start of destination form */
    UBYTE *sform;           /* start of source form */
    WORD buffa;             /* for clip & prerotate blt */
} vdi_Text_Context;

/* here we should have the preprocessor verify the length of vdi_Text_Context */

/*
 * the following table maps a 4-bit sequence to its reverse
 */
static const UBYTE vdi_Text_reverseNybbleTable[] =
/*  0000  0001  0010  0011  0100  0101  0110  0111  */
{   0x00, 0x08, 0x04, 0x0c, 0x02, 0x0a, 0x06, 0x0e,
/*  1000  1001  1010  1011  1100  1101  1110  1111  */
    0x01, 0x09, 0x05, 0x0d, 0x03, 0x0b, 0x07, 0x0f };

#if CONF_WITH_VDI_TEXT_SPEEDUP
/*
 * Output the font directly to the screen.
 * note: like Atari TOS, we assume that the font contains the full
 * character set, i.e. first_ade==0, last_ade==255
 */
static void vdi_Text_blitDirectlyToScreen(WORD characterCount, WORD *str) {
    WORD height = lineaVars.text_characterSizeY;
    WORD mode = lineaVars.writingMode;
    WORD srcStride = lineaVars.font_widthInBytes, dstStride = lineaVars.screen_lineSize2;
    WORD planeNb = lineaVars.screen_planeNb;
    WORD planeStride = (planeNb - 1) * sizeof(WORD);
    UWORD colorForeground = lineaVars.text_colorForeground;
    UBYTE *srcData = (UBYTE *)lineaVars.font_data;
    WORD destinationX = lineaVars.text_destinationX, destinationY = lineaVars.text_destinationY;
    UBYTE *dstData = (UBYTE *)vdi_getPixelAddress(destinationX, destinationY);
    if (destinationX & 0x0008)
        dstData++;
    LOOP_DO(characterIndex, characterCount) {
        UBYTE *src = srcData + *str++;
        UBYTE *dst = dstData;
        UWORD color = colorForeground;
        LOOP_DO(planeIndex, planeNb) {
            UBYTE *s = src, *d = dst;
            switch (mode) {
            default:
            case WM_REPLACE:
                if (color & 1)
                    LOOP_DO(y, height) { *d = *s; s += srcStride; d += dstStride; } LOOP_WHILE(y)
                else
                    LOOP_DO(y, height) { *d = 0; d += dstStride; } LOOP_WHILE(y)
                break;
            case WM_TRANS:
                if (color & 1)
                    LOOP_DO(y, height) { *d |= *s; s += srcStride; d += dstStride; } LOOP_WHILE(y)
                else
                    LOOP_DO(y, height) { *d &= ~*s; s += srcStride; d += dstStride; } LOOP_WHILE(y)
                break;
            case WM_XOR:
                LOOP_DO(y, height) { *d ^= *s; s += srcStride; d += dstStride; } LOOP_WHILE(y)
                break;
            case WM_ERASE:
                if (color & 1)
                    LOOP_DO(y, height) { *d |= ~*s; s += srcStride; d += dstStride; } LOOP_WHILE(y)
                else
                    LOOP_DO(y, height) { *d &= *s; s += srcStride; d += dstStride; } LOOP_WHILE(y)
                break;
            }
            dst += sizeof(WORD); /* next plane */
            color >>= 1;
        } LOOP_WHILE(planeIndex);
        dstData++;
        if (!IS_ODD_POINTER(dstData)) /* must go to next screen word */
            dstData += planeStride;
    } LOOP_WHILE(characterIndex);
}
#endif

#define vdi_Text_blitNormal vdi_Text_blitNormalC
void vdi_Text_blitNormal(vdi_Text_Context *vars, UBYTE *src, UBYTE *dst);

// Style equates converted to 68k bit numbers (bit number 0 is the least significant bit)
enum {
    vdi_Text_blit_thicken = 1 << 0,
    vdi_Text_blit_light = 1 << 1,
    vdi_Text_blit_skew = 1 << 2,
    vdi_Text_blit_under = 1 << 3,
    vdi_Text_blit_outline = 1 << 4,
    vdi_Text_blit_shadow = 1 << 5,
};

enum {
    vdi_Text_sgl_loop,
    vdi_Text_dbl_loop,
    vdi_Text_mlt_rite,
    vdi_Text_mlt_left,
};

#if 0

// thknop
// s contains the current word and the next word.
static ULONG vdi_Text_blitThicken(vdi_Text_Context *vars, ULONG s, UWORD mask) {
    UWORD thk_msk = vars->thk_msk;
    if (vars->skew_msk & 1) {
        if (thk_msk & 1)
            thk_msk = 0x8000;
        else
            thk_msk = 0x8000 | (thk_msk >> 1);
        vars->thk_msk = thk_msk;
    }
    if (vars->dest_wrd >= 0 && vars->rite_msk < thk_msk)
        s &= thk_msk;
    else
        s &= (ULONG)thk_msk << 16;
    WORD smear = vars->smear;
    s &= (((ULONG)mask << 16) | vars->rite_msk) << smear;
    smear--;
    do {
        s |= s >> 1;
    } while (--smear != -1);
    return s;
}

// thknopw
static UWORD vdi_Text_blitThickenW(vdi_Text_Context *vars, UWORD s) {
    vars->src_wrd--;
    if (vars->src_wrd <= 0)
        s &= vars->thk_msk;
    WORD smear = vars->smear;
    ULONG st = (ULONG)s << 16, d = st;
    smear--;
    do {
        st >>= 1;
        d |= st;
    } while (--smear != -1);
    UWORD thknover = vars->thknover;
    vars->thknover = d;
    return (d >> 16) | thknover;
}

// thknopwf
static UWORD vdi_Text_blitThickenWF(vdi_Text_Context *vars, UWORD s, UWORD mask) {
    WORD thk_msk = vars->thk_msk;
    if (--vars->src_wrd > 0) {
        // Starting left fringe, do left mask only.
        if (--vars->src_wrd == 0)
            s &= thk_msk;
        s &= vars->left_msk;
    } else {
        if (vars->src_wrd < 0)
            s = 0; // Right fringe, source data invalid, erase.
        else
            s &= thk_msk; // Right fringe, source data AND'd with source mask.
        // Compute mask & count for next line.
        if (vars->skew_msk >= 0) {
            WORD dest_wrd = vars->dest_wrd + 2;
            UWORD mask;
            if (vars->rite_msk & 1) {
                mask = 0x8000;
                dest_wrd++;
            } else
                mask = 0x8000 | (vars->rite_msk >> 1);
            if (vars->left_msk == 1)
                dest_wrd--;
            if (thk_msk & 1)
                thk_msk = 0x8000;
            else
                thk_msk = 0x8000 | (thk_msk >> 1);
            if (mask >= thk_msk)
                dest_wrd++;
            vars->thk_msk = thk_msk;
            vars->src_wthk = dest_wrd;
        }
        vars->src_wthk = vars->src_wrd;
    }
    WORD smear = vars->smear;
    ULONG d = (ULONG)s << 16, st = d;
    smear--;
    do {
        st >>= 1;
        d |= st;
    } while (--smear != -1);
    UWORD thknover = vars->thknover;
    vars->thknover = d;
    return ((d >> 16) | thknover) & mask;
}

// liteop
// s contains the current word and the next word.
static ULONG vdi_Text_blitLight(vdi_Text_Context *vars, ULONG s) {
    UWORD lite_msk = vars->lite_msk;
    s &= ((ULONG)lite_msk << 16) | lite_msk;
    rolw1(lite_msk); // For the next line.
    vars->lite_msk = lite_msk;
    return s;
}

// liteopw
static ULONG vdi_Text_blitLightW(vdi_Text_Context *vars, UWORD s) {
    return s & vars->lite_msk;
}

// liteopwf
static UWORD vdi_Text_blitLightWF(vdi_Text_Context *vars, UWORD s) {
    return s & vars->lite_msk;
}

// skewop
static ULONG vdi_Text_blitSkew(vdi_Text_Context *vars, ULONG s, UWORD mask, UBYTE **dst, UWORD *pixelDst) {
    UWORD skew_msk = vars->skew_msk;
    rolw1(skew_msk);
    vars->skew_msk = skew_msk;
    if (skew_msk & 0x8000) {
        s >>= 1;
        ULONG m = (((ULONG)mask << 16) | vars->rite_msk) >> 1;
        vars->rite_msk = (UWORD)m;
        vars->left_msk = (UWORD)(m >> 16);
        WORD shif_cnt = vars->shif_cnt;
        if (vars->left_msk) {
            if (shif_cnt >= 0)
                shif_cnt++;
            else {
                if ((shif_cnt & 0xff) != 0)
                    shif_cnt--;
                else
                    shif_cnt = 0;
            }
        } else {
            // We crossed a word boundary.
            vars->rite_msk = (UWORD)(m >> 16);
            vars->left_msk = (UWORD)m; // Move right mask to left mask.
            *dst += vars->nextwrd;
            *pixelDst = *(UWORD*)*dst;
            shif_cnt = (15 - shif_cnt) | 0x8000;
        }
        vars->shif_cnt = shif_cnt;
    }
    return s;
}

static bool vdi_Text_blitSkewW(vdi_Text_Context *vars, WORD blt_flag, UBYTE **dst) {
    UWORD skew_msk = vars->skew_msk;
    rolw1(skew_msk);
    vars->skew_msk = skew_msk;
    if (skew_msk & 0x8000) 
        return blt_flag;
    vars->rota_mask = 0x8000 | (vars->rota_mask >> 1);
    UWORD rite_msk = vars->rite_msk;
    if (rite_mask == 0xffff) {
        vars->dest_wrd++;
        vars->rite_msk = 0x8000;
    } else {
        vars->rite_msk = 0x8000 | (rite_mask >> 1);
    }
    WORD shif_cnt = vars->shif_cnt;
    if ((shif_cnt & 0xff) == 0)
        vars->rota_mask = 0x8000;
    UWORD left_msk = vars->left_msk >> 1;
    if (!left_msk) {
        vars->left_msk = left_msk;
        if (shif_cnt >= 0) {
            vars->shif_cnt = shif_cnt + 1;
            return vdi_Text_mlt_rite;
        }
        if ((shif_cnt & 0xff) != 0) {
            vars->shif_cnt = shif_cnt - 1;
            return vdi_Text_mlt_left;
        }
        vars->shif_cnt = 1;
        return vdi_Text_mlt_rite;
    }
    vars->left_msk = 0xffff;
    vars->dest_wrd--;
    *dst += vars->nextwrd;
    vars->shif_cnt = 0x80000 | (15 - shif_cnt);
    return vdi_Text_mlt_left;
}

static ULONG vdi_Text_processEffects(vdi_Text_Context *vars, ULONG s, UWORD mask) {
    WORD font_style = vars->font_style;
    if (font_style) {
//        if (font_style & vdi_Text_blit_skew)
//            s = vdi_Text_blitSkew(vars, s, mask);
        if (font_style & vdi_Text_blit_thicken)
            s = vdi_Text_blitThicken(vars, s, mask);
        if (font_style & vdi_Text_blit_light)
            s = vdi_Text_blitLight(vars, s);
    }
    return s;
}

static UWORD vdi_Text_processEffectsW(vdi_Text_Context *vars, UWORD s) {
    WORD font_style = vars->font_style;
    if (font_style) {
//        if (font_style & vdi_Text_blit_skew)
//            s = vdi_Text_blitSkewW(vars, s);
        if (font_style & vdi_Text_blit_thicken)
            s = vdi_Text_blitThickenW(vars, s);
        if (font_style & vdi_Text_blit_light)
            s = vdi_Text_blitLightW(vars, s);
    }
    return s;
}

static UWORD vdi_Text_processEffectsWF(vdi_Text_Context *vars, UWORD s, UWORD mask) {
    WORD font_style = vars->font_style;
    if (font_style) {
        if (font_style & vdi_Text_blit_thicken)
            s = vdi_Text_blitThickenWF(vars, s, mask);
        if (font_style & vdi_Text_blit_light)
            s = vdi_Text_blitLightWF(vars, s);
    }
    return s;
}

void vdi_Text_blitNormal(vdi_Text_Context *vars, UBYTE *src, UBYTE *dst) {
    if (vars->height <= 0) return;
//restart:
    WORD shiftTemp = vars->tddad - vars->tsdad, fringeTableIndex = shiftTemp;
    if (shiftTemp < 0) {
//        shiftTemp = -shiftTemp | 0x8000; // form 2's cmpliment for positive shift/count, fake a negative (stripped by ROR or ROL).
        fringeTableIndex += 16; // make word_mask_table index positive
    }
    vars->shif_cnt = shiftTemp; // save shift count (bit15=1 if ROL, else ROR)
    vars->rota_msk = ~vdi_fringeTable[fringeTableIndex];
    
    // Set up fringe masks.
    WORD dstWordPixel = vars->tddad;
    vars->left_msk = vdi_fringeTable[dstWordPixel];
    WORD rightSide = dstWordPixel + vars->width;
    // Thicken bug fix.
    UWORD thk_msk = ~vdi_fringeTable[(rightSide - vars->smear) & 0xf];
    vars->thk_msk = thk_msk;
    vars->skew_msk = 0x8000;
    // More than one word ?
    WORD blt_flag;
    WORD dest_wrd;
    UWORD rite_msk;
    if (rightSide <= 16) {
        // Fits in one word.
        vars->left_msk &= ~vdi_fringeTable[rightSide]; // merge both masks.
        blt_flag = vdi_Text_sgl_loop;
        dest_wrd = -1; // assume sgl_loop
        rite_msk = 0;
    } else {
        // Two fringe masks needed.
        dest_wrd = (rightSide >> 4) - 1;
        if (dest_wrd == 0 && (vars->tsdad + vars->width) <= 32)
            blt_flag = vdi_Text_dbl_loop;
        else if (shiftTemp >= 0)
            blt_flag = vdi_Text_mlt_rite;
        else
            blt_flag = vdi_Text_mlt_left;
        WORD rightSideWordPixel = rightSide & 0xf;
        if (rightSideWordPixel == 0) {
            // Last word is full so its a fringe.
            dest_wrd--;
            rightSideWordPixel = 16;
        }
        rite_msk = ~vdi_fringeTable[rightSideWordPixel];
    }
    vars->blt_flag = blt_flag;
    vars->dest_wrd = dest_wrd;
    vars->rite_msk = rite_msk;
    dest_wrd += 2;
    if (rite_msk >= thk_msk)
        dest_wrd++;
    vars->src_wthk = dest_wrd;
    vars->src_wrd = dest_wrd;

    // Plane loop.
    while (1) {
        WORD wrt_mod = vars->writingMode; // 000xxxxx (0x00-0x13 are valid)
        wrt_mod = (wrt_mod << 1) | (vars->forecol & 1); vars->forecol >>= 1; // Plane foreground bit: 00xxxxxf.
        wrt_mod = (wrt_mod << 1) | (vars->ambient & 1); vars->ambient >>= 1; // Plane background bit: 0xxxxxfb.
        static const UBYTE wrmappin[4*4 + 16*4]= {
//           00  01  10  11  fore/back
            // VDI modes
            0x0,0x0,0x3,0x3, // replace mode
            0x4,0x4,0x7,0x7, // transparent mode
            0x6,0x6,0x6,0x6, // XOR mode
            0x1,0x1,0xd,0xd, // inverse transparent mode
            // BitBlt modes
            0x0,0xf,0x0,0xf, // mode 0: all zeros
            0x0,0xe,0x1,0xf, // mode 1: source AND destination
            0x0,0xd,0x2,0xf, // mode 2: source AND (NOT destination)
            0x0,0xc,0x3,0xf, // mode 3: source
            0x0,0xb,0x4,0xf, // mode 4: (NOT source) AND destination
            0x0,0xa,0x5,0xf, // mode 5: destination
            0x0,0x9,0x6,0xf, // mode 6: source XOR destination
            0x0,0x8,0x7,0xf, // mode 7: source OR destination
            0x0,0x7,0x8,0xf, // mode 8: (NOT source) AND (NOT destination)
            0x0,0x6,0x9,0xf, // mode 9: (NOT source) XOR destination
            0x0,0x5,0xa,0xf, // mode A: NOT destination
            0x0,0x4,0xb,0xf, // mode B: source or (NOT destination)
            0x0,0x3,0xc,0xf, // mode C: NOT source
            0x0,0x2,0xd,0xf, // mode D: (NOT source) OR destination
            0x0,0x1,0xe,0xf, // mode E: (NOT source) OR (NOT destination)
            0x0,0x0,0xf,0xf, // mode F: all ones
        };
        WORD op = wrmappin[wrt_mod];
        WORD font_style = vars->font_style;
        if (font_style) {
            if (font_style & vdi_Text_blit_light)
                vars->lite_msk = lineaVars.font_liteMask;
            if (font_style & vdi_Text_blit_thicken)
                vars->thknover = 0;
            if (font_style & vdi_Text_blit_skew) {
                vars->skew_msk = lineaVars.font_skewMask;
                if (vars->blt_flag == vdi_Text_sgl_loop) {
                    vars->dest_wrd = 0;
                    vars->blt_flag = vdi_Text_dbl_loop;
                } else if (vars->blt_flag == vdi_Text_dbl_loop) {
                    if (vars->width > 0x10)
                        vars->blt_flag = vars->shif_cnt >= 0 ? vdi_Text_mlt_rite : vdi_Text_mlt_left;
                }
            }
        }
        
        WORD s_next = vars->s_next;
        WORD d_next = vars->d_next;
        WORD shif_cnt = vars->shif_cnt;
        WORD height = vars->height;
        switch (vars->blt_flag) {
        default: return;
        case vdi_Text_sgl_loop:
            {
                while (1) {
                    ULONG pixelSrc = *(ULONG*)src;
                    if (shif_cnt >= 0)
                        pixelSrc >>= shif_cnt;
                    else
                        pixelSrc <<= -shif_cnt;                                       
                    pixelSrc = vdi_Text_processEffects(vars, pixelSrc, vars->left_msk);
                    
                    *(UWORD*)dst = doPixelOpWithMask(op, pixelSrc >> 16, *(UWORD*)dst, vars->left_msk);
                    
                    if (--height <= 0)
                        break;
                    
                    src += s_next;
                    dst += d_next;
                }
            }
            break;
        case vdi_Text_dbl_loop:
            {
                while (1) {
                    ULONG pixelSrc = *(ULONG*)src;
                    if (shif_cnt >= 0)
                        pixelSrc >>= shif_cnt;
                    else
                        pixelSrc <<= -shif_cnt;
                    pixelSrc = vdi_Text_processEffects(vars, pixelSrc, vars->left_msk);
                    
                    UWORD *d0 = (UWORD*)dst;
                    *d0 = doPixelOpWithMask(op, pixelSrc >> 16, *d0, vars->left_msk);
                    
                    UWORD *d1 = (UWORD*)(dst + vars->nextwrd);
                    *d1 = doPixelOpWithMask(op, pixelSrc, *d1, vars->rite_msk);
                    
                    if (--height <= 0)
                        break;
                        
                    src += s_next;
                    dst += d_next;
                }
            }
            break;
        #if 0
        case vdi_Text_mlt_rite:
            {
                while (1) {
                    #if 0
                    if (vars->font_style & vdi_Text_blit_skew) {
                        blt_flag = vdi_Text_blitSkewW(vars, blt_flag, &dst);
                        if (blt_flag != vdi_Text_mlt_rite)
                            goto on_left;
                    }
                    #endif
                
                    UBYTE *s = src, *d = dst;
                    
                    ULONG pixelSrc = ((ULONG)*(UWORD*)s << 16) >> shif_cnt; s += 2;
                    UWORD pixelSrcHi = pixelSrc >> 16, pixelSrcLo = (UWORD)pixelSrc;
                    pixelSrcHi = vdi_Text_processEffectsWF(vars, pixelSrcHi, vars->left_msk);
                    pixelSrcHi = doPixelOpWithMask(op, pixelSrcHi, *(UWORD*)d, vars->left_msk);
                    *(UWORD*)d = pixelSrcHi;
                    d += vars->nextwrd;
                    
                    UWORD rota_msk = vars->rota_msk;
                    WORD wordNb = vars->dest_wrd;
                    while (1) {
                        UWORD pixelSrcLoSaved = pixelSrcLo & rota_msk;
                        pixelSrc = (((ULONG)*(UWORD*)s << 16) | pixelSrcLo) >> shif_cnt; s += 2;
                        pixelSrcHi = ((pixelSrc >> 16) & ~rota_msk) ^ pixelSrcLoSaved;
                        pixelSrcLo = (UWORD)pixelSrc;
                        if (--wordNb <= 0)
                            break;
                        pixelSrcHi = vdi_Text_processEffectsW(vars, pixelSrcHi);
                        pixelSrcHi = doPixelOp(op, pixelSrcHi, *(UWORD*)d);
                        *(UWORD*)d = pixelSrcHi;
                        d += vars->nextwrd;
                    }
                   
                    pixelSrcHi = vdi_Text_processEffectsWF(vars, pixelSrcHi, vars->rite_msk);
                    pixelSrcHi = doPixelOpWithMask(op, pixelSrcHi, *(UWORD*)d, vars->rite_msk);
                    *(UWORD*)d = pixelSrcHi;

                    if (--height <= 0)
                        break;
                        
                    vars->thknover = 0;
                    rolw1(vars->lite_msk);
                    src += s_next;
                    dst += d_next;
                }
            }
            break;
        case vdi_Text_mlt_left:
            {
                while (1) {
                    #if 0
                    if (vars->font_style & vdi_Text_blit_skew) {
                        blt_flag = vdi_Text_blitSkewW(vars, blt_flag, &dst);
                        if (blt_flag != vdi_Text_mlt_left)
                            goto on_right;
                    }
                    #endif
                
                    UBYTE *s = src, *d = dst;
                    
                    ULONG pixelSrc = *(ULONG*)s << shif_cnt; s += 4;
                    UWORD pixelSrcHi = pixelSrc >> 16, pixelSrcLo = (UWORD)pixelSrc;
                    pixelSrcHi = vdi_Text_processEffectsWF(vars, pixelSrcHi, vars->left_msk);
                    pixelSrcHi = doPixelOpWithMask(op, pixelSrcHi, *(UWORD*)d, vars->left_msk);
                    *(UWORD*)d = pixelSrcHi;
                    d += vars->nextwrd;
                    
                    UWORD rota_msk = vars->rota_msk;
                    WORD wordNb = vars->dest_wrd;
                    while (1) {
                        UWORD pixelSrcLoSaved = pixelSrcLo & rota_msk;
                        pixelSrc = (((ULONG)pixelSrcHi << 16) | *(UWORD*)s) << shif_cnt; s += 2;
                        pixelSrcHi = ((pixelSrc >> 16) & ~rota_msk) ^ pixelSrcLoSaved;
                        pixelSrcLo = (UWORD)pixelSrc;
                        if (--wordNb <= 0)
                            break;
                        pixelSrcHi = vdi_Text_processEffectsW(vars, pixelSrcHi);
                        pixelSrcHi = doPixelOp(op, pixelSrcHi, *(UWORD*)d);
                        *(UWORD*)d = pixelSrcHi;
                        d += vars->nextwrd;
                    }
                   
                    pixelSrcHi = vdi_Text_processEffectsWF(vars, pixelSrcHi, vars->rite_msk);
                    pixelSrcHi = doPixelOpWithMask(op, pixelSrcHi, *(UWORD*)d, vars->rite_msk);
                    *(UWORD*)d = pixelSrcHi;

                    if (--height <= 0)
                        break;
                        
                    vars->thknover = 0;
                    rolw1(vars->lite_msk);
                    src += s_next;
                    dst += d_next;
                }
            }
            break;
        #endif
        }

        if (--vars->nbrplane <= 0)
            break;
        src = vars->sform;
        dst = vars->dform + 2;
        vars->dform = dst;
//        if (vars->font_style & vdi_Text_blit_skew)
//            goto restart; // only skew screws up other planes, must reinit
    } 
    
}

#else

#define vdi_Text_blitOpti __attribute__ ((optimize("Os"), optimize("no-tree-scev-cprop")))

forceinline void vdi_Text_blitNormalPlane(WORD loopMode, UBYTE *src, UBYTE *dst, WORD srcStride, WORD dstStride, WORD wordStride, WORD wordNb, WORD height, WORD shift, UWORD maskLeft, UWORD maskRight, UWORD maskMiddle, WORD op) {
    switch (loopMode) {
    default:
    case vdi_Text_sgl_loop:
        {
            if (shift >= 0) {
                do {
                    ULONG pixelSrc = *(ULONG*)src >> shift;
                    *(UWORD*)dst = doPixelOpWithMask(op, pixelSrc >> 16, *(UWORD*)dst, maskLeft);
                    src += srcStride; dst += dstStride;
                } while (--height != -1);
            } else {
                shift = -shift;
                do {
                    ULONG pixelSrc = *(ULONG*)src << shift;
                    *(UWORD*)dst = doPixelOpWithMask(op, pixelSrc >> 16, *(UWORD*)dst, maskLeft);
                    src += srcStride; dst += dstStride;
                } while (--height != -1);
            }
        }
        break;
    case vdi_Text_dbl_loop:
        {
            if (shift >= 0) {
                do {
                    ULONG pixelSrc = *(ULONG*)src >> shift;
                    UWORD *d0 = (UWORD*)dst;
                    *d0 = doPixelOpWithMask(op, pixelSrc >> 16, *d0, maskLeft);
                    UWORD *d1 = (UWORD*)(dst + wordStride);
                    *d1 = doPixelOpWithMask(op, pixelSrc, *d1, maskRight);
                    src += srcStride; dst += dstStride;
                } while (--height != -1);
            } else {
                shift = -shift;
                do {
                    ULONG pixelSrc = *(ULONG*)src << shift;
                    UWORD *d0 = (UWORD*)dst;
                    *d0 = doPixelOpWithMask(op, pixelSrc >> 16, *d0, maskLeft);
                    UWORD *d1 = (UWORD*)(dst + wordStride);
                    *d1 = doPixelOpWithMask(op, pixelSrc, *d1, maskRight);
                    src += srcStride; dst += dstStride;
                } while (--height != -1);
            }
        }
        break;
    case vdi_Text_mlt_rite:
        {
            do {
                UBYTE *s = src, *d = dst;
                
                ULONG pixelSrc = ((ULONG)*(UWORD*)s << 16) >> shift; s += 2;
                UWORD pixelSrcHi = pixelSrc >> 16, pixelSrcLo = (UWORD)pixelSrc;
                pixelSrcHi = doPixelOpWithMask(op, pixelSrcHi, *(UWORD*)d, maskLeft);
                *(UWORD*)d = pixelSrcHi;
                d += wordStride;
                
                WORD wordIndex = wordNb;
                while (1) {
                    UWORD pixelSrcLoSaved = pixelSrcLo & maskMiddle;
                    pixelSrc = (((ULONG)*(UWORD*)s << 16) | pixelSrcLo) >> shift; s += 2;
                    pixelSrcHi = ((pixelSrc >> 16) & ~maskMiddle) ^ pixelSrcLoSaved;
                    pixelSrcLo = (UWORD)pixelSrc;
                    if (--wordIndex <= 0)
                        break;
                    pixelSrcHi = doPixelOp(op, pixelSrcHi, *(UWORD*)d);
                    *(UWORD*)d = pixelSrcHi;
                    d += wordStride;
                }
               
                pixelSrcHi = doPixelOpWithMask(op, pixelSrcHi, *(UWORD*)d, maskRight);
                *(UWORD*)d = pixelSrcHi;

                src += srcStride; dst += dstStride;
            } while (--height != -1);
        }
        break;
    case vdi_Text_mlt_left:
        {
            do {
                UBYTE *s = src, *d = dst;
                
                ULONG pixelSrc = *(ULONG*)s << shift; s += 4;
                UWORD pixelSrcHi = pixelSrc >> 16, pixelSrcLo = (UWORD)pixelSrc;
                pixelSrcHi = doPixelOpWithMask(op, pixelSrcHi, *(UWORD*)d, maskLeft);
                *(UWORD*)d = pixelSrcHi;
                d += wordStride;
                
                WORD wordIndex = wordNb;
                while (1) {
                    UWORD pixelSrcLoSaved = pixelSrcLo & maskMiddle;
                    pixelSrc = (((ULONG)pixelSrcHi << 16) | *(UWORD*)s) << shift; s += 2;
                    pixelSrcHi = ((pixelSrc >> 16) & ~maskMiddle) ^ pixelSrcLoSaved;
                    pixelSrcLo = (UWORD)pixelSrc;
                    if (--wordIndex <= 0)
                        break;
                    pixelSrcHi = doPixelOp(op, pixelSrcHi, *(UWORD*)d);
                    *(UWORD*)d = pixelSrcHi;
                    d += wordStride;
                }
               
                pixelSrcHi = doPixelOpWithMask(op, pixelSrcHi, *(UWORD*)d, maskRight);
                *(UWORD*)d = pixelSrcHi;

                src += srcStride; dst += dstStride;
            } while (--height != -1);
        }
        break;
    }
}

vdi_Text_blitOpti
void vdi_Text_blitNormal(vdi_Text_Context *vars, UBYTE *src, UBYTE *dst) {
    if (vars->height <= 0) return;
    WORD shift = vars->tddad - vars->tsdad;
    UWORD maskMiddle = ~vdi_fringeTable[shift < 0 ? 16 + shift : shift];
    
    // Set up fringe masks.
    WORD dstWordPixelLeft = vars->tddad, dstWordPixelRight = dstWordPixelLeft + vars->width;
    UWORD maskLeft = vdi_fringeTable[dstWordPixelLeft], maskRight;
    WORD loopMode;
    WORD wordNb;
    if (dstWordPixelRight <= 16) {
        // Fits in one word.
        maskLeft &= ~vdi_fringeTable[dstWordPixelRight]; // merge both masks.
        maskRight = 0;
        loopMode = vdi_Text_sgl_loop;
        wordNb = 0;
    } else {
        // Two fringe masks needed.
        wordNb = (dstWordPixelRight >> 4) - 1; // -1 to remove the left fringe. The right fringe is not counted if not full, and removed if full.
        if (wordNb == 0 && (vars->tsdad + vars->width) <= 32)
            loopMode = vdi_Text_dbl_loop;
        else if (shift >= 0)
            loopMode = vdi_Text_mlt_rite;
        else
            loopMode = vdi_Text_mlt_left;
        dstWordPixelRight &= 0xf;
        if (dstWordPixelRight == 0) {
            // Last word is full so its a fringe, remove it.
            wordNb--;
            dstWordPixelRight = 16;
        }
        maskRight = ~vdi_fringeTable[dstWordPixelRight];
    }

    // Plane loop.
    WORD writingMode = vars->writingMode;
    WORD srcStride = vars->s_next, dstStride = vars->d_next, wordStride = vars->nextwrd;
    WORD height = vars->height - 1;
    WORD planeIndex = vars->nbrplane - 1;
    UWORD colorFg = vars->forecol, colorBg = vars->ambient;
    do {
        WORD op = translateWritingModeToOp(writingMode, colorFg & 1, colorBg & 1);
        colorFg >>= 1; colorBg >>= 1;
        #if 0
        vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, op);
        #else
        switch (op) {
        default:
        case BM_ALL_WHITE:  vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_ALL_WHITE); break;
        case BM_S_AND_D:    vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_S_AND_D); break;
        case BM_S_AND_NOTD: vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_S_AND_NOTD); break;
        case BM_S_ONLY:     vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_S_ONLY); break;
        case BM_NOTS_AND_D: vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_NOTS_AND_D); break;
        case BM_D_ONLY:     vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_D_ONLY); break;
        case BM_S_XOR_D:    vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_S_XOR_D); break;
        case BM_S_OR_D:     vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_S_OR_D); break;
        case BM_NOT_SORD:   vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_NOT_SORD); break;
        case BM_NOT_SXORD:  vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_NOT_SXORD); break;
        case BM_NOT_D:      vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_NOT_D); break;
        case BM_S_OR_NOTD:  vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_S_OR_NOTD); break;
        case BM_NOT_S:      vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_NOT_S); break;
        case BM_NOTS_OR_D:  vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_NOTS_OR_D); break;
        case BM_NOT_SANDD:  vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_NOT_SANDD); break;
        case BM_ALL_BLACK:  vdi_Text_blitNormalPlane(loopMode, src, dst, srcStride, dstStride, wordStride, wordNb, height, shift, maskLeft, maskRight, maskMiddle, BM_ALL_BLACK); break;
        }
        #endif
        dst += 2;
    } while (--planeIndex != -1);
}

#endif

/*
 * check for clipping
 *
 * returns  1 clipping required
 *          0 no clipping
 *         -1 entirely clipped, nothing to output
 */
static WORD vdi_Text_checkClip(vdi_Text_Context *vars, WORD delx, WORD dely) {
    if (!lineaVars.clipping_enabled)
        return 0;
    WORD rc = 0;

    /*
     * check x coordinate
     */
    WORD x = vars->destinationX, xMin = lineaVars.clipping_rect.xMin;
    if (x < xMin) { /* (partially) left of clip window */
        if (x + delx <= xMin) /* wholly left of clip window */
            return -1;
        rc = 1;
    }
    WORD xMax = lineaVars.clipping_rect.xMax;
    if (x > xMax) /* wholly right of clip window */
        return -1;
    if (x + delx > xMax) /* partially right of clip window */
        rc = 1;

    /*
     * check y coordinate
     */
    WORD y = vars->destinationY, yMin = lineaVars.clipping_rect.yMin;
    if (y < yMin) { /* (partially) below clip window */
        if (y + dely <= yMin)     /* wholly below clip window */
            return -1;
        rc = 1;
    }
    WORD yMax = lineaVars.clipping_rect.yMax;
    if (y > yMax) /* wholly above clip window */
        return -1;
    if (y + dely > yMax) /* partially above clip window */
        rc = 1;

    return rc;
}

/*
 * do the actual clipping
 * returns  0 OK
 *         -1 entirely clipped, nothing to output.  I believe this can
 *            only happen if rotation or scaling has been done
 */
static WORD vdi_Text_clip(vdi_Text_Context *vars) {
    /*
     * if clipping not requested, exit
     */
    if (!lineaVars.clipping_enabled)
        return 0;

    // clip x minimum if necessary
    WORD destinationX = vars->destinationX, characterSizeX = vars->characterSizeX;
    WORD xMin = lineaVars.clipping_rect.xMin;
    if (destinationX < xMin) {
        WORD n = destinationX + characterSizeX - xMin;
        if (n <= 0)
            return -1;
        lineaVars.text_sourceX += characterSizeX - n;
        characterSizeX = n;
        destinationX = xMin;
        vars->destinationX = destinationX;
    }
    // clip x maximum if necessary
    {
        WORD xMax = lineaVars.clipping_rect.xMax;
        if (destinationX > xMax)
            return -1;
        WORD n = destinationX + characterSizeX - xMax - 1;
        if (n > 0)
            characterSizeX -= n;
    }
    vars->characterSizeX = characterSizeX;
    
    // clip y minimum if necessary
    WORD destinationY = vars->destinationY, characterSizeY = vars->characterSizeY;
    WORD yMin = lineaVars.clipping_rect.yMin;
    if (destinationY < yMin) {
        WORD n = destinationY + characterSizeY - yMin;
        if (n <= 0)
            return -1;
        lineaVars.text_sourceY += characterSizeY - n;
        characterSizeY = n;
        destinationY = yMin;
        vars->destinationY = destinationY;
    }
    // clip y maximum if necessary
    {
        WORD yMax = lineaVars.clipping_rect.yMax;
        if (destinationY > yMax)
            return -1;
        WORD n = destinationY + characterSizeY - yMax - 1;
        if (n > 0)
            characterSizeY -= n;
    }
    vars->characterSizeY = characterSizeY;
    
    return 0;
}

/*
 * return a ULONG equal to the 3 high-order bytes pointed to by
 * the input pointer, concatenated with the low-order byte of
 * the input UWORD
 */
static ULONG vdi_Text_mergeByte(UWORD *p, UWORD n) {
    union {
        ULONG a;
        UBYTE b[4];
    } work;
    work.a = *(ULONG *)p;
    work.b[3] = n;
    return work.a;
}

/*
 * Perform text outlining
 * in the following code, the top 18 bits of unsigned longs are used
 * to manage 18-bit values, consisting of the last bit of the previous
 * screen word, the 16 bits of the current screen word, and the first
 * bit of the next screen word.
 * neighbours are numbered as follows:
 * 1 2 3
 * 7 X 8
 * 4 5 6
 */
static void vdi_Text_outline(vdi_Text_Context *vars) {
    WORD form_width = vars->s_next / sizeof(WORD);
    UWORD *currline = (UWORD *)vars->sform + form_width;
    UWORD *nextline = currline + form_width;

    /* process lines sequentially */
    for (WORD i = vars->characterSizeY; i > 0; i--) {
        UWORD *save_next = nextline;
        UWORD prev = 0;
        UWORD curr = 0;
        ULONG bottom_left = (*(ULONG *)nextline) >> 1;

        /* process one word at a time */
        UWORD *scratch = (UWORD *)vars->sform;
        for (WORD j = form_width; j > 0; j--) {
            // Get the data from the current line.
            ULONG current_left = vdi_Text_mergeByte(currline, curr), current_noshift = current_left;
            rorli(current_noshift, 1);
            ULONG current_right = current_noshift;
            rorli(current_right, 1);

            // Get the data from the scratch buffer (the previous line) and merge into result
            ULONG top_right = vdi_Text_mergeByte(scratch, prev);
            rorli(top_right, 1);
            ULONG top_left = top_right, top_noshift = top_left;
            top_left ^= current_left; /* neighbours 1,2,3 */
            top_noshift ^= current_noshift;
            top_right ^= current_right;
            rolli(top_noshift, 1);
            rolli(top_right, 2);
            ULONG result = (top_left | top_noshift | top_right);

            // Get the data from the next line & merge into result.
            ULONG bottom_right = bottom_left, bottom_noshift = bottom_right;
            bottom_left ^= current_left; /* neighbours 4,5,6 */
            bottom_noshift ^= current_noshift;
            bottom_right ^= current_right;
            rolli(bottom_noshift, 1);
            rolli(bottom_right, 2);
            result |= (bottom_left | bottom_noshift | bottom_right);

            /* finally, merge current line neighbours */
            current_left ^= current_noshift; /* neighbours 7,8 */
            current_right ^= current_noshift;
            rolli(current_right, 2);
            result |= (current_left | current_right);
            result >>= 16; /* move to lower 16 bits */

            prev = curr = *currline;
            prev = (prev ^ result) & result;
            *currline++ = prev;
            prev = *scratch;
            *scratch++ = curr;

            UWORD tmp = *nextline++;
            bottom_left = vdi_Text_mergeByte(nextline, tmp);
            rorli(bottom_left, 1);
        }

        nextline = save_next;
        currline = nextline;

        if (i > 2) /* mustn't go past end */
            nextline += form_width;
    }
}

/*
 * Copy the character into a buffer so that we can outline/rotate/scale it.
 */
static void vdi_Text_preBlit(vdi_Text_Context *vars) {
    vars->height = vars->characterSizeY;
    vars->tsdad = lineaVars.text_sourceX & 0x000f; /* source dot address */
    UBYTE *src = vars->sform + (lineaVars.text_sourceY + vars->characterSizeY-1) * (LONG)vars->s_next + ((lineaVars.text_sourceX >> 3) & ~1); /* bottom of font char source */
    vars->s_next = -vars->s_next; /* we draw from the bottom up */
    WORD dest_width = vars->characterSizeX;
    if (vars->font_style & vdi_TextStyle_thickened) {
        WORD weight = lineaVars.font_weight;
        dest_width += weight;
        vars->smear = weight;
    }

    /*
     * handle outlining
     */
    vars->tddad = 0;
    WORD dest_height = vars->characterSizeY;
    if (vars->font_style & vdi_TextStyle_outlined) {
        dest_width += 3; /* add 1 left & 2 right pixels */
        vars->tddad += 1; /* and make leftmost column blank */
        vars->characterSizeY += 2; /* add 2 rows */
        dest_height += 3; /* add 3 rows for buffer clear */
    }
    vars->width = dest_width;
    dest_width += lineaVars.font_skewLoff + lineaVars.font_skewRoff;
    vars->characterSizeX = dest_width;
    dest_width = ((dest_width >> 4) << 1) + 2; /* in bytes */
    vars->d_next = -dest_width;
    WORD size = dest_width * (dest_height - 1);
    vars->buffa = lineaVars.text_largeBuffer - vars->buffa; /* switch buffers */
    UBYTE *dst = (UBYTE *)lineaVars.text_scratchBuffer + vars->buffa;
    vars->sform = dst;
    if (vars->font_style & (vdi_TextStyle_outlined | vdi_TextStyle_skewed)) {
        WORD *p = (WORD *)vars->sform;
        WORD n = (size - vars->d_next) / 2; /* add bottom line */
        while (n--)
            *p++ = 0; /* clear buffer */
        if (vars->font_style & vdi_TextStyle_outlined) {
            vars->width -= 3;
            vars->characterSizeX -= 1;
            size += vars->d_next;
        }
    }

    dst += size; /* start at the bottom */
    vars->writingMode = 0;
    vars->forecol = 1;
    vars->ambient = 0;
    vars->nbrplane = 1; /* 1 plane for this blit */
    vars->nextwrd = 2;
    WORD tmp_style = vars->font_style; /* save temporarily */
    vars->font_style &= (vdi_TextStyle_skewed|vdi_TextStyle_thickened); /* only thicken, skew */

    vdi_Text_blitNormal(vars, src, dst);

    vars->font_style = tmp_style; /* restore */
    vars->writingMode = lineaVars.writingMode;
    vars->s_next = -vars->d_next; /* reset the source to the buffer */

    if (vars->font_style & vdi_TextStyle_outlined) {
        vdi_Text_outline(vars);
        vars->sform += vars->s_next;
    }

    lineaVars.text_sourceX = 0;
    lineaVars.text_sourceY = 0;
    vars->font_style &= ~(vdi_TextStyle_skewed | vdi_TextStyle_thickened); /* cancel effects */
}

static void vdi_Text_rotate(vdi_Text_Context *vars) {
    vars->tsdad = lineaVars.text_sourceX & 0x000f;
    UBYTE *src = vars->sform + ((lineaVars.text_sourceX >> 4) << 1);
    vars->buffa = lineaVars.text_largeBuffer - vars->buffa;     /* switch buffers */
    UBYTE *dst = (UBYTE *)lineaVars.text_scratchBuffer + vars->buffa;

    vars->width = vars->characterSizeX;
    vars->height = vars->characterSizeY;

    /*
     * first, handle the simplest case: inverted text (180 rotation)
     */
    if (lineaVars.text_upVector == 1800) {
        WORD form_width = ((vars->characterSizeX + vars->tsdad-1) >> 4) + 1; /* in words */
        vars->d_next = form_width * sizeof(WORD);
        UWORD *d = (UWORD *)(dst + vars->d_next * vars->height);
        for (WORD i = vars->height; i > 0; i--) {
            UWORD *s = (UWORD *)src;
            for (WORD j = form_width; j > 0; j--) {
                UWORD in = *s++;
                UWORD out = vdi_Text_reverseNybbleTable[in&0x000f]; /* reverse 4 bits at a time */
                for (WORD k = 3; k > 0; k--) {
                    out <<= 4;
                    in >>= 4;
                    out |= vdi_Text_reverseNybbleTable[in&0x000f];
                }
                *--d = out;
            }
            src += vars->s_next;
        }
        vars->s_next = vars->d_next;
        vars->sform = (UBYTE *)lineaVars.text_scratchBuffer + vars->buffa;
        lineaVars.text_sourceX = -(lineaVars.text_sourceX + vars->characterSizeX) & 0x000f;
        lineaVars.text_sourceY = 0;
        return;
    }

    /*
     * handle remaining cases (90 and 270 rotation)
     */
    vars->d_next = ((vars->characterSizeY >> 4) << 1) + 2;

    if (lineaVars.text_upVector == 900) {
        dst += (vars->characterSizeX - 1) * vars->d_next;
        vars->d_next = -vars->d_next;
    } else {       /* 2700 */
        src += (lineaVars.text_sourceY + vars->height - 1) * vars->s_next;
        vars->s_next = -vars->s_next;
    }

    {
        UWORD srcBit = 0x8000 >> vars->tsdad, dstBit = 0x8000, dstWord = 0;
        UWORD *s = (UWORD *)src, *d = (UWORD *)dst;
        for (WORD i = vars->width; i > 0; i--) {
            for (WORD j = vars->height; j > 0; j--) {
                if (*s & srcBit)
                    dstWord |= dstBit;
                dstBit >>= 1;
                if (!dstBit) {
                    dstBit = 0x8000;
                    *d++ = dstWord;
                    dstWord = 0;
                }
                s = (UWORD *)((UBYTE *)s + vars->s_next);
            }

            dstBit = 0x8000;
            *d = dstWord;
            dstWord = 0;
            dst += vars->d_next;
            d = (UWORD *)dst;

            srcBit >>= 1;
            if (!srcBit) {
                srcBit = 0x8000;
                src += sizeof(WORD);
            }

            s = (UWORD *)src;
        }
    }
    
    vars->height = vars->characterSizeX;  /* swap width & height */
    vars->width = vars->characterSizeY;
    vars->characterSizeX = vars->width;
    vars->characterSizeY = vars->height;
    WORD tmp = vars->tmp_delx;
    vars->tmp_delx = vars->tmp_dely;
    vars->tmp_dely = tmp;
    vars->swap_tmps = 1;

    vars->s_next = (lineaVars.text_upVector == 900) ? -vars->d_next : vars->d_next;
    vars->sform = (UBYTE*)lineaVars.text_scratchBuffer + vars->buffa;
    lineaVars.text_sourceX = 0;
    lineaVars.text_sourceY = 0;
}

/*
 * inline function to clarify horizontal scaling code
 */
forceinline UWORD* vdi_Text_shiftAndUpdate(UWORD *dst, UWORD *dstbit, UWORD *out) {
    UWORD dstBit_ = *dstbit;
    dstBit_ >>= 1;
    if (!dstBit_) { /* end of word ? */
        dstBit_ = 0x8000; /* reset test bit */
        *dst++ = *out; /* output accumulated word */
        *out = 0; /* & reset it */
    }
    *dstbit = dstBit_;
    return dst;
}

/*
 * Increase width of character (lineaVars.text_scaleDirection is 1).
 */
static void vdi_Text_scaleUp(vdi_Text_Context *vars, UWORD *src, UWORD *dst) {
    UWORD srcbit = 0x8000 >> vars->tsdad, srcWord = *src++, dstbit = 0x8000, dstWord = 0;
    UWORD accum = lineaVars.ddaX, ddaIncrement = lineaVars.ddaIncrement;
    for (WORD i = vars->width; i > 0; i--) {
        accum += ddaIncrement;
        if (srcWord & srcbit) { /* handle bit set in source */
            if (accum < ddaIncrement) {
                dstWord |= dstbit;
                dst = vdi_Text_shiftAndUpdate(dst, &dstbit, &dstWord);
            }
            dstWord |= dstbit;
        } else { /* handle bit clear in source */
            if (accum < ddaIncrement)
                dst = vdi_Text_shiftAndUpdate(dst, &dstbit, &dstWord);
        }
        dst = vdi_Text_shiftAndUpdate(dst, &dstbit, &dstWord);
        srcbit >>= 1;
        if (!srcbit) {
            srcbit = 0x8000;
            srcWord = *src++;
        }
    }
    *dst = dstWord;
}

/*
 * Decrease width of character (lineaVars.text_scaleDirection is 0).
 */
static void vdi_Text_scaleDown(vdi_Text_Context *vars, UWORD *src, UWORD *dst) {
    UWORD srcbit = 0x8000 >> vars->tsdad, srcWord = *src++, dstbit = 0x8000, dstWord = 0;
    UWORD accum = lineaVars.ddaX, ddaIncrement = lineaVars.ddaIncrement;
    for (WORD i = vars->width; i > 0; i--) {
        if (srcWord & srcbit) { /* handle bit set in source */
            accum += ddaIncrement;
            if (accum < ddaIncrement) {
                dstWord |= dstbit;
                dst = vdi_Text_shiftAndUpdate(dst, &dstbit, &dstWord);
            }
        } else /* handle bit clear in source */
            dst = vdi_Text_shiftAndUpdate(dst, &dstbit, &dstWord);
        srcbit >>= 1;
        if (!srcbit) {
            srcbit = 0x8000;
            srcWord = *src++;
        }
    }
    *dst = dstWord;
}

/*
 * Perform text scaling.
 */
static void vdi_Text_scale(vdi_Text_Context *vars) {
    vars->tsdad = lineaVars.text_sourceX & 0x000f;
    UBYTE *src = vars->sform + ((lineaVars.text_sourceX >> 4) << 1) + (lineaVars.text_sourceY * vars->s_next);
    vars->buffa = lineaVars.text_largeBuffer - vars->buffa;     /* switch buffers */
    UBYTE *dst = (UBYTE *)lineaVars.text_scratchBuffer + vars->buffa;
    vars->width = vars->characterSizeX;
    vars->height = vars->characterSizeY;
    vars->d_next = ((vars->width >> 3) << 1) + 2;
    UWORD ddaIncrement = lineaVars.ddaIncrement;

    /*
     * first, scale the character
     */
    UWORD accum = 0x7fff;
    if (lineaVars.text_scaleDirection) { /* scale up */
        for (WORD i = vars->height; i > 0; i--) {
            accum += ddaIncrement;
            if (accum < ddaIncrement) {
                vdi_Text_scaleUp(vars, (UWORD *)src, (UWORD *)dst);
                dst += vars->d_next;
            }
            vdi_Text_scaleUp(vars, (UWORD *)src, (UWORD *)dst);
            dst += vars->d_next;
            src += vars->s_next;
        }
    } else { /* scale down */
        for (WORD i = vars->height; i > 0; i--) {
            accum += ddaIncrement;
            if (accum < ddaIncrement) {
                vdi_Text_scaleDown(vars, (UWORD *)src, (UWORD *)dst);
                dst += vars->d_next;
            }
            src += vars->s_next;
        }
    }

    /*
     * then, adjust the character spacing
     */
    accum = lineaVars.ddaX;
    WORD delx = lineaVars.text_scaleDirection ? vars->characterSizeX : 0;
    for (WORD i = vars->characterSizeX; i > 0; i--) {
        accum += lineaVars.ddaIncrement;
        if (accum < lineaVars.ddaIncrement)
            delx++;
    }
    lineaVars.ddaX = accum;

    vars->characterSizeX = delx;
    vars->characterSizeY = vars->tmp_dely;
    vars->s_next = vars->d_next;
    vars->sform = (UBYTE *)lineaVars.text_scratchBuffer + vars->buffa;
    lineaVars.text_sourceX = 0;
    lineaVars.text_sourceY = 0;
}

// Output a block to the screen.
static void vdi_Text_blitToScreen(vdi_Text_Context *vars) {
    vars->forecol = lineaVars.text_colorForeground;
    vars->ambient = 0; /* logically lineaVars.text_background, but that isn't set up by the VDI */
    vars->nbrplane = lineaVars.screen_planeNb;
    vars->nextwrd = vars->nbrplane * sizeof(WORD);
    vars->height = vars->characterSizeY;
    vars->width = vars->characterSizeX;

    /*
     * calculate the starting address for the character to be copied
     */
    vars->tsdad = lineaVars.text_sourceX & 0x000f; /* source dot address */
    LONG offset = (lineaVars.text_sourceY+vars->characterSizeY-1) * (LONG)vars->s_next + ((lineaVars.text_sourceX >> 3) & ~1);
    vars->sform += offset;
    vars->s_next = -vars->s_next; /* we draw from the bottom up */

    /*
     * calculate the screen address
     *
     * note that the casts below allow the compiler to generate a mulu
     * instruction rather than calling _mulsi3(): this by itself speeds
     * up plain text output by about 3% ...
     */
    vars->tddad = vars->destinationX & 0x000f;
    vars->dform = v_bas_ad;
    vars->dform += (vars->destinationX & 0xfff0) >> vdi_context.planeNbShift; /* add x coordinate part of addr */
    vars->dform += (UWORD)(vars->destinationY + vars->characterSizeY-1) * (ULONG)lineaVars.screen_lineSize2; /* add y coordinate part of addr */
    vars->d_next = -lineaVars.screen_lineSize2;

//    vdi_Text_blitNormal(vars+1, vars->sform, vars->dform);  /* call assembler helper function */
    vdi_Text_blitNormal(vars, vars->sform, vars->dform);
}

/*
 * resize characters for line-A
 *
 * this is similar to act_siz(), but note that act_siz() always starts
 * with the same value in the accumulator, while here the initial value
 * is passed as an argument.
 *
 * this allows us to use the same routine for scaling height (which is
 * constant for a given string) and width (which can vary since we do a
 * kind of nano-justification).
 *
 * entry:
 *  init        initial value of accumulator
 *  size        value to scale
 *
 * used variables:
 *  lineaVars.ddaIncrement      DDA increment passed externally
 *  lineaVars.text_scaleDirection     0 if scale down, 1 if enlarge
 *
 * exit:
 *  new size
 */
static UWORD vdi_Text_resizeChar(UWORD init, UWORD size) {
    UWORD ddaIncrement = lineaVars.ddaIncrement;
    if (ddaIncrement == 0xffff) /* double size */
        return (size<<1);
    UWORD accu = init;
    UWORD retval = lineaVars.text_scaleDirection ? size : 0;
    for (UWORD i = 0; i < size; i++) {
        accu += ddaIncrement;
        if (accu < ddaIncrement)
            retval++;
    }
    /* if input is non-zero, make return value at least 1 */
    if (size && !retval)
        retval = 1;
    return retval;
}

// Can also be called by vdi_asm.S.
void vdi_Text_blit(void);
void vdi_Text_blit(void) {
    vdi_Text_Context vars;

    vars.swap_tmps = 0;

    // Make local copies
    vars.font_style = lineaVars.font_style;
    vars.writingMode = lineaVars.writingMode;
    vars.characterSizeX = lineaVars.text_characterSizeX;
    vars.destinationX = lineaVars.text_destinationX;
    vars.characterSizeY = lineaVars.text_characterSizeY;
    vars.destinationY = lineaVars.text_destinationY;

    vars.buffa = 0;
    WORD dely = vars.characterSizeY, delx = vars.characterSizeX;

    if (lineaVars.text_scale) {
        vars.tmp_dely = dely = vdi_Text_resizeChar(0x7fff, vars.characterSizeY);
        vars.tmp_delx = delx = vdi_Text_resizeChar(lineaVars.ddaX, vars.characterSizeX);
    }

    vars.smear = 0;
    if (vars.font_style & vdi_TextStyle_thickened) {
        WORD weight = lineaVars.font_weight;
        if (weight == 0) // Cancel thicken if no weight.
            vars.font_style &= ~vdi_TextStyle_thickened;
        if (!lineaVars.font_monospaced)
            delx += weight;
    }

    if (vars.font_style & vdi_TextStyle_skewed)
        delx += lineaVars.font_skewLoff + lineaVars.font_skewRoff;
 
    if (vars.font_style & vdi_TextStyle_outlined) {
        delx += vdi_outlineThickness * 2;
        dely += vdi_outlineThickness * 2;
    }

    switch(lineaVars.text_upVector) {
    case 900:
        vars.destinationY -= delx;
        FALLTHROUGH;
    case 2700:
        {
            WORD temp = delx; /* swap delx/dely for 90 or 270 */
            delx = dely;
            dely = temp;
        }
        break;
    case 1800:
        vars.destinationX -= delx;
    }

    WORD clipped = vdi_Text_checkClip(&vars, delx, dely);
    if (clipped >= 0) {
        vars.dest_wrd = 0;
        vars.s_next = lineaVars.font_widthInBytes;
        vars.sform = (UBYTE *)lineaVars.font_data;

        if (lineaVars.text_scale)
            vdi_Text_scale(&vars);

        /*
         * the following is equivalent to:
         *  if outlining, OR rotating AND (skewing OR thickening), OR skewing AND clipping-is-required,
         *      call vdi_Text_preBlit()
         */
        if (vars.font_style & (vdi_TextStyle_skewed|vdi_TextStyle_thickened|vdi_TextStyle_outlined)) {
            if (lineaVars.text_upVector || ((vars.font_style & vdi_TextStyle_skewed) && clipped) || (vars.font_style & vdi_TextStyle_outlined))
                vdi_Text_preBlit(&vars);
        }

        if (lineaVars.text_upVector)
            vdi_Text_rotate(&vars);

        if (vars.font_style & vdi_TextStyle_thickened) {
            vars.smear = lineaVars.font_weight;
            if (!lineaVars.font_monospaced)
                vars.characterSizeX += vars.smear;
        }

        if (vdi_Text_clip(&vars) == 0)
            vdi_Text_blitToScreen(&vars);
    }
    
    delx = lineaVars.text_characterSizeX;
    if (lineaVars.text_scale)
        delx = vars.swap_tmps ? vars.tmp_dely : vars.tmp_delx;

    if (lineaVars.font_style & vdi_TextStyle_outlined)
        delx += vdi_outlineThickness * 2;

    if ((lineaVars.font_style & vdi_TextStyle_thickened) && !lineaVars.font_monospaced)
        delx += lineaVars.font_weight;

    switch(lineaVars.text_upVector) {
    default: /* normally 0, the default */
        lineaVars.text_destinationX += delx; /* move right by lineaVars.text_characterSizeX */
        break;
    case 900:
        lineaVars.text_destinationY -= delx; /* move up by lineaVars.text_characterSizeX */
        break;
    case 1800:
        lineaVars.text_destinationX -= delx; /* move left by lineaVars.text_characterSizeX */
        break;
    case 2700:
        lineaVars.text_destinationY += delx; /* move down by lineaVars.text_characterSizeX */
        break;
    }
}

/*
 * start of calculations extracted from vdi_tblit.S
 *
 * the calculations (as revised by the addition of parentheses) for the
 * 8x16 font have been verified with a test program.  the maximum usage
 * is observed with text that has been rotated, skewed and outlined:
 * 72 bytes for the small buffer and 212 bytes for the large buffer.
 */

/*
 *  NOTE: The calculations below should serve as an example for
 *  determining the cell size and buffer size required for creating
 *  a scratch character buffer for various sized fonts.
 *
 *  A larger scratch buffer must be used for character rotation/replication.
 *  Size requirement calculations for this buffer are outlined below.
 *  NOTE: font dependent equates would normally be found in the font header.
 */

/*
 * 8x16 font data
 */
#define vdi_Text_leftOffsetFromSkew 1
#define vdi_Text_rightOffsetFromSkew 7
#define vdi_Text_formHeight 16
#define vdi_Text_maximumCellWidth 8

#define vdi_Text_totalSkewOffset   (vdi_Text_leftOffsetFromSkew+vdi_Text_rightOffsetFromSkew) /* total skew offset */

#if 0

/*
 *  Since a character cell may be rotated 90 or 270 degrees, the cell
 *  height and width may be interchanged.  The width must be a multiple
 *  of a word (e.g. a 3-pixel width requires a minimum of 16 bits), but
 *  the height needn't be rounded up in a similar fashion, since it
 *  represents the number of rows).  Cell width and cell height must be
 *  calculated two different ways in order to accommodate rotation.
 */
#define cel_ww  (((vdi_Text_totalSkewOffset+vdi_Text_maximumCellWidth+15)/16)*2) /* worst case # bytes/row if width */
#define cel_wh  (vdi_Text_totalSkewOffset+vdi_Text_maximumCellWidth)     /* cell "width" if used as height (90 rotation) */
#define cel_hh  (vdi_Text_formHeight)               /* cell height if used as height */
#define cel_hw  (((vdi_Text_formHeight+15)/16)*2)   /* cell "height" if used as width (90 rotation) */

/*
 *  The maximum of:
 *      cell width (as width) * cell height (as height)
 *      cell width (as height) * cell height (as width)
 *  will be used for the basic buffer size.
 */
#define vdi_Text_cellSize0Rotation (cel_ww*cel_hh) /* cell size if no rotation */
#define vdi_Text_cellSize90Rotation (cel_wh*cel_hw) /* cell size if 90 deg rotation */

#if vdi_Text_cellSize0Rotation >= vdi_Text_cellSize90Rotation
# define vdi_Text_cellSize    (vdi_Text_cellSize0Rotation*2)
#else
# define vdi_Text_cellSize    (vdi_Text_cellSize90Rotation*2)
#endif

#endif

/*
 *  Now we repeat the whole thing for doubled cell dimensions
 */
#define vdi_Text_cell2LineSize ((((2*(vdi_Text_totalSkewOffset+vdi_Text_maximumCellWidth))+3+15)/16)*2)
#define vdi_Text_cell2LineSize90Rotation ((2*(vdi_Text_totalSkewOffset+vdi_Text_maximumCellWidth))+2)
#define vdi_Text_cell2Height ((2*vdi_Text_formHeight)+2)
#define vdi_Text_cell2Height90Rotation ((((2*vdi_Text_formHeight)+3+15)/16)*2)

#define vdi_Text_cell2Size0Rotation (vdi_Text_cell2LineSize*vdi_Text_cell2Height)   /* doubled cell size, no rotation */
#define vdi_Text_cell2Size90Rotation (vdi_Text_cell2LineSize90Rotation*vdi_Text_cell2Height90Rotation)   /* doubled cell size, 90 deg rotation */

#if vdi_Text_cell2Size0Rotation >= vdi_Text_cell2Size90Rotation
# define vdi_Text_cell2Size (vdi_Text_cell2Size0Rotation)
#else
# define vdi_Text_cell2Size (vdi_Text_cell2Size90Rotation)
#endif

/*
 *  [The following is unclear to me - RFB]
 *  Determine the maximum horizontal line (from width or height)
 *  which is required for outlining the character buffer.
 *  For worst case add two bytes.
 */
#if vdi_Text_cell2LineSize >= vdi_Text_cell2Height90Rotation
# define vdi_Text_cell2OutsidePad    (vdi_Text_cell2LineSize+2)
#else
# define vdi_Text_cell2OutsidePad    (vdi_Text_cell2Height90Rotation+2)
#endif

/*
 *  Since outlining can happen in either the small or large buffer, the
 *  small buffer requires at least (vdi_Text_cellSize+vdi_Text_cell2OutsidePad) bytes, and the large
 *  buffer requires (vdi_Text_cell2Size+vdi_Text_cell2OutsidePad) bytes.
 *
 *  IMPORTANT: in order to be able to rearrange & simplify the text
 *  blitting code, it is desirable to be able to rotate and/or scale
 *  the text in either buffer.  Therefore both buffers should be 'large'
 *  buffers.  The following defines & tests have been adjusted accordingly.
 */
#define vdi_Text_scratchBufferOffset   (vdi_Text_cell2Size+vdi_Text_cell2OutsidePad)
#if vdi_Text_scratchBufferSize < (2*vdi_Text_scratchBufferOffset)
# error vdi_Text_scratchBufferSize is too small for specified font metrics
#endif
/*
 * end of calculations extracted from vdi_tblit.S
 */

/*
 * Local structure for passing justification info
 *
 * note 1: in each of the following pairs of variables, only one variable
 * has a non-zero value:
 *  (wordx,wordy), (rmwordx,rmwordy), (charx,chary), (rmcharx,rmchary)
 *
 * note 2: if rmword is zero, there are no 'remainder pixels' to use up
 * between words; otherwise, rmword = max(rmwordx/rmwordy).  similarly
 * for rmchar/rmcharx/rmchary.
 */
typedef struct {
    /* word-spacing variables */
    WORD wordx, wordy;          /* #pixels to add to each space */
    WORD rmword;                /* remaining #pixels to add over all spaces */
    WORD rmwordx, rmwordy;      /* add this to use up remainder */
    /* character-spacing variables */
    WORD charx, chary;          /* #pixels to add to each character */
    WORD rmchar;                /* remaining #pixels to add over all characters */
    WORD rmcharx, rmchary;      /* add this to use up remainder */
} vdi_Text_justificationInfo;

/*
 * Calculates height of text string
 */
static WORD vdi_Text_computeHeight(vdi_VirtualWorkstation *vwk)
{
    const Fonthead *fnt_ptr = vwk->cur_font;
    WORD height = fnt_ptr->top + fnt_ptr->bottom + 1;    /* handles scaled fonts */
    if (vwk->style & vdi_TextStyle_outlined)
        height += vdi_outlineThickness * 2;    /* outlining adds 1 pixel all around */
    return height;
}

/*
 * Actual sizer routine
 *
 * entry:
 *   top     - size to scale (lineaVars.text_characterSizeY etc)
 *
 * used variables:
 *   vwk->dda_inc (UWORD) - DDA increment passed externally
 *   vwk->t_sclsts (WORD) - 0 if scale down, 1 if enlarge
 *
 * exit:
 *   actual size
 */
static UWORD vdi_Text_computeActualSize(vdi_VirtualWorkstation * vwk, UWORD top) {
    if (vwk->dda_inc == 0xffff)
        return (top<<1); /* double size */
    UWORD accu = 0x7fff;
    UWORD retval = vwk->t_sclsts ? top : 0;
    for (UWORD i = 0; i < top; i++) {
        accu += vwk->dda_inc;
        if (accu < vwk->dda_inc)
            retval++;
    }
    /* if input is non-zero, make return value at least 1 */
    if (top && !retval)
        retval = 1;
    return retval;
}

/*
 * Calculates width of text string
 */
static WORD vdi_Text_computeWidth(vdi_VirtualWorkstation *vwk, WORD cnt, WORD *str) {
    const Fonthead *fnt_ptr = vwk->cur_font;
    WORD table_start = fnt_ptr->first_ade;
    WORD width = 0;

    if (fnt_ptr->flags & F_MONOSPACE) {
        width = cnt * (fnt_ptr->off_table[1]-fnt_ptr->off_table[0]);
    } else {
        for (WORD i = 0; i < cnt; i++) {
            WORD chr = *str++ - table_start;
            width += fnt_ptr->off_table[chr + 1] - fnt_ptr->off_table[chr];
        }
    }

    if (vwk->scaled) {
        if (vwk->dda_inc == 0xFFFF)
            width *= 2;
        else
            width = vdi_Text_computeActualSize(vwk, width);
    }

    if ((vwk->style & vdi_TextStyle_thickened) && !(fnt_ptr->flags & F_MONOSPACE))
        width += cnt * fnt_ptr->thicken;

    if (vwk->style & vdi_TextStyle_skewed)
        width += fnt_ptr->left_offset + fnt_ptr->right_offset;

    if (vwk->style & vdi_TextStyle_outlined)
        width += cnt * vdi_outlineThickness * 2;   /* outlining adds 1 pixel all around */

    return width;
}

#if CONF_WITH_VDI_TEXT_SPEEDUP
/*
 * returns true if we can use a direct screen blit
 *
 * the following must all be true:
 *  there are no effects
 *  there is no rotation
 *  the output is left-aligned
 *  the output is not justified
 *  the characters are byte-aligned
 *  the font is monospace with a cell width of 8
 *  the font contains glyphs for all 256 characters
 *  the entire text string will not be clipped
 */
static bool vdi_Text_checkForDirectBlit(vdi_VirtualWorkstation *vwk, WORD width, vdi_Text_justificationInfo *justified) {
    if (vwk->style | vwk->chup | vwk->h_align)
        return false;
    if (justified)
        return false;
    if (lineaVars.text_destinationX & 0x0007)
        return false;

    const Fonthead *fnt_ptr = vwk->cur_font;

    if (!lineaVars.font_monospaced || fnt_ptr->max_cell_width != 8)
        return false;

    if (fnt_ptr->first_ade != 0 || fnt_ptr->last_ade != 255)
        return false;

    ClippingRect *clippingRect = &vwk->clippingRect;
    if (vwk->clippingEnabled)
        clippingRect = &vwk->clippingRect;
    else
        clippingRect = &vdi_context.clippingRectFull;

    /* check that string falls entirely within clip area */
    if (lineaVars.text_destinationX < clippingRect->xMin || lineaVars.text_destinationX+width > clippingRect->xMax)
        return false;
    if (lineaVars.text_destinationY < clippingRect->yMin || lineaVars.text_destinationY+lineaVars.text_characterSizeY > clippingRect->yMax)
        return false;

    return true;
}
#endif

/*
 * output specified text string
 *
 * 'width' is the pre-calculated width of the text on the screen;
 * if negative, it has not yet been calculated
 */
static void vdi_Text_output(vdi_VirtualWorkstation *vwk, WORD count, WORD *str, WORD width, vdi_Text_justificationInfo *justified) {
    if (count <= 0) /* quick out for unlikely occurrence */
        return;

    if (width < 0) /* called from vdi_v_gtext() */
        width = vdi_Text_computeWidth(vwk, count, str);

    const Fonthead *fnt_ptr = vwk->cur_font; /* get current font pointer */

    /* some data copying for the assembler part */
    lineaVars.ddaIncrement = vwk->dda_inc;
    lineaVars.text_scaleDirection = vwk->t_sclsts;
    lineaVars.text_scale = vwk->scaled;
    lineaVars.font_monospaced = F_MONOSPACE & fnt_ptr->flags;
    lineaVars.writingMode = vwk->wrt_mode;
    lineaVars.clipping_enabled = vwk->clippingEnabled;
    lineaVars.clipping_rect = vwk->clippingRect;
    lineaVars.font_style = vwk->style;
    lineaVars.text_upVector = vwk->chup;
    lineaVars.text_largeBuffer = vwk->scrpt2;
    lineaVars.text_scratchBuffer = vwk->scrtchp;

    if (vwk->style & vdi_TextStyle_thickened)
        lineaVars.font_weight = fnt_ptr->thicken;

    if (vwk->style & vdi_TextStyle_light)
        lineaVars.font_liteMask = fnt_ptr->lighten;

    WORD d1, d2;
    if (vwk->style & vdi_TextStyle_skewed) {
        d1 = fnt_ptr->left_offset;  /* used in vertical alignment calcs */
        d2 = fnt_ptr->right_offset;
        lineaVars.font_skewMask = fnt_ptr->skew;
    } else {
        d1 = 0;
        d2 = 0;
    }
    lineaVars.font_skewLoff = d1;
    lineaVars.font_skewRoff = d2;

    lineaVars.font_data = fnt_ptr->dat_table;
    lineaVars.font_widthInBytes = fnt_ptr->form_width;

    /*
     * in Atari TOS, outlined text starts 1 pixel earlier than
     * non-outlined, so we set 'outline' to handle that.
     * this also affects horizontal alignment calculations.
     */
    WORD outline = (vwk->style & vdi_TextStyle_outlined) ? vdi_outlineThickness : 0;

    WORD delh, delv;
    switch(vwk->h_align) {
    default: /* normally case 0: left justified */
        delh = 0;
        break;
    case 1:
        delh = width / 2 - outline;
        break;
    case 2:
        delh = width - (outline * 2);
        break;
    }

    switch(vwk->v_align) {
    default: /* normally case 0: baseline */
        delv = fnt_ptr->top;
        delh += d1;
        break;
    case 1:
        delv = fnt_ptr->top - fnt_ptr->half;
        delh += (fnt_ptr->half * d2) / fnt_ptr->top;
        break;
    case 2:
        delv = fnt_ptr->top - fnt_ptr->ascent;
        delh += (fnt_ptr->ascent * d2) / fnt_ptr->top;
        break;
    case 3:
        delv = fnt_ptr->top + fnt_ptr->bottom;
        break;
    case 4:
        delv = fnt_ptr->top + fnt_ptr->descent;
        delh += (fnt_ptr->descent * d1) / fnt_ptr->bottom;
        break;
    case 5:
        delv = 0;
        delh += d1 + d2;
        break;
    }

    /*
     * like Atari TOS, we try to ensure that any underline will fall within
     * the character cell.  if we have sufficient room (e.g. in an 8x16 font),
     * we drop the underline to the bottom line; otherwise it sits on the
     * descent line.  in the latter case, if the font has been doubled, the
     * underline will be thick, and we need to raise it.
     */
    WORD underline;
    if (fnt_ptr->bottom > fnt_ptr->ul_size)             /* normal for 8x16 font */
        underline = 1;
    else if (vwk->scaled && (vwk->dda_inc == 0xffff))   /* doubling size exactly? */
        underline = -1;
    else
        underline = 0;
    underline += fnt_ptr->ul_size;

    WORD startx, starty;
    WORD xfact, yfact;
    Point * point = (Point*)lineaVars.PTSIN;
    switch (vwk->chup) {
    default: /* normally case 0: no rotation */
        lineaVars.text_destinationX = point->x - delh - outline;
        lineaVars.text_destinationY = point->y - delv - outline;
        startx = lineaVars.text_destinationX;
        starty = lineaVars.text_destinationY + fnt_ptr->top + underline;
        xfact = 0;
        yfact = 1;
        break;
    case 900:
        lineaVars.text_destinationX = point->x - delv - outline;
        lineaVars.text_destinationY = point->y + delh + outline + 1;
        startx = lineaVars.text_destinationX + fnt_ptr->top + underline;
        starty = lineaVars.text_destinationY;
        xfact = 1;
        yfact = 0;
        break;
    case 1800:
        lineaVars.text_destinationX = point->x + delh + outline + 1;
        lineaVars.text_destinationY = point->y - ((fnt_ptr->top + fnt_ptr->bottom) - delv) - outline;
        startx = lineaVars.text_destinationX;
        starty = lineaVars.text_destinationY + fnt_ptr->bottom - underline;
        xfact = 0;
        yfact = -1;
        break;
    case 2700:
        lineaVars.text_destinationX = point->x - ((fnt_ptr->top + fnt_ptr->bottom) - delv) - outline;
        lineaVars.text_destinationY = point->y - delh - outline;
        startx = lineaVars.text_destinationX + fnt_ptr->bottom - underline;
        starty = lineaVars.text_destinationY;
        xfact = -1;
        yfact = 0;
        break;
    }

    lineaVars.text_colorForeground = vwk->text_color;
    lineaVars.text_characterSizeY = fnt_ptr->form_height;

#if CONF_WITH_VDI_TEXT_SPEEDUP
    /*
     * call special direct screen blit routine if applicable
     */
    if (vdi_Text_checkForDirectBlit(vwk, width, justified)) {
        vdi_Text_blitDirectlyToScreen(count, str);
        return;
    }
#endif

    lineaVars.ddaX = 32767; /* init the horizontal dda */

    for (WORD j = 0; j < count; j++) {
        WORD temp = str[j];

        /* If the character is out of range for this font make it a ? */
        if (temp < fnt_ptr->first_ade || temp > fnt_ptr->last_ade)
            temp = '?';
        temp -= fnt_ptr->first_ade;

        lineaVars.text_sourceX = fnt_ptr->off_table[temp];
        lineaVars.text_characterSizeX = fnt_ptr->off_table[temp + 1] - lineaVars.text_sourceX;

        lineaVars.text_sourceY = 0;
        lineaVars.text_characterSizeY = fnt_ptr->form_height;

        vdi_Text_blit();

        if (justified) {
            lineaVars.text_destinationX += justified->charx;
            lineaVars.text_destinationY += justified->chary;
            if (justified->rmchar) {
                lineaVars.text_destinationX += justified->rmcharx;
                lineaVars.text_destinationY += justified->rmchary;
                justified->rmchar--;
            }
            if (str[j] == ' ') {
                lineaVars.text_destinationX += justified->wordx;
                lineaVars.text_destinationY += justified->wordy;
                if (justified->rmword) {
                    lineaVars.text_destinationX += justified->rmwordx;
                    lineaVars.text_destinationY += justified->rmwordy;
                    justified->rmword--;
                }
            }
        }
        /* end if justified */
        if (fnt_ptr->flags & F_HORZ_OFF)
            lineaVars.text_destinationX += fnt_ptr->hor_table[temp];
    } /* for j */

    if (vwk->style & vdi_TextStyle_underscored) {
        vdi_DrawContext dc;
        vdi_Line_drawStart(&dc, vwk, vwk->clippingEnabled, vwk->wrt_mode, vwk->text_color);
        dc.line.lastFlag = false;
        dc.line.mask = (vwk->style & vdi_TextStyle_light) ? fnt_ptr->lighten : 0xffff; // Overwrite line-A line mask.

        Line line;
        line.x1 = startx;
        line.y1 = starty;
        if ((vwk->chup == 900) || (vwk->chup == 2700)) {
            line.x2 = line.x1;
            line.y2 = lineaVars.text_destinationY;
        } else {
            line.x2 = lineaVars.text_destinationX;
            line.y2 = line.y1;
        }

        count = fnt_ptr->ul_size;
        for (WORD i = 0; i < count; i++) {
            dc.line.line = line;
            vdi_getDriver()->drawLine(&dc);
            line.x1 += xfact;
            line.x2 += xfact;
            line.y1 += yfact;
            line.y2 += yfact;
            rorw1(dc.line.mask);
        }
        
        vdi_Line_drawEnd(&dc);
    }
}

static void vdi_v_gtext(vdi_VirtualWorkstation * vwk) {
    vdi_Text_output(vwk, lineaVars.parameters.contrl->inputIntNb, lineaVars.INTIN, -1, NULL);
}

#if CONF_WITH_GDOS
/*
 * Converts a font to standard form.
 *
 * The routine just does byte swapping.
 *
 * input:
 *     lineaVars.font_widthInBytes = width of font data in bytes.
 *     lineaVars.text_characterSizeY   = number of scan lines in font.
 *     lineaVars.font_data  = starting address of the font data.
 */
static void vdi_Text_convertFontToStandardForm(void) {
    UWORD *addr = (UWORD *)lineaVars.font_data;
    WORD cnt = (lineaVars.font_widthInBytes * lineaVars.text_characterSizeY) / sizeof(*addr);
    for (WORD i = 0; i < cnt; i++, addr++)
        swpw(*addr);
}
#endif

static void vdi_Text_initialize2(vdi_VirtualWorkstation * vwk) {
    vwk->cur_font = lineaVars.text_defaultFont;
    vwk->loaded_fonts = NULL;
    vwk->scrpt2 = vdi_Text_scratchBufferOffset;
    vwk->scrtchp = vdi_sharedBuffer.common.deftxbuf;
    vwk->num_fonts = lineaVars.text_fontCount;

    vwk->style = 0;        /* reset special effects */
    vwk->scaled = false;
    vwk->h_align = 0;
    vwk->v_align = 0;
    vwk->chup = 0;
    vwk->pts_mode = false;

    lineaVars.text_fontRing[2] = vwk->loaded_fonts;
    lineaVars.workstation_deviceTable[10] = vwk->num_fonts;
}

static void vdi_Text_initialize(void) {
    lineaVars.workstation_sizeTable.charWidthMin = 32767; /* minimal char width */
    lineaVars.workstation_sizeTable.charHeightMin = 32767; /* minimal char height */
    lineaVars.workstation_sizeTable.charWidthMax = 0; /* maximal char width */
    lineaVars.workstation_sizeTable.charHeightMax = 0; /* maximal char height */

    /* Initialize the font ring. */
    lineaVars.text_fontRing[0] = &fon6x6;
    lineaVars.text_fontRing[1] = &fon8x8;
    lineaVars.text_fontRing[2] = NULL;
    lineaVars.text_fontRing[3] = NULL;

    WORD id_save = fon6x6.font_id;

    lineaVars.text_defaultFont = NULL;
    WORD cell_height = (lineaVars.screen_height >= 400) ? 16 : 8;   /* to select among default fonts */

    const Fonthead **chain_ptr = lineaVars.text_fontRing;
    const Fonthead *fnt_ptr;
    WORD i = 0, j = 0;
    while ((fnt_ptr = *chain_ptr++)) {
        do {
            if (fnt_ptr->flags & F_DEFAULT) {   /* If default, save font pointer */
                if (!lineaVars.text_defaultFont)                  /* ... for sure if we don't have one yet */
                    lineaVars.text_defaultFont = fnt_ptr;
                else if (lineaVars.text_defaultFont->form_height != cell_height)
                    lineaVars.text_defaultFont = fnt_ptr;         /* ... also if previously-saved has wrong height */
            }

            if (fnt_ptr->font_id != id_save) {  /* If new font count */
                j++;
                id_save = fnt_ptr->font_id;
            }

            if (fnt_ptr->font_id == 1) {        /* Update lineaVars.workstation_sizeTable if system font */
                if (lineaVars.workstation_sizeTable.charWidthMin > fnt_ptr->max_char_width)
                    lineaVars.workstation_sizeTable.charWidthMin = fnt_ptr->max_char_width;

                if (lineaVars.workstation_sizeTable.charHeightMin > fnt_ptr->top)
                    lineaVars.workstation_sizeTable.charHeightMin = fnt_ptr->top;

                if (lineaVars.workstation_sizeTable.charWidthMax < fnt_ptr->max_char_width)
                    lineaVars.workstation_sizeTable.charWidthMax = fnt_ptr->max_char_width;

                if (lineaVars.workstation_sizeTable.charHeightMax < fnt_ptr->top)
                    lineaVars.workstation_sizeTable.charHeightMax = fnt_ptr->top;
                i++; /* Increment count of heights */
            }
            /* end if system font */

#if CONF_WITH_GDOS
            /*
             * all builtin fonts have standard format, so this test is
             * only useful when we are supporting loaded fonts
             */
            if (!(fnt_ptr->flags & F_STDFORM)) {
                lineaVars.font_data = fnt_ptr->dat_table;
                lineaVars.font_widthInBytes = fnt_ptr->form_width;
                lineaVars.text_characterSizeY = fnt_ptr->form_height;
                vdi_Text_convertFontToStandardForm();
            }
#endif
        } while ((fnt_ptr = fnt_ptr->next_font));
    }
    lineaVars.workstation_deviceTable[5] = i;                     /* number of sizes */
    lineaVars.text_fontCount = lineaVars.workstation_deviceTable[10] = ++j;     /* number of faces */
}

// Set up width/height values returned by vst_height()/vst_point()
static void vdi_Text_setupSize(vdi_VirtualWorkstation * vwk, const Fonthead *font) {
    UWORD top = font->top;
    WORD *p = lineaVars.PTSOUT;
    *p++ = font->max_char_width;
    *p++ = top;
    *p++ = font->max_cell_width;
    *p = top + font->bottom + 1;
    vwk->yFlipped = true;
}

/*
 * input:
 *   req - d0, get requested size
 *   act - d1, get actual size
 *
 * output:
 *   vwk->t_sclsts is the text scaling flag (means: scale up or down)
 */
static UWORD vdi_Text_computeDda(vdi_VirtualWorkstation * vwk, UWORD act, UWORD req) {
    if (req < act) {
        vwk->t_sclsts = 0; /* we do scale down */
        if (req == 0)
            req = 1; /* if 0 then make it 1 (minimum value) */
    } else {
        vwk->t_sclsts = 1; /* we do scale up */
        req -= act;
        /* if larger than 2x? */
        if (req >= act)
            return 0xFFFF; /* indicates 2x scale up */
    }
    return divu(((ULONG)req)<<16, act);
}

static void vdi_Text_makeHeader(vdi_VirtualWorkstation * vwk) {
    const Fonthead *source_font = vwk->cur_font;
    Fonthead *dest_font = &vwk->scratch_head;

    dest_font->font_id = source_font->font_id;
    dest_font->point = source_font->point * 2;

    memcpy(dest_font->name, source_font->name, FONT_NAME_LEN);

    dest_font->first_ade = source_font->first_ade;
    dest_font->last_ade = source_font->last_ade;

    if (vwk->dda_inc == 0xFFFF) {
        dest_font->top = source_font->top * 2 + 1;
        dest_font->ascent = source_font->ascent * 2 + 1;
        dest_font->half = source_font->half * 2 + 1;
    } else {
        dest_font->top = vdi_Text_computeActualSize(vwk, source_font->top);
        dest_font->ascent = vdi_Text_computeActualSize(vwk, source_font->ascent);
        dest_font->half = vdi_Text_computeActualSize(vwk, source_font->half);
    }
    dest_font->descent = vdi_Text_computeActualSize(vwk, source_font->descent);
    dest_font->bottom = vdi_Text_computeActualSize(vwk, source_font->bottom);
    dest_font->max_char_width = vdi_Text_computeActualSize(vwk, source_font->max_char_width);
    dest_font->max_cell_width = vdi_Text_computeActualSize(vwk, source_font->max_cell_width);
    dest_font->left_offset = vdi_Text_computeActualSize(vwk, source_font->left_offset);
    dest_font->right_offset = vdi_Text_computeActualSize(vwk, source_font->right_offset);
    dest_font->thicken = vdi_Text_computeActualSize(vwk, source_font->thicken);
    dest_font->ul_size = vdi_Text_computeActualSize(vwk, source_font->ul_size);

    dest_font->lighten = source_font->lighten;
    dest_font->skew = source_font->skew;
    dest_font->flags = source_font->flags;

    dest_font->hor_table = source_font->hor_table;
    dest_font->off_table = source_font->off_table;
    dest_font->dat_table = source_font->dat_table;

    dest_font->form_width = source_font->form_width;
    dest_font->form_height = source_font->form_height;

    vwk->scaled = true;
    vwk->cur_font = dest_font;
}

static const Fonthead ** vdi_Text_findNextFont(const Fonthead **fontChain, const Fonthead **fontCurrent) {
    const Fonthead *font = *fontCurrent;
    if (font) {
        font = font->next_font;
        if (!font) {
            fontChain++;
            font = *fontChain;
        }
    } else
        font = *fontChain;
    *fontCurrent = font;
    return fontChain;
}

static const Fonthead ** vdi_Text_findNextFontById(const Fonthead **fontChain, const Fonthead **fontCurrent, WORD fontId) {
    const Fonthead *font = *fontCurrent;
    do {
        fontChain = vdi_Text_findNextFont(fontChain, &font);
    } while (font && font->font_id != fontId);
    *fontCurrent = font;
    return fontChain;
}

static void vdi_vst_height(vdi_VirtualWorkstation * vwk) {
    WORD fontId = vwk->cur_font->font_id;
    vwk->pts_mode = false;
    WORD height = lineaVars.PTSIN[1];

    const Fonthead *fontSingle;
    /* Find the smallest font in the requested face */
    const Fonthead *font = NULL, **fontChain = vdi_Text_findNextFontById(lineaVars.text_fontRing, &font, fontId);
    if (font) {
        /* Traverse the chains and find the font closest to the size requested. */
        do {
            fontSingle = font;
            fontChain = vdi_Text_findNextFont(fontChain, &font);
        } while (font && font->top <= height && font->font_id == fontId);
    } else {
        // Internal error.
        fontSingle = vwk->cur_font;
    }

    /* Set up environment for this font in the non-scaled case */
    vwk->cur_font = fontSingle;
    vwk->scaled = false;

    if (fontSingle->top != height) {
        vwk->dda_inc = vdi_Text_computeDda(vwk, fontSingle->top, height);
        vdi_Text_makeHeader(vwk);
        fontSingle = vwk->cur_font;
    }

    vdi_Text_setupSize(vwk, fontSingle);    /* set up return values */
}

static void vdi_vst_point(vdi_VirtualWorkstation * vwk) {
    WORD fontId = vwk->cur_font->font_id;
    vwk->pts_mode = true;
    WORD point = lineaVars.INTIN[0];

    /* Find the smallest font in the requested face */
    const Fonthead *fontSingle, *fontDouble;
    const Fonthead *font = NULL, **fontChain = vdi_Text_findNextFontById(lineaVars.text_fontRing, &font, fontId);
    if (font) {
        /* Traverse the chains and find the font closest to the size requested and closest to half the size requested. */
        fontDouble = font;
        do {
            fontSingle = font;
            if (font->point * 2 <= point)
                fontDouble = font;
             fontChain = vdi_Text_findNextFont(fontChain, &font);
        } while (font && font->point <= point && font->font_id == fontId);
    } else {
        // Internal error.
        fontSingle = fontDouble = vwk->cur_font;
    }

    /* Set up environment for this font in the non-scaled case */
    vwk->cur_font = fontSingle;
    vwk->scaled = false;

    if (fontSingle->point != point) {
        WORD pointDouble = fontDouble->point * 2;
        if (pointDouble > fontSingle->point && pointDouble <= point) {
            vwk->dda_inc = 0xFFFF;
            vwk->t_sclsts = 1;
            vwk->cur_font = fontDouble;
            vdi_Text_makeHeader(vwk);
            fontSingle = vwk->cur_font;
        }
    }

    vdi_Text_setupSize(vwk, fontSingle); /* set up return values */

    /* also return point size actually set */
    lineaVars.INTOUT[0] = fontSingle->point;
}

static void vdi_vst_effects(vdi_VirtualWorkstation * vwk) {
    lineaVars.INTOUT[0] = vwk->style = lineaVars.INTIN[0] & lineaVars.workstation_inquireTable[2];
}

static void vdi_vst_alignment(vdi_VirtualWorkstation * vwk) {
    WORD *int_in = lineaVars.INTIN;
    WORD *int_out = lineaVars.INTOUT;

    WORD a = *int_in++;
    if (a < 0 || a > 2)
        a = 0;
    vwk->h_align = *int_out++ = a;

    a = *int_in;
    if (a < 0 || a > 5)
        a = 0;
    vwk->v_align = *int_out = a;
}

/*
 * the following implementation mimics TOS3/TOS4 behaviour
 *
 * the rotation angle in tenths of a degree may be supplied as any
 * positive or negative WORD value, but it is first normalised to
 * a value between 0 & 3599 by adding or subtracting multiples of
 * 3600, and then rounded to the nearest multiple of 900 between
 * 0 and 3600 inclusive before storing.
 *
 * trivia: a value of 3600 is handled by other VDI code in the same
 * way as a value of 0, but if you really want to store a value of
 * 3600 rather than 0, you can pass a value between 3150 and 3599
 * inclusive ...
 *
 * TOS1 difference: TOS1 does not normalise before rounding, so
 * stored values range from -32400 to +32400; values outside the
 * range of 0 to 3600 inclusive cause vqt_extent() to not return
 * values ...
 */
static void vdi_vst_rotation(vdi_VirtualWorkstation * vwk) {
    WORD angle = lineaVars.INTIN[0];
    while (angle < 0)        /* ensure positive angle */
        angle += 3600;
    while (angle >= 3600)    /* between 0 and 3599 inclusive */
        angle -= 3600;
    /* this sets a value of 0, 900, 1800, 2700 or 3600, just like TOS3/TOS4 */
    lineaVars.INTOUT[0] = vwk->chup = ((angle + 450) / 900) * 900;
}

static void vdi_vst_font(vdi_VirtualWorkstation * vwk) {
    WORD dummy[4];
    const Fonthead *fontCurrent = vwk->cur_font;
    WORD point = fontCurrent->point;
    dummy[1] = fontCurrent->top;
    
    WORD fontId = lineaVars.INTIN[0];
    const Fonthead *font = NULL;
    vdi_Text_findNextFontById(lineaVars.text_fontRing, &font, fontId);
    if (!font)
        font = &fon6x6; /* Default to the system font. */
    vwk->cur_font = font;

    /* Call down to the set text height routine to get the proper size */
    WORD *old_intin = lineaVars.INTIN;
    WORD *old_ptsin = lineaVars.PTSIN;
    WORD *old_ptsout = lineaVars.PTSOUT;
    lineaVars.INTIN = &point;
    lineaVars.PTSIN = lineaVars.PTSOUT = dummy;
    if (vwk->pts_mode)
        vdi_vst_point(vwk);
    else
        vdi_vst_height(vwk);
    lineaVars.INTIN = old_intin;
    lineaVars.PTSIN = old_ptsin;
    lineaVars.PTSOUT = old_ptsout;

    lineaVars.INTOUT[0] = vwk->cur_font->font_id;
}

static void vdi_vst_color(vdi_VirtualWorkstation * vwk) {
    WORD r = vdi_Color_validateIndex(lineaVars.INTIN[0]);
    lineaVars.INTOUT[0] = r;
    vwk->text_color = vdi_context.palette.penToPaletteTable[r];
}

static void vdi_vqt_attributes(vdi_VirtualWorkstation * vwk) {
    WORD *pointer = lineaVars.INTOUT;
    const Fonthead *fnt_ptr = vwk->cur_font;
    *pointer++ = fnt_ptr->font_id;      /* lineaVars.INTOUT[0] */
    *pointer++ = vdi_context.palette.paletteToPenTable[vwk->text_color];     /* lineaVars.INTOUT[1] */
    *pointer++ = vwk->chup;        /* lineaVars.INTOUT[2] */
    *pointer++ = vwk->h_align;     /* lineaVars.INTOUT[3] */
    *pointer++ = vwk->v_align;     /* lineaVars.INTOUT[4] */
    *pointer = vwk->wrt_mode + 1;  /* lineaVars.INTOUT[5] */

    pointer = lineaVars.PTSOUT;
    *pointer++ = fnt_ptr->max_char_width;
    *pointer++ = fnt_ptr->top;
    *pointer++ = fnt_ptr->max_cell_width;
    *pointer = fnt_ptr->top + fnt_ptr->bottom + 1;  /* handles scaled fonts */

    vwk->yFlipped = true;
}

static void vdi_vqt_extent(vdi_VirtualWorkstation * vwk) {
    WORD height = vdi_Text_computeHeight(vwk);
    WORD width = vdi_Text_computeWidth(vwk, lineaVars.parameters.contrl->inputIntNb, lineaVars.INTIN);
    bzero(lineaVars.PTSOUT,8*sizeof(WORD));
    switch (vwk->chup) {
    default: /* 0 or 3600 ... see vdi_vst_rotation() */
        lineaVars.PTSOUT[2] = width;
        lineaVars.PTSOUT[4] = width;
        lineaVars.PTSOUT[5] = height;
        lineaVars.PTSOUT[7] = height;
        break;
    case 900:
        lineaVars.PTSOUT[0] = height;
        lineaVars.PTSOUT[2] = height;
        lineaVars.PTSOUT[3] = width;
        lineaVars.PTSOUT[5] = width;
        break;
    case 1800:
        lineaVars.PTSOUT[0] = width;
        lineaVars.PTSOUT[1] = height;
        lineaVars.PTSOUT[3] = height;
        lineaVars.PTSOUT[6] = width;
        break;
    case 2700:
        lineaVars.PTSOUT[1] = width;
        lineaVars.PTSOUT[4] = height;
        lineaVars.PTSOUT[6] = height;
        lineaVars.PTSOUT[7] = width;
        break;
    }
    vwk->yFlipped = true;
}

static void vdi_vqt_width(vdi_VirtualWorkstation * vwk) {
    const Fonthead *fnt_ptr = vwk->cur_font;
    WORD *pointer = lineaVars.PTSOUT;

    /* Set that there is no horizontal offset */
    *(pointer + 2) = 0;
    *(pointer + 4) = 0;

    WORD k = lineaVars.INTIN[0];
    if ((k < fnt_ptr->first_ade) || (k > fnt_ptr->last_ade))
        lineaVars.INTOUT[0] = -1;
    else {
        lineaVars.INTOUT[0] = k;
        k -= fnt_ptr->first_ade;
        *(pointer) = fnt_ptr->off_table[k + 1] - fnt_ptr->off_table[k];
        if (vwk->scaled) {
            if (vwk->dda_inc == 0xFFFF)
                *pointer *= 2;
            else
                *pointer = vdi_Text_computeActualSize(vwk, *pointer);
        }
        if (fnt_ptr->flags & F_HORZ_OFF) {
            *(pointer + 2) = fnt_ptr->hor_table[k * 2];
            *(pointer + 4) = fnt_ptr->hor_table[(k * 2) + 1];
        }
    }

    vwk->yFlipped = true;
}

static void vdi_vqt_name(vdi_VirtualWorkstation * vwk) {
    WORD element = lineaVars.INTIN[0], fontIndex = 0;
    const Fonthead **fontChain = lineaVars.text_fontRing, *font = *fontChain;
    while (font) {
        fontIndex++;
        if (fontIndex == element)
            break;
        fontChain = vdi_Text_findNextFont(fontChain, &font);
    }
    
    /* The element is out of bounds - default to the system font */
    if (!font)
        font = &fon6x6;

    WORD *int_out = lineaVars.INTOUT;
    *int_out++ = font->font_id;
    const char *name = font->name;
    WORD i;
    for (i = 0; (*int_out++ = *name++); i++) ;
    while (i < FONT_NAME_LEN) {
        *int_out++ = 0;
        i++;
    }
}

static void vdi_vqt_fontinfo(vdi_VirtualWorkstation * vwk) {
    const Fonthead *fnt_ptr = vwk->cur_font;

    WORD *pointer = lineaVars.INTOUT;
    *pointer++ = fnt_ptr->first_ade;
    *pointer = fnt_ptr->last_ade;

    pointer = lineaVars.PTSOUT;
    *pointer++ = fnt_ptr->max_cell_width;
    *pointer++ = fnt_ptr->bottom;

    if (vwk->style & vdi_TextStyle_thickened)
        *pointer++ = fnt_ptr->thicken;
    else
        *pointer++ = 0;

    *pointer++ = fnt_ptr->descent;

    if (vwk->style & vdi_TextStyle_skewed) {
        *pointer++ = fnt_ptr->left_offset;
        *pointer++ = fnt_ptr->half;
        *pointer++ = fnt_ptr->right_offset;
    } else {
        *pointer++ = 0;
        *pointer++ = fnt_ptr->half;
        *pointer++ = 0;
    }

    *pointer++ = fnt_ptr->ascent;
    *pointer++ = 0;
    *pointer = fnt_ptr->top;

    vwk->yFlipped = true;
}

static void vdi_Gdp_drawJustifiedText(vdi_VirtualWorkstation * vwk) {
    WORD *pointer = lineaVars.INTIN;
    WORD interword = *pointer++;
    WORD interchar = *pointer++;
    WORD *str = pointer;

    /*
     * if interword adjustment required, count spaces
     */
    WORD cnt = lineaVars.parameters.contrl->inputIntNb - 2;
    WORD spaces = 0;
    if (interword) {
        for (WORD i = 0; i < cnt; i++)
            if (*(pointer++) == ' ')
                spaces++;
    }

    WORD width = vdi_Text_computeWidth(vwk, cnt, str);

    WORD max_x = lineaVars.PTSIN[2];

    vdi_Text_justificationInfo just;
    bzero(&just, sizeof(vdi_Text_justificationInfo));     /* set zero default values */

    /*
     * calculate values for interword spacing
     */
    WORD direction;
    if (interword && spaces) {
        WORD delword = (max_x - width) / spaces;
        just.rmword = (max_x - width) % spaces;

        if (just.rmword < 0) {
            direction = -1;
            just.rmword = 0 - just.rmword;
        } else
            direction = 1;

        if (interchar) {
            WORD expand = vwk->cur_font->max_cell_width / 2;
            if (delword > expand) {
                delword = expand;
                just.rmword = 0;
            }
            if (delword < (0 - expand)) {
                delword = 0 - expand;
                just.rmword = 0;
            }
            width += (delword * spaces) + (just.rmword * direction);
        }

        switch (vwk->chup) {
        default:                /* normally case 0: no rotation */
            just.wordx = delword;
            just.rmwordx = direction;
            break;
        case 900:
            just.wordy = 0 - delword;
            just.rmwordy = 0 - direction;
            break;
        case 1800:
            just.wordx = 0 - delword;
            just.rmwordx = 0 - direction;
            break;
        case 2700:
            just.wordy = delword;
            just.rmwordy = direction;
            break;
        }
    }

    /*
     * calculate values for intercharacter spacing
     */
    if (interchar && cnt > 1) {
        WORD delchar = (max_x - width) / (cnt - 1);
        just.rmchar = (max_x - width) % (cnt - 1);

        if (just.rmchar < 0) {
            direction = -1;
            just.rmchar = 0 - just.rmchar;
        } else
            direction = 1;

        switch (vwk->chup) {
        default:                /* normally case 0: no rotation */
            just.charx = delchar;
            just.rmcharx = direction;
            break;
        case 900:
            just.chary = 0 - delchar;
            just.rmchary = 0 - direction;
            break;
        case 1800:
            just.charx = 0 - delchar;
            just.rmcharx = 0 - direction;
            break;
        case 2700:
            just.chary = delchar;
            just.rmchary = direction;
            break;
        }
    }

    vdi_Text_output(vwk, cnt, str, max_x, &just);
}

static void vdi_vst_load_fonts(vdi_VirtualWorkstation * vwk) {
    WORD count = 0; /* If no GDOS, we loaded no new fonts */
#if CONF_WITH_GDOS
    /* Init some common variables */
    WORD *control = lineaVars.CONTRL;

    /* You only get one chance to load fonts.  If fonts are linked in, exit  */
    if (vwk->loaded_fonts) {
        lineaVars.INTOUT[0] = 0;
        return;
    }

    /* The inputs to this routine are :         */
    /* lineaVars.CONTRL[7-8]   = Pointer to scratch buffer    */
    /* lineaVars.CONTRL[9]     = Offset to buffer 2       */
    /* lineaVars.CONTRL[10-11] = Pointer to first font    */

    /* Init the global structures */
    vwk->scrpt2 = control[9];
    vwk->scrtchp = (WORD *) ULONG_AT(&control[7]);

    Fonthead *first_font = (Fonthead *) ULONG_AT(&control[10]);
    vwk->loaded_fonts = first_font;

    /* Find out how many distinct font id numbers have just been linked in. */
    WORD id = -1;
    do {
        /* Update the count of font id numbers, if necessary. */
        if (first_font->font_id != id) {
            id = first_font->font_id;
            count++;
        }

        /* Make sure the font is in device specific format. */
        if (!(first_font->flags & F_STDFORM)) {
            lineaVars.font_data = first_font->dat_table;
            lineaVars.font_widthInBytes = first_font->form_width;
            lineaVars.text_characterSizeY = first_font->form_height;
            vdi_Text_convertFontToStandardForm();
            first_font->flags |= F_STDFORM;
        }
        first_font = first_font->next_font;
    } while (first_font);

    /* Update the device table count of faces. */
    vwk->num_fonts += count;

    lineaVars.text_fontRing[2] = vwk->loaded_fonts;
#endif
    lineaVars.INTOUT[0] = count;
}

static void vdi_vst_unload_fonts(vdi_VirtualWorkstation * vwk) {
#if CONF_WITH_GDOS
    /* Since we always unload all fonts, this is easy. */
    lineaVars.text_fontRing[2] = NULL;
    vwk->loaded_fonts = NULL;           /* No fonts installed */
    vwk->scrpt2 = vdi_Text_scratchBufferOffset;    /* Reset pointers to default buffers */
    vwk->scrtchp = vdi_sharedBuffer.common.deftxbuf;
    vwk->num_fonts = lineaVars.text_fontCount;        /* Reset font count to default */
#endif
}

//--------------------------------------------------------------------------------
// Gdp.
//--------------------------------------------------------------------------------
#define vdi_Gdp_pointNumMin 32  /* min # of points to use when drawing circle/ellipse */
#define vdi_Gdp_pointNumMax 128 /* max # of points ... (must not exceed CONF_VDI_MAX_VERTICES) */

// The number of points used to describe the corners of a rounded rectangle. This should be 5 for TOS visual compatibility.
#define vdi_roundedRectangleCornerPointNum 5
#define vdi_roundedRectanglePointNum (4*vdi_roundedRectangleCornerPointNum+1) // For polyline / wideline.
#define vdi_roundedRectangleFilledPointNum (4*vdi_roundedRectangleCornerPointNum) // For polygon.

// Local GDP variables.
// These are used to pass values to vdi_Gdp_computeArc(), vdi_Gdp_computeNumberOfLineSegment(), and vdi_Gdp_computePoints()
typedef struct {
    WORD primitive;
    WORD xc, yc;
    WORD xrad, yrad;
    WORD angleStart, angleDel, angleEnd;
} vdi_Gdp;

// Definitions for sine and cosine.
#define vdi_halfPi 900
#define vdi_pi 1800
#define vdi_twoPi 3600

// The maximum value (in tenths of a degree) that can be looked up in vdi_sinTable[] below.
#define vdi_sinTableAngleMax 896
#define vdi_sinTableSize ((vdi_sinTableAngleMax/8)+1)

// Precalculated sine/cosine values used in vdi_Gdp_drawRoundedBox(). NOTE: these are scaled to a max value of 32767.
#define vdi_sin225 12539
#define vdi_sin450 23170
#define vdi_sin675 30273
#define vdi_cos225 vdi_sin675
#define vdi_cos450 vdi_sin450
#define vdi_cos675 vdi_sin225

/*
 * Sines of angles 0-90 degrees in 0.8 degree steps, normalized to 0-65536
 *
 * NOTE: because of the construction of this table, there is no entry for
 * exactly 90 degrees (the final entry is for 89.6 degrees).  We take
 * advantage of this to scale to a maximum value which we cannot actually
 * return.  We special-case the maximum value in vdi_Gdp_computePoints() which is the
 * only place where we use this table.
 */
static const UWORD vdi_sinTable[vdi_sinTableSize] = {
        0,   915,  1830,  2744,  3658,  4572,  5484,  6395,
     7305,  8214,  9121, 10026, 10929, 11831, 12729, 13626,
    14519, 15410, 16298, 17183, 18064, 18942, 19816, 20686,
    21553, 22415, 23272, 24125, 24974, 25817, 26656, 27489,
    28317, 29140, 29956, 30767, 31572, 32371, 33163, 33949,
    34729, 35501, 36267, 37026, 37777, 38521, 39258, 39986,
    40708, 41421, 42126, 42823, 43511, 44191, 44862, 45525,
    46179, 46824, 47459, 48086, 48703, 49310, 49908, 50496,
    51075, 51643, 52201, 52750, 53287, 53815, 54332, 54838,
    55334, 55819, 56293, 56756, 57208, 57649, 58078, 58497,
    58903, 59299, 59683, 60055, 60415, 60764, 61101, 61426,
    61739, 62040, 62328, 62605, 62870, 63122, 63362, 63589,
    63804, 64007, 64197, 64375, 64540, 64693, 64833, 64960,
    65075, 65177, 65266, 65343, 65407, 65458, 65496, 65522,
    65534
};

// Returns integer sine between 0 and 32767.
// Input is the angle in tenths of a degree, 0 to 900.
static UWORD vdi_sin(WORD angle) {
    UWORD index = angle >> 3;
    UWORD remainder = angle & 7;
    UWORD tmpsin = vdi_sinTable[index];
    if (remainder != 0) // Add interpolation.
        tmpsin += ((vdi_sinTable[index + 1] - tmpsin) * remainder) >> 3;
    return tmpsin;
}

// Return integer cosine between 0 and 32767.
// Input is the angle in tenths of a degree, 0 to 900.
static UWORD vdi_cos(UWORD angle) {
    return vdi_sin(vdi_halfPi-angle);
}

// Calculates and saves an endpoint position (in raster coordinates), based on the input angle and vdi_context.gdp.xc/vdi_context.gdp.yc/vdi_context.gdp.xrad/vdi_context.gdp.yrad.
#define vdi_Gdp_computePointsFlags_negativeX 0x02 // Values in 'negative' flag below
#define vdi_Gdp_computePointsFlags_negativeY 0x01
static void vdi_Gdp_computePoints(vdi_Gdp *gdp, Point *point, WORD angle) {
    WORD negative = vdi_Gdp_computePointsFlags_negativeY; /* default for first quadrant */
    while (angle >= vdi_twoPi) /* normalise angle to 0-3599 inclusive */
        angle -= vdi_twoPi;
    if (angle > 3 * vdi_halfPi) { /* fourth quadrant */
        angle = vdi_twoPi - angle;
        negative = 0;
    } else if (angle > vdi_pi) { /* third quadrant */
        angle -= vdi_pi;
        negative = vdi_Gdp_computePointsFlags_negativeX;
    } else if (angle > vdi_halfPi) { /* second quadrant */
        angle = vdi_pi - angle;
        negative = vdi_Gdp_computePointsFlags_negativeX|vdi_Gdp_computePointsFlags_negativeY;
    }

    /* handle the values not handled by the table */
    WORD xdiff, ydiff;
    if (angle > vdi_sinTableAngleMax) {
        xdiff = 0;
        ydiff = gdp->yrad;
    } else if (angle < vdi_halfPi-vdi_sinTableAngleMax) {
        xdiff = gdp->xrad;
        ydiff = 0;
    } else {
        xdiff = umul_shift(vdi_cos(angle), gdp->xrad);
        ydiff = umul_shift(vdi_sin(angle), gdp->yrad);
    }
    if (negative & vdi_Gdp_computePointsFlags_negativeX)
        xdiff = -xdiff;
    if (negative & vdi_Gdp_computePointsFlags_negativeY)
        ydiff = -ydiff;
    point->x = gdp->xc + xdiff;
    point->y = gdp->yc + ydiff;
}

// Calculates the positions of all the points necessary to draw a circular/elliptical arc (or a circle/ellipse), and draws it
static void vdi_Gdp_computeArc(vdi_VirtualWorkstation * RESTRICT vwk, vdi_Gdp * RESTRICT gdp, WORD steps) {
    Point *points = (Point *)lineaVars.PTSIN;

    Point * RESTRICT point = points;
    WORD start = gdp->angleStart;
    vdi_Gdp_computePoints(gdp, point++, start);
    for (WORD i = 1; i < steps; i++) {
        WORD angle = mul_div(gdp->angleDel, i, steps) + start;
        vdi_Gdp_computePoints(gdp, point, angle);
        if (*(LONG *)point != *(LONG *)(point-1))   /* ignore duplicates */
            point++;
    }
    vdi_Gdp_computePoints(gdp, point++, gdp->angleEnd);
    steps = point - points; /* number of points, not number of steps */

    // If pie wedge, draw to center and then close (when 'polygon' runs, it always connects the first point to the last).
    if (gdp->primitive == vdi_GdpType_pieSlice || gdp->primitive == vdi_GdpType_ellipticalPieSlice) { /* v_pieslice()/v_ellpie() */
        point->x = gdp->xc;
        point->y = gdp->yc;
        steps++;
    }

    // If arc or ellarc, we draw a line; otherwise (pieslice, circle, ellipse, ellpie), we draw a polygon.
    if (gdp->primitive == vdi_GdpType_arc || gdp->primitive == vdi_GdpType_ellipticalArc) { /* v_arc() or v_ellarc() */
        if (vwk->line_width == 1) {
            vdi_Line_setMask(vwk);
            vdi_Line_drawPoly(vwk, points, steps, vwk->line_color, false);
            // If the ends are arrowed, output them.
            if ((vwk->line_beg | vwk->line_end) & vdi_LineEndStyle_arrow)
                vdi_Arrow_draw(vwk, points, steps);
        } else
            vdi_WideLine_draw(vwk, points, steps);
    } else
        vdi_Polygon_fill(vwk, points, steps); // TODO: For circles and ellipses, we could use a simpler method to computes points and fills the shape.
}

/*
 * Calculates the number of line segments ('steps') to draw for a circle/ellipse,
 * based on the larger of vdi_context.gdp.xrad/vdi_context.gdp.yrad,
 * and clamped to a range of vdi_Gdp_pointNumMin -> vdi_Gdp_pointNumMax
 */
static WORD vdi_Gdp_computeNumberOfLineSegment(vdi_Gdp *gdp) {
    WORD steps = max(gdp->xrad, gdp->yrad);
    steps >>= 2;
    if (steps < vdi_Gdp_pointNumMin)
        steps = vdi_Gdp_pointNumMin;
    else if (steps > vdi_Gdp_pointNumMax)
        steps = vdi_Gdp_pointNumMax;
    return steps;
}

// Handles all circle/ellipse GDP functions:
// v_arc(), v_pieslice(), v_circle(), v_ellipse(), v_ellarc(), v_ellpie()
static void vdi_Gdp_computeCurve(vdi_VirtualWorkstation *vwk) {
    vdi_Gdp gdp;
    gdp.primitive = lineaVars.parameters.contrl->subOpcode;

    WORD *ptsin = lineaVars.PTSIN;
    WORD xc = ptsin[0], yc = ptsin[1];
    gdp.xc = xc; gdp.yc = yc;

    WORD xrad, yrad;
    if (gdp.primitive <= vdi_GdpType_circle) { /* v_arc(), v_pieslice(), v_circle() */
        xrad = (gdp.primitive == vdi_GdpType_circle) ? ptsin[4] : ptsin[6];
        yrad = mul_div(xrad, vdi_deviceSizeX, vdi_deviceSizeY);
    } else { /* v_ellipse(), v_ellarc(), v_ellpie() */
        xrad = ptsin[2];
        yrad = ptsin[3];
        if (vwk->xfm_mode < 2) /* NDC coordinates ... not tested AFAIK */
            yrad = lineaVars.screen_height - yrad;
    }

    // Atari TOS handles negative radii more-or-less the same as positive ones. We explicitly treat them the same.
    if (xrad < 0)
        xrad = -xrad;
    if (yrad < 0)
        yrad = -yrad;
    gdp.xrad = xrad; gdp.yrad = yrad;

    // We can quit now if clipping excludes the entire curve.
    if (vdi_ClipRect_checkRect(&vwk->clippingRect, xc - xrad, yc - yrad, xc + xrad, yc + yrad))
        return;

    WORD angleBegin, angleEnd;
    if (gdp.primitive == vdi_GdpType_circle || gdp.primitive == vdi_GdpType_ellipse) { /* v_circle(), v_ellipse() */
        angleBegin = 0;
        angleEnd = vdi_twoPi;
    } else {
        WORD *intin = lineaVars.INTIN;
        angleBegin = intin[0];
        angleEnd = intin[1];
    }
    gdp.angleStart = angleBegin;
    gdp.angleEnd = angleEnd;

    WORD angleRange = angleEnd - angleBegin;
    if (angleRange < 0)
        angleRange += vdi_twoPi;
    gdp.angleDel = angleRange;

    WORD steps = vdi_Gdp_computeNumberOfLineSegment(&gdp);
    vdi_Gdp_computeArc(vwk, &gdp, steps);
}

// Implements v_rbox(), v_rfbox()
static void vdi_Gdp_drawRoundedBox(vdi_VirtualWorkstation *vwk) {
    WORD *ptsin = lineaVars.PTSIN;

    // Set (x1,y1) to LL corner of box, (x2,y2) to UR corner of box
    Line * line = (Line*)ptsin;
    vdi_Line_sortVertices(line);
    WORD x1 = line->x1, y1 = line->y1, x2 = line->x2, y2 = line->y2;

    /*
     * calculate x & y radii:
     * . the x radius is nominally 1/64th of the screen width
     * . because the corners are nominally quadrants of a circle, we
     *   scale the y radius according to pixel dimensions
     * . we clamp both radii to a maximum of half the length of the
     *   corresponding box side
     */
    WORD xradius = min(vdi_deviceResolutionX >> 6, (x2 - x1) / 2);
    WORD yradius = min(mul_div(xradius, vdi_deviceSizeX, vdi_deviceSizeY),(y1-y2)/2);

    /*
     * for each corner we generate 5 points.  the following calculates
     * the unsigned offset of those points from the centre of the
     * 'circle', one quarter of which is drawn at each box corner.
     */
    WORD xoff[vdi_roundedRectangleCornerPointNum], yoff[vdi_roundedRectangleCornerPointNum];
    xoff[0] = 0;
    xoff[1] = mul_div(vdi_cos675, xradius, 32767);
    xoff[2] = mul_div(vdi_cos450, xradius, 32767);
    xoff[3] = mul_div(vdi_cos225, xradius, 32767);
    xoff[4] = xradius;
    yoff[0] = yradius;
    yoff[1] = mul_div(vdi_sin675, yradius, 32767);
    yoff[2] = mul_div(vdi_sin450, yradius, 32767);
    yoff[3] = mul_div(vdi_sin225, yradius, 32767);
    yoff[4] = 0;

    /*
     * now we fill in lineaVars.PTSIN, starting with the UR corner of the box
     *
     * we first calculate the centre of the circle used for the quadrant
     * and then add in the offset (appropriately signed)
     */
    WORD *p = ptsin;
    WORD xcentre = x2 - xradius, ycentre = y2 + yradius;
    WORD *xp = xoff, *yp = yoff;
    for (WORD i = 0; i < vdi_roundedRectangleCornerPointNum; i++) {
        *p++ = xcentre + *xp++;
        *p++ = ycentre - *yp++;
    }

    /*
     * handle LR corner: note that the offset sequence is reversed
     * xcentre, xp and yp are already set correctly
     */
    ycentre = y1 - yradius;
    for (WORD i = 0; i < vdi_roundedRectangleCornerPointNum; i++) {
        *p++ = xcentre + *--xp;
        *p++ = ycentre + *--yp;
    }

    /*
     * handle LL corner
     * ycentre, xp and yp are already set correctly
     */
    xcentre = x1 + xradius;
    for (WORD i = 0; i < vdi_roundedRectangleCornerPointNum; i++) {
        *p++ = xcentre - *xp++;
        *p++ = ycentre + *yp++;
    }

    /*
     * handle UL corner: the offset sequence is reversed here too
     * xcentre, xp and yp are already set correctly
     */
    ycentre = y2 + yradius;
    for (WORD i = 0; i < vdi_roundedRectangleCornerPointNum; i++) {
        *p++ = xcentre - *--xp;
        *p++ = ycentre - *--yp;
    }

    if (lineaVars.parameters.contrl->subOpcode == 8) {
        /* v_rbox() */

        vdi_Line_setMask(vwk);

        /* join up the polyline */
        *p++ = lineaVars.PTSIN[0];
        *p = lineaVars.PTSIN[1];

        if (vwk->line_width == 1)
            vdi_Line_drawPoly(vwk, (Point*)ptsin, vdi_roundedRectanglePointNum, vwk->line_color, false);
        else
            vdi_WideLine_draw(vwk, (Point*)ptsin, vdi_roundedRectanglePointNum);
    } else {
        /* v_rfbox() */
        vdi_Polygon_fill(vwk, (Point*)ptsin, vdi_roundedRectangleFilledPointNum); // polygon() will join up the first & last points itself.
    }
}

// Major opcode for graphics device primitives.
static void vdi_v_gdp(vdi_VirtualWorkstation * vwk) {
    switch (lineaVars.parameters.contrl->subOpcode) {
    case vdi_GdpType_bar: /* GDP BAR - converted to alpha 2 RJG 12-1-84 */
        vdi_vr_recfl(vwk);
        if (vwk->fill_per) {
            lineaVars.line_mask = 0xffff;
            WORD *ptsin = lineaVars.PTSIN;
            ptsin[5] = ptsin[7] = ptsin[3];
            ptsin[3] = ptsin[9] = ptsin[1];
            ptsin[4] = ptsin[2];
            ptsin[6] = ptsin[8] = ptsin[0];
            vdi_Line_drawPoly(vwk, (Point*)ptsin, 5, vwk->fill_color, false);
        }
        break;

    case vdi_GdpType_arc:
    case vdi_GdpType_pieSlice:
    case vdi_GdpType_circle:
    case vdi_GdpType_ellipse:
    case vdi_GdpType_ellipticalArc:
    case vdi_GdpType_ellipticalPieSlice:
        vdi_Gdp_computeCurve(vwk);
        break;

    case vdi_GdpType_roundedBox:
        {
            WORD save_beg = vwk->line_beg;
            WORD save_end = vwk->line_end;
            vwk->line_beg = vdi_LineEndStyle_square;
            vwk->line_end = vdi_LineEndStyle_square;
            vdi_Gdp_drawRoundedBox(vwk);
            vwk->line_beg = save_beg;
            vwk->line_end = save_end;
        }
        break;

    case vdi_GdpType_filledRoundedBox:
        vdi_Gdp_drawRoundedBox(vwk);
        break;

    case vdi_GdpType_justifiedText:
        vdi_Gdp_drawJustifiedText(vwk);
        break;
#if vdi_Bezier_enabled
    case vdi_GdpType_bezier:
        v_bez_control(vwk); /* check, if we can do bezier curves */
        break;
#endif
    }
}

//--------------------------------------------------------------------------------
// Timer.
//--------------------------------------------------------------------------------
/*
 * VDI Timer interrupt routine
 * The etv_timer does point to this routine
 */
static void vdi_Timer_handleInterrupt(int u) {
    if (!vdi_context.timer.processing) {
        vdi_context.timer.processing = 1; /* set flag, that we are running. MAD: evtl. registers to stack */
        lineaVars.timer_handle(u); /* call the timer vector and back from stack */
    }
    vdi_context.timer.processing = 0; /* allow yet another trip through. MAD: evtl. registers to stack */
    lineaVars.timer_chain(u); /* call the old timer vector too and back from stack */
}

/*
 * Exchange timer interrupt vector
 * entry: new vector in lineaVars.CONTRL[7-8]
 * exit:  old vector in lineaVars.CONTRL[9-10]
 */
static void vdi_vex_timv(vdi_VirtualWorkstation * vwk) {
    WORD old_sr = set_sr(0x2700);
    ULONG_AT(&lineaVars.CONTRL[9]) = (ULONG) lineaVars.timer_handle;
    lineaVars.timer_handle = (ETV_TIMER_T) ULONG_AT(&lineaVars.CONTRL[7]);
    set_sr_only(old_sr);

    lineaVars.INTOUT[0] = (WORD)Tickcal(); /* ms between timer C calls */
    lineaVars.parameters.contrl->outputIntNb = 1;
}

/*
 * Initialize the timer
 * initially set timer vector to dummy, save old vector
 */
static void vdi_Timer_initialize(void) {
    vdi_context.timer.processing = 0; /* no vblanks in process */
    /* Now initialize the lower level things */
    lineaVars.timer_handle = (ETV_TIMER_T)just_rts; /* tick points to rts */
    WORD old_sr = set_sr(0x2700);/* disable interrupts */
    lineaVars.timer_chain = (ETV_TIMER_T)Setexc(0x100, (long)vdi_Timer_handleInterrupt); /* save old vector and set etv_timer to vdi_Timer_handleInterrupt */
    set_sr_only(old_sr); /* enable interrupts */
}

/*
 * De-initialize the time
 * reactivate the old saved vector
 */
static void vdi_Timer_finalize(void) {
    WORD old_sr = set_sr(0x2700); /* disable interrupts */
    Setexc(0x100, (long)lineaVars.timer_chain); /* set etv_timer to vdi_Timer_handleInterrupt */
    set_sr_only(old_sr); /* enable interrupts */
}

//--------------------------------------------------------------------------------
// Keys.
//--------------------------------------------------------------------------------
/*
 * Get shift state
 * returns: CTL/SHIFT/ALT status
 */
static WORD vdi_Input_setShiftState(void) {
    return Kbshift(-1) & 0x000f;
}

/*
 * Get choice for choice input.
 * returns:   0    nothing happened
 *            1    choice value
 *            2    button pressed
 */
static WORD vdi_Input_getChoiceKey(void) {
    lineaVars.keyboard_pressedKey = 1; /* 16 bit char info */
    return lineaVars.keyboard_pressedKey;
}

/*
 * Get char for string input
 * returns:  1     button pressed
 *           0     nothing happened
 * lineaVars.keyboard_pressedKey         16 bit char info
 */
static WORD vdi_Input_getCharKey(void) {
    if (Bconstat(2)) { /* see if a character present at con */
        ULONG ch = Bconin(2);
        lineaVars.keyboard_pressedKey = (WORD)
            (ch >> 8)| /* scancode down to bit 8-15 */
            (ch & 0xff); /* asciicode to bit 0-7 */
        return 1;
    }
    return 0;
}

/*
 * Implements vrq_choice()/vsm_choice()
 * These functions return the status of the logical 'choice' device.
 * The "GEM Programmer's Guide: VDI" indicates that these functions
 * are not required, and both Atari TOS and EmuTOS (using the original
 * imported DRI source) implement them as dummy functions.
 */
static void vdi_v_choice(vdi_VirtualWorkstation * vwk) {
    vdi_Input_getChoiceKey();
    lineaVars.INTOUT[0] = lineaVars.keyboard_pressedKey & 0x00ff;
}

/*
 * Implements vrq_string()/vsm_string()
 *
 * These functions return the status of the logical 'string' device,
 * which is the keyboard under TOS.
 *
 * vrq_string() operation in Atari TOS and EmuTOS
 * ----------------------------------------------
 * 1. This function reads characters from the keyboard until a carriage
 *    return is entered, or until the maximum number of characters has
 *    been read, and then returns.  The characters are returned in
 *    intout[]: each word in intout[] will contain zero in the high-order
 *    byte, and the ASCII character in the low-order byte.  The 'C'
 *    binding will copy the low-order bytes to a buffer.  If the call is
 *    terminated by a carriage return, the carriage return is NOT placed
 *    in intout[].
 * 2. The maximum number of characters may be specified as negative.  In
 *    this case, the maximum used will be the absolute value of that
 *    specified, and everything else will work the same as (1) above,
 *    except that the words in the intout[] array will contain extended
 *    keyboard codes: the scancode in the high-order byte and the ASCII
 *    code in the low-order byte.
 * 3. The 'echo' argument is ignored.
 * 4. Atari TOS bug: when the maximum is specified as negative, carriage
 *    returns do NOT terminate input; input is only terminated by the
 *    maximum number of characters being reached.
 *
 * vsm_string() operation in Atari TOS and EmuTOS
 * ----------------------------------------------
 * 1. On entry, this function checks if any keyboard input is pending;
 *    if not, it returns immediately.  Otherwise, it reads characters
 *    until there are no more, or until the maximum number of characters
 *    has been read.
 *    NOTE: carriage returns are treated like any other character, are
 *    included in intout[], and do NOT cause input termination.
 * 2. The maximum number of characters may be specified as negative, with
 *    the same results as described above for vrq_string().
 * 3. The 'echo' argument is ignored.
 */
static void vdi_v_string(vdi_VirtualWorkstation * vwk) {
    WORD mask = 0x00ff;
    WORD j = lineaVars.INTIN[0];
    if (j < 0) {
        j = -j;
        mask = 0xffff;
    }
    if (!lineaVars.input_stringMode) { /* Request mode */
        lineaVars.keyboard_pressedKey = 0;
        WORD i;
        for (i = 0; (i < j) && ((lineaVars.keyboard_pressedKey & 0x00ff) != 0x000d); i++) {
            while (vdi_Input_getCharKey() == 0) ;
            lineaVars.INTOUT[i] = lineaVars.keyboard_pressedKey = lineaVars.keyboard_pressedKey & mask;
        }
        if ((lineaVars.keyboard_pressedKey & 0x00ff) == 0x000d)
            --i;
        lineaVars.parameters.contrl->outputIntNb = i;
    } else { /* Sample mode */
        WORD i = 0;
        while ((vdi_Input_getCharKey() != 0) && (i < j))
            lineaVars.INTOUT[i++] = lineaVars.keyboard_pressedKey & mask;
        lineaVars.parameters.contrl->outputIntNb = i;
    }
}

/* Return Shift, Control, Alt State */
static void vdi_vq_key_s(vdi_VirtualWorkstation * vwk) {
    lineaVars.INTOUT[0] = vdi_Input_setShiftState();
}

/* vdi_Function_setInputMode: */
static void vdi_vsin_mode(vdi_VirtualWorkstation * vwk) {
    WORD i = lineaVars.INTIN[1];
    lineaVars.INTOUT[0] = i;
    i--;
    switch (lineaVars.INTIN[0]) {
    default: break;
    case 1: lineaVars.keyboard_locatorMode = i; break; /* locator */
    case 2: lineaVars.input_valuatorMode = i; break; /* valuator */
    case 3: lineaVars.keyboard_inputModeChoice = i; break; /* choice */
    case 4: lineaVars.input_stringMode = i; break; /* string */
    }
}

/*
 * INQUIRE INPUT MODE: implements vqin_mode()
 * This function is documented by Atari to return the mode value set by vsin_mode() [this is either 1 (request mode) or 2 (sample mode)].
 * However, like all versions of Atari TOS, it actually returns the mode value minus 1 (i.e. 0 or 1).
 */
static void vdi_vqin_mode(vdi_VirtualWorkstation * vwk) {
    WORD v;
    switch (lineaVars.INTIN[0]) {
    default: v = 0; break;
    case 1: v = lineaVars.keyboard_locatorMode; break; /* locator */
    case 2: v = lineaVars.input_valuatorMode; break; /* valuator */
    case 3: v = lineaVars.keyboard_inputModeChoice; break; /* choice */
    case 4: v = lineaVars.input_stringMode; break; /* string */
    }
    WORD *int_out = lineaVars.INTOUT;
    *int_out = v;
}

//********************************************************************************
// Mouse.
//********************************************************************************
//--------------------------------------------------------------------------------
// Mouse buttons, position and wheels.
//--------------------------------------------------------------------------------
typedef void (*vdi_Mouse_Handler)(void);

void vdi_Mouse_handlePositionDefaultAsm(void);
void vdi_Mouse_handlePositionDefault(Point *point);
void vdi_Mouse_handlePositionDefault(Point *point) {
    if (lineaVars.mouse_hiddenCount != 0)
        return;
    WORD srOld = set_sr(0x2700); // Disable interrupts.
    lineaVars.mouse_newX = point->x;
    lineaVars.mouse_newY = point->y;
    lineaVars.mouse_drawRequested |= 1;
    set_sr_only(srOld);
}

forceinline WORD vdi_Mouse_handleUserButton(vdi_Mouse_Handler handler, WORD state) {
    register WORD d0 asm("d0") = state;
    register vdi_Mouse_Handler a0 asm("a0") = handler;
    asm volatile (
        "jsr (a0)\n\t"
        : "+d" (d0), "+a" (a0)
        : 
        : "d1", "a1"
    );
    return d0;
}

forceinline void vdi_Mouse_handleUserPosition(vdi_Mouse_Handler handler, Point *point) {
    register WORD d0 asm("d0") = point->x;
    register WORD d1 asm("d1") = point->y;
    register vdi_Mouse_Handler a0 asm("a0") = handler;
    asm volatile (
        "jsr (a0)\n\t"
        : "+d" (d0), "+d" (d1), "+a" (a0)
        : 
        : "a1"
    );
    point->x = d0;
    point->y = d1;
}

static void vdi_Mouse_clip(Point *point) {
    point->x = clipWord(point->x, 0, vdi_deviceResolutionX);
    point->y = clipWord(point->y, 0, vdi_deviceResolutionY);
}

void vdi_Mouse_handleInterruptAsm(void);
void vdi_Mouse_handleInterrupt(const SBYTE *packet);
void vdi_Mouse_handleInterrupt(const SBYTE *packet) {
    UBYTE state = (UBYTE)packet[0];
    if ((state & 0xf8) != 0xf8)
        return;

    UBYTE buttonNew = (((state & 1) != 0) << 1) | ((state & 2) != 0); // Swap buttons from the packet.
    UBYTE buttonOld = lineaVars.mouse_currentStatus & 0x03; // Keep only left and right buttons.
    if (buttonNew != buttonOld) {
        UWORD buttonState = (lineaVars.mouse_buttonState & ~0x03) | buttonNew; // Merge with other buttons.
        lineaVars.mouse_buttonState = buttonState;
        buttonState = vdi_Mouse_handleUserButton(lineaVars.mouse_handleUserButton, buttonState);
        buttonState &= 0x03;
        lineaVars.mouse_currentStatus = buttonState | ((buttonState ^ buttonOld) << 6);
    }
    
    SBYTE xDelta = packet[1], yDelta = packet[2];
    if (xDelta | yDelta) {
        lineaVars.mouse_currentStatus |= (1 << 5);
        Point pNew;
        pNew.x = lineaVars.mouse_positionX + xDelta; pNew.y = lineaVars.mouse_positionY + yDelta;
        vdi_Mouse_clip(&pNew);
        vdi_Mouse_handleUserPosition(lineaVars.mouse_handleUserMotion, &pNew);
        vdi_Mouse_clip(&pNew);
        lineaVars.mouse_positionX = pNew.x; lineaVars.mouse_positionY = pNew.y;
        vdi_Mouse_handleUserPosition(lineaVars.mouse_handleUserCursor, &pNew);
    } else
        lineaVars.mouse_currentStatus &= ~(1 << 5);
}

static void vdi_vq_mouse(vdi_VirtualWorkstation * vwk) {
    WORD old_sr = set_sr(0x2700); /* disable interrupts */
    lineaVars.INTOUT[0] = lineaVars.mouse_buttonState;
    lineaVars.PTSOUT[0] = lineaVars.mouse_positionX;
    lineaVars.PTSOUT[1] = lineaVars.mouse_positionY;
    set_sr(old_sr); /* enable interrupts */
}

static void vdi_vex_butv(vdi_VirtualWorkstation * vwk) {
    ULONG_AT(&lineaVars.CONTRL[9]) = (ULONG) lineaVars.mouse_handleUserButton;
    lineaVars.mouse_handleUserButton = (PFVOID) ULONG_AT(&lineaVars.CONTRL[7]);
}

static void vdi_vex_motv(vdi_VirtualWorkstation * vwk) {
    ULONG_AT(&lineaVars.CONTRL[9]) = (ULONG) lineaVars.mouse_handleUserMotion;
    lineaVars.mouse_handleUserMotion = (PFVOID) ULONG_AT(&lineaVars.CONTRL[7]);
}

static void vdi_vex_curv(vdi_VirtualWorkstation * vwk) {
    ULONG_AT(&lineaVars.CONTRL[9]) = (ULONG) lineaVars.mouse_handleUserCursor;
    lineaVars.mouse_handleUserCursor = (PFVOID) ULONG_AT(&lineaVars.CONTRL[7]);
}

#if CONF_WITH_EXTENDED_MOUSE

void vdi_Mouse_handleWheelInterruptAsm(void);

forceinline void vdi_Mouse_handleUserWheel(vdi_Mouse_Handler handler, WORD wheelNumber, WORD wheelDelta) {
    register WORD d0 asm("d0") = wheelNumber;
    register WORD d1 asm("d1") = wheelDelta;
    register vdi_Mouse_Handler a0 asm("a0") = handler;
    asm volatile (
        "jsr (a0)\n\t"
        : "+d" (d0), "+d" (d1), "+a" (a0)
        : 
        : "a1"
    );
}

typedef void (*IkbdHandler)(void);
IkbdHandler vdi_Mouse_handleWheelInterrupt(const SBYTE *packet);
IkbdHandler vdi_Mouse_handleWheelInterrupt(const SBYTE *packet) {
    static const SBYTE wheelIkbdPacketHeader[] = { 0x05, 0x00, 0x00, 0x00, 0x00, 0x00 };
    const SBYTE *header = wheelIkbdPacketHeader;
    for (WORD i = 0; i < sizeof(wheelIkbdPacketHeader); i++)
        if (*packet++ != *header++)
            goto on_exit;
    WORD status = (UBYTE)*packet++ - 0x59;
    if (status < 0 || status > 3)
        goto on_exit;
    WORD wheelNumber = status >> 1;
    WORD wheelDelta = ((status & 1) << 1) - 1;
    vdi_Mouse_handleUserWheel(vdi_context.mouse.userWheelHandler, wheelNumber, wheelDelta);           
on_exit:
    return vdi_context.mouse.savedIkbdStatusRoutine;
}

// Handle additional mouse buttons
static void vdi_mousex_handler(WORD scancode) {
    WORD buttonStateOld = lineaVars.mouse_buttonState, buttonStateNew = buttonStateOld;
    switch (scancode) {
    case 0x37: buttonStateNew |= 0x04; break; // Mouse button 3 press
    case 0xb7: buttonStateNew &= ~0x04; break; // Mouse button 3 release
    case 0x5e: buttonStateNew |= 0x08; break; // Mouse button 4 press
    case 0xde: buttonStateNew &= ~0x08; break; // Mouse button 4 release
    case 0x5f: buttonStateNew |= 0x10; break; // Mouse button 5 press
    case 0xdf: buttonStateNew &= ~0x10; break; // Mouse button 5 release
    }
    lineaVars.mouse_buttonState = buttonStateNew;
    if (buttonStateNew != buttonStateOld)
        vdi_Mouse_handleUserButton(lineaVars.mouse_handleUserButton, lineaVars.mouse_buttonState);

    switch (scancode) {
    case 0x59: vdi_Mouse_handleUserWheel(vdi_context.mouse.userWheelHandler, 0, -1); break; // Wheel up
    case 0x5a: vdi_Mouse_handleUserWheel(vdi_context.mouse.userWheelHandler, 0, 1); break; // Wheel down
    case 0x5c: vdi_Mouse_handleUserWheel(vdi_context.mouse.userWheelHandler, 1, -1); break; // Wheel left
    case 0x5d: vdi_Mouse_handleUserWheel(vdi_context.mouse.userWheelHandler, 1, 1); break; // Wheel right
    }
}

static void vdi_vex_wheelv(vdi_VirtualWorkstation * vwk) {
    ULONG_AT(&lineaVars.CONTRL[9]) = (ULONG) vdi_context.mouse.userWheelHandler;
    vdi_context.mouse.userWheelHandler = (PFVOID) ULONG_AT(&lineaVars.CONTRL[7]);
}

#endif /* CONF_WITH_EXTENDED_MOUSE */

//--------------------------------------------------------------------------------
// Mouse cursor drawing.
//--------------------------------------------------------------------------------
#define vdi_mouseSizeX 16
#define vdi_mouseSizeY 16

/*
 * Blits a "cursor" to the destination.
 * Before the destination is overwritten, the current contents are saved to the user-provided save area (vdi_MouseCursorSaveArea).
 * Then the cursor is  written, combining a background colour form, a foreground colour form, and the current contents of the destination.
 * Some points to note:
 * - The cursor is always 16x16 pixels. In the general case, it will overlap two adjacent screen words in each plane.
 *   Thus the save area requires 4 bytes per plane for each row of the cursor, or 64 bytes in total per plane (plus some bookkeeping overhead).
 * - If the cursor is subject to left or right clipping, however, then it must lie within one screen word (per plane), so we only save 32 bytes/plane.
 * Called from vdi_asm.S.
 */
void vdi_Mouse_draw(vdi_MouseSprite *sprite, vdi_MouseCursorSaveArea *mcs, WORD x, WORD y);
void vdi_Mouse_draw(vdi_MouseSprite *sprite, vdi_MouseCursorSaveArea *mcs, WORD x, WORD y) {
    x -= sprite->xhot; // x = left side of destination block.
    y -= sprite->yhot; // y = top of destination block.

    mcs->stat = 0x00; // Reset status of save buffer.

    // clip x axis
    WORD clipState;
    WORD w;
    if (x < 0) {
        // Clip left.
        w = x + vdi_mouseSizeX;
        if (w <= 0)
            return;
        x += vdi_mouseSizeX; // Address of right word.
        clipState = -1; // Left clipping.
    } else if (x > (vdi_deviceResolutionX + 1 - vdi_mouseSizeX)) {
        // Clip right.
        w = vdi_deviceResolutionX + 1 - x;
        if (w <= 0)
            return;
        clipState = +1;
    } else {
        // No clipping.
        w = vdi_mouseSizeX;
        clipState = 0;
        mcs->stat |= vdi_MouseCursorSaveAreaFlag_longUsed; // Mark savearea as longword save.
    }

    // clip y axis
    WORD h;
    UWORD *src = sprite->maskdata; /* MASK/DATA for cursor */
    if (y < 0) {
        // Clip top.
        h = y + vdi_mouseSizeY;
        if (h <= 0)
            return;
        src -= y << 1; // Point to first visible row of MASK/FORM.
        y = 0;
    } else if (y > (vdi_deviceResolutionY + 1 - vdi_mouseSizeY)) {
        // Clip bottom.
        h = vdi_deviceResolutionY + 1 - y;
        if (h <= 0)
            return;
    } else
        h = vdi_mouseSizeY;
    mcs->len = h;

    UWORD *dst = vdi_getPixelAddress(x, y);
    mcs->addr = dst;
    mcs->stat |= vdi_MouseCursorSaveAreaFlag_valid; // Flag the buffer as being loaded.
    WORD dstStride = lineaVars.screen_lineSize2;
 
    UWORD colorBg = sprite->bg_col, colorFg = sprite->fg_col;

    UWORD shft = 16 - (x & 0x0f); /* amount to shift forms by */
    WORD planeNb = lineaVars.screen_planeNb;

    if (clipState) {
        // Handles cursor display for cursors that are subject to L/R clipping
        UWORD *save = (UWORD *)mcs->area; /* we save words, not longwords */
        LOOP_DO(planeIndex, planeNb) {
            UWORD *s = src, *d = dst;
            LOOP_DO(y0, h) {
                // Read destination.
                ULONG dstWord = *d;
                *save++ = (UWORD)dstWord; // Save the existing data.
                if (clipState > 0)
                    dstWord <<= 16;
                
                // Align the forms with the cursor position on the screen.
                
                // Select operation for mouse mask background color.
                ULONG srcBg = (ULONG)*s++ << shft; // Background color.
                if (colorBg & 1)
                    dstWord |= srcBg;
                else
                    dstWord &= ~srcBg;

                // Select operation for mouse mask foreground color.
                ULONG srcFg = (ULONG)*s++ << shft; // Foreground color.
                if (colorFg & 1)
                    dstWord |= srcFg;
                else
                    dstWord &= ~srcFg;

                // Write destination.
                if (clipState > 0)
                    dstWord >>= 16;
                *d = dstWord;
                d = (UWORD*)((intptr_t)d + dstStride);
            } LOOP_WHILE (y0);
            dst++; 
            colorBg >>= 1;
            colorFg >>= 1;
        } LOOP_WHILE(planeIndex);
    } else {
        // The rest of this function handles the no-L/R clipping case
        ULONG *save = mcs->area; // For long stores.
        LOOP_DO(planeIndex, planeNb) {
            /* setup the things we need for each plane again */
            UWORD *s = src, *d0 = dst, *d1 = dst + planeNb;
            LOOP_DO(y0, h) {
                ULONG dstWord = ((ULONG)*d0 << 16) | *d1;
                *save++ = dstWord; // Save the existing data.

                // Align the forms with the cursor position on the screen.

                // Select operation for mouse mask background color.
                ULONG srcBg = (ULONG)*s++ << shft; // Background color.
                if (colorBg & 1)
                    dstWord |= srcBg;
                else
                    dstWord &= ~srcBg;

                // Select operation for mouse mask foreground color.
                ULONG srcFg = (ULONG)*s++ << shft; // Foreground color.
                if (colorFg & 1)
                    dstWord |= srcFg;
                else
                    dstWord &= ~srcFg;

                // Write destination.
                *d0 = (UWORD)(dstWord >> 16);
                *d1 = (UWORD)dstWord;
                d0 = (UWORD*)((intptr_t)d0 + dstStride);
                d1 = (UWORD*)((intptr_t)d1 + dstStride);
            } LOOP_WHILE (y0);
            dst++;
            colorBg >>= 1;
            colorFg >>= 1;
        } LOOP_WHILE(planeIndex);
    }
}

/*
 * Replace cursor with data in save area.
 * Called from vdi_asm.S.
 */
void vdi_Mouse_clear(vdi_MouseCursorSaveArea *mcs);
void vdi_Mouse_clear(vdi_MouseCursorSaveArea * RESTRICT mcs) {
    UBYTE stat = mcs->stat;
    if (!(stat & vdi_MouseCursorSaveAreaFlag_valid))
        return;
    mcs->stat = stat & ~vdi_MouseCursorSaveAreaFlag_valid;
    UWORD * RESTRICT src = (UWORD *)mcs->area, * RESTRICT dst = mcs->addr;
    WORD dstStride = lineaVars.screen_lineSize2;
    WORD planeNb = lineaVars.screen_planeNb;
    WORD h = mcs->len;
//    if (h < 0) return;
    if (stat & vdi_MouseCursorSaveAreaFlag_longUsed) {
        // Handle long word data.
        #if vdi_Soft_asmEnabled
        UWORD *dst0 = dst, *dst1 = dst + planeNb, *d0, *d1;
        WORD y;
        __asm__ volatile (
            "subq.w #1,%0\n\t"
            "subq.w #1,%1\n\t"
            "1:\n\t"
            "move.l %6,%4\n\t"
            "move.l %7,%5\n\t"
            "move.w %1,%2\n\t"
            "2:\n\t"
            "move.w (%3)+,(%4)\n\t"
            "move.w (%3)+,(%5)\n\t"
            "adda.w %8,%4\n\t"
            "adda.w %8,%5\n\t"
            "dbf %2,2b\n\t"
            "addq.l #2,%6\n\t"
            "addq.l #2,%7\n\t"
            "dbf %0,1b\n\t"
            : "+d"(planeNb), "+r"(h), "=&d"(y), "+a"(src), "=&a"(d0), "=&a"(d1)
            : "r"(dst0), "r"(dst1), "r"(dstStride)
            : "cc"
        );
        #else
        LOOP_DO(planeIndex, planeNb) {
            UWORD * RESTRICT d0 = dst, * RESTRICT d1 = dst + planeNb;
            LOOP_DO(y0, h) {
                *d0 = *src++;
                *d1 = *src++;
                d0 = (UWORD*)((intptr_t)d0 + dstStride);
                d1 = (UWORD*)((intptr_t)d1 + dstStride);
            } LOOP_WHILE (y0);
            dst++;
        } LOOP_WHILE(planeIndex);
        #endif
    } else {
        // Handle word data.
        #if vdi_Soft_asmEnabled
        UWORD *d;
        WORD y;
        __asm__ volatile (
            "subq.w #1,%0\n\t"
            "subq.w #1,%1\n\t"
            "1:\n\t"
            "move.l %5,%4\n\t"
            "move.w %1,%2\n\t"
            "2:\n\t"
            "move.w (%3)+,(%4)\n\t"
            "adda.w %6,%4\n\t"
            "dbf %2,2b\n\t"
            "addq.l #2,%5\n\t"
            "dbf %0,1b\n\t"
            : "+d"(planeNb), "+r"(h), "=&d"(y), "+a"(src), "=&a"(d)
            : "r"(dst), "r"(dstStride)
            : "cc"
        );
        #else
        LOOP_DO(planeIndex, planeNb) {
            UWORD * RESTRICT d = dst;
            LOOP_DO(y0, h) {
                *d = *src++;
                d = (UWORD*)((intptr_t)d + dstStride);
            } LOOP_WHILE (y0);
            dst++;
        } LOOP_WHILE(planeIndex);
        #endif
    }
}

/*
 * Displays the mouse cursor if the number of hide operations has gone back to 0.
 * Decrement the counter for the number of hide operations performed.
 * If this is not the last one then do nothing because the cursor
 * should remain hidden.
 *   Outputs:
 *      hide_cnt = hide_cnt - 1
 *      lineaVars.mouse_drawRequested = 0
 */

static void vdi_Mouse_show(void) {
    if (lineaVars.mouse_hiddenCount != 1) {
        /* if not about to be shown: */
        lineaVars.mouse_hiddenCount--; /* just decrement hide count */
        if (lineaVars.mouse_hiddenCount < 0) /* but make sure it doesn't go negative! */
            lineaVars.mouse_hiddenCount = 0;
    } else {
        /* lineaVars.mouse_hiddenCount is precisely 1 at this point */
        vdi_Mouse_draw(&lineaVars.mouse_data, vdi_context.mouse.saveArea, lineaVars.mouse_positionX, lineaVars.mouse_positionY);  /* display the cursor */
        lineaVars.mouse_drawRequested = 0; /* disable VBL drawing routine */
        lineaVars.mouse_hiddenCount--;
    }
}

/*
 * This routine hides the mouse cursor if it has not already
 * been hidden.
 * Inputs:         None
 * Outputs:
 *    hide_cnt = hide_cnt + 1
 *    lineaVars.mouse_drawRequested = 0
 */
static void vdi_Mouse_hide(void) {
    /*
     * Increment the counter for the number of hide operations performed.
     * If this is the first one then remove the cursor from the screen.
     * If not then do nothing, because the cursor wasn't on the screen.
     */
    lineaVars.mouse_hiddenCount += 1;              /* increment it */
    if (lineaVars.mouse_hiddenCount == 1) {        /* if cursor was not hidden... */
        vdi_Mouse_clear(vdi_context.mouse.saveArea);   /* remove the cursor from screen */
        lineaVars.mouse_drawRequested = 0;          /* disable VBL drawing routine */
    }
}

static void vdi_v_show_c(vdi_VirtualWorkstation * vwk) {
    if (!lineaVars.INTIN[0] && lineaVars.mouse_hiddenCount)
        lineaVars.mouse_hiddenCount = 1;           /* reset cursor to on */
    vdi_Mouse_show();
}

static void vdi_v_hide_c(vdi_VirtualWorkstation * vwk) {
    vdi_Mouse_hide();
}

// Copies src mouse form to dst mouse sprite, constrains hotspot position & colors and maps colors.
static void vdi_Mouse_setForm(const MFORM *src, vdi_MouseSprite *dst) {
    lineaVars.mouse_modifyingCount += 1; // Disable updates while redefining cursor.

    /* save x-offset of mouse hot spot */
    dst->xhot = src->mf_xhot & 0x000f;

    /* save y-offset of mouse hot spot */
    dst->yhot = src->mf_yhot & 0x000f;

    /* check/fix background color index */
    WORD col = vdi_Color_validateIndex(src->mf_bg);
    dst->bg_col = vdi_context.palette.penToPaletteTable[col];

    /* check/fix foreground color index */
    col = vdi_Color_validateIndex(src->mf_fg);
    dst->fg_col = vdi_context.palette.penToPaletteTable[col];

    /*
     * Move the new mouse definition into the global mouse cursor definition
     * table.  The values for the mouse mask and data are supplied as two
     * separate 16-word entities.  They must be stored as a single array
     * starting with the first word of the mask followed by the first word
     * of the data and so on.
     */

    /* copy the data to the global mouse definition table */
    UWORD * gmdt = dst->maskdata;                /* global mouse definition table */
    const UWORD *mask = src->mf_mask;
    const UWORD *data = src->mf_data;
    for (int i = 15; i >= 0; i--) {
        *gmdt++ = *mask++;              /* get next word of mask */
        *gmdt++ = *data++;              /* get next word of data */
    }

    lineaVars.mouse_modifyingCount -= 1;                    /* re-enable mouse drawing */
}

static void vdi_vsc_form(vdi_VirtualWorkstation * vwk) {
    vdi_Mouse_setForm((const MFORM *)lineaVars.INTIN, &lineaVars.mouse_data);
}

/*
 * Moves mouse cursor, GEM VBL routine
 * It removes the mouse cursor from its current location, if necessary,
 * and redraws it at a new location.
 *
 *      Inputs:
 *         lineaVars.mouse_drawRequested - signals need to redraw cursor
 *         lineaVars.mouse_newX - new cursor x-coordinate
 *         lineaVars.mouse_newY - new cursor y-coordinate
 *         lineaVars.mouse_modifyingCount - mouse cursor is being modified
 *         lineaVars.mouse_hiddenCount - mouse cursor hide/show indicator
 *
 *      Outputs:
 *         lineaVars.mouse_drawRequested is cleared
 */
/* If we do not need to draw the cursor now then just exit. */
static void vdi_Mouse_handleAtVbl(void) {
    /* if the cursor is being modified, or is hidden, just exit */
    if (lineaVars.mouse_modifyingCount || lineaVars.mouse_hiddenCount)
        return;
    WORD old_sr = set_sr(0x2700); // disable interrupts
    if (lineaVars.mouse_drawRequested) {
        lineaVars.mouse_drawRequested = false;
        WORD x = lineaVars.mouse_newX; // Get x/y for cur_display() atomically */
        WORD y = lineaVars.mouse_newY;
        set_sr_only(old_sr);
        vdi_Mouse_clear(vdi_context.mouse.saveArea); /* remove the old cursor from the screen */
        vdi_Mouse_draw(&lineaVars.mouse_data, vdi_context.mouse.saveArea, x, y); /* display the cursor */
    } else
        set_sr_only(old_sr);
}

//--------------------------------------------------------------------------------
// Locator.
//--------------------------------------------------------------------------------
/*
 * Get locator key
 *
 * returns:  0    - nothing
 *           1    - button pressed
 *                  lineaVars.keyboard_pressedKey = 16 bit char info
 *           2    - coordinate info
 *                     X1 = new x
 *                     Y1 = new y
 *
 * The variable lineaVars.mouse_currentStatus holds the bitmap of mouse status since the last
 * interrupt. The bits are
 *
 * 0 - 0x01 Left mouse button status  (0=up)
 * 1 - 0x02 Right mouse button status (0=up)
 * 2 - 0x04 Reserved
 * 3 - 0x08 Reserved
 * 4 - 0x10 Reserved
 * 5 - 0x20 Mouse move flag (1=moved)
 * 6 - 0x40 Right mouse button status flag (0=hasn't changed)
 * 7 - 0x80 Left mouse button status flag  (0=hasn't changed)
 */
static WORD vdi_Mouse_getLocatorKey(void) {
    WORD retval = 0;

    /*
     * check for mouse button or keyboard key
     */
    if (lineaVars.mouse_currentStatus & 0xc0) {   /* some button status bits set? */
        if (lineaVars.mouse_currentStatus & 0x40) /* if bit 6 set,                     */
            lineaVars.keyboard_pressedKey = 0x20; /* send terminator code for left key */
        else
            lineaVars.keyboard_pressedKey = 0x21; /* send terminator code for right key */
        lineaVars.mouse_currentStatus &= 0x23; /* clear mouse button status (bit 6/7) */
        retval = 1; /* set button pressed flag */
    } else if (Bconstat(2)) { /* see if a character present at con */
        ULONG ch = Bconin(2);
        lineaVars.keyboard_pressedKey = (WORD)
                  (ch >> 8)| /* scancode down to bit 8-15 */
                  (ch & 0xff); /* asciicode to bit 0-7 */
        retval = 1; /* set button pressed flag */
    }

    /*
     * check for mouse movement
     */
    if (lineaVars.mouse_currentStatus & 0x20) { /* if bit #5 set ... */
        Point * point = (Point*)lineaVars.PTSIN;
        lineaVars.mouse_currentStatus &= ~0x20; /* clear bit 5 */
        point->x = lineaVars.mouse_positionX; /* set X = lineaVars.mouse_positionX */
        point->y = lineaVars.mouse_positionY; /* set Y = lineaVars.mouse_positionY */
        retval += 2;
    }

    return retval;
}

/*
 * vdi_Function_setLocatorInput: implements vrq_locator()/vsm_locator()
 *
 * These functions return the status of the logical 'locator' device.
 *
 * vrq_locator() operation in Atari TOS and EmuTOS
 * -----------------------------------------------
 * 1. The first call to vrq_locator() always returns immediately: the
 *    output mouse positions are the same as the input, and the
 *    terminating character is set to 0x20, indicating the left mouse
 *    button.
 * 2. Subsequent calls return when either a keyboard key is pressed, or
 *    a mouse button is pressed OR released (thus a normal mouse button
 *    action satisfies TWO calls to vrq_locator()).  The output mouse
 *    positions are the current positions, and the terminating character
 *    is the ASCII key pressed, or 0x20 for the left mouse button / 0x21
 *    for the right.
 *    As a consequence, pressing the space key twice is indistinguishable
 *    from pressing/releasing the left mouse button, and likewise for
 *    the exclamation mark and the right mouse button.
 *
 * vsm_locator() operation in Atari TOS and EmuTOS
 * -----------------------------------------------
 * 1. The first call to vsm_locator() always sets the terminating
 *    character to 0x20 and lineaVars.parameters.contrl->outputIntNb to 1 (indicating the left mouse
 *    button).
 * 2. On every call:
 *    . if the mouse has been moved, lineaVars.parameters.contrl->outputVertexNb is set to 1
 *    . if a keyboard key is pressed, the terminating character is the
 *      ASCII value of the key pressed, and lineaVars.parameters.contrl->outputIntNb is set to 1
 *    . if a mouse button is pressed or released, the terminating
 *      character is 0x20 for the left button, 0x21 for the right
 *      button, and lineaVars.parameters.contrl->outputIntNb is set to 1
 *    . the output mouse positions are always set to the same as the
 *      input
 *
 * Differences from official Atari documentation
 * ---------------------------------------------
 * 1. No special behaviour is described for the first call to
 *    vrq_locator() or vsm_locator().
 * 2. No mention is made of button press & release being separate
 *    events.
 * 3. For vsm_locator(), the output mouse positions should be the
 *    current positions, not the input positions.
 */
static void vdi_v_locator(vdi_VirtualWorkstation * vwk) {
    Point * point = (Point*)lineaVars.PTSIN;

    /* Set the initial locator position. */
    lineaVars.mouse_positionX = point->x;
    lineaVars.mouse_positionY = point->y;

    if (lineaVars.keyboard_locatorMode == 0) {    /* handle request mode (vrq_locator()) */
        vdi_Mouse_show();
        /* loop till button or keyboard event */
        while (!(vdi_Mouse_getLocatorKey() & 1)) ;
        lineaVars.INTOUT[0] = lineaVars.keyboard_pressedKey & 0x00ff;

        lineaVars.parameters.contrl->outputIntNb = 1;
        lineaVars.parameters.contrl->outputVertexNb = 1;

        lineaVars.PTSOUT[0] = point->x;
        lineaVars.PTSOUT[1] = point->y;
        vdi_Mouse_hide();
    } else {                /* handle sample mode (vsm_locator()) */
        WORD i = vdi_Mouse_getLocatorKey();
        if (i & 1) {
            lineaVars.parameters.contrl->outputIntNb = 1;
            lineaVars.INTOUT[0] = lineaVars.keyboard_pressedKey & 0x00ff;
        }
        if (i & 2) {
            lineaVars.parameters.contrl->outputVertexNb = 1;
            lineaVars.PTSOUT[0] = point->x;
            lineaVars.PTSOUT[1] = point->y;
        }
    }
}

//--------------------------------------------------------------------------------
// Mouse initialization.
//--------------------------------------------------------------------------------
#if !WITH_AES
/* Default Mouse Cursor Definition */
static const MFORM vdi_Mouse_arrow = {
    1, 0, 1, 0, 1,
    /* background definition */
    {
        0xE000, /* %1110000000000000 */
        0xF000, /* %1111000000000000 */
        0xF800, /* %1111100000000000 */
        0xFC00, /* %1111110000000000 */
        0xFE00, /* %1111111000000000 */
        0xFF00, /* %1111111100000000 */
        0xFF80, /* %1111111110000000 */
        0xFFC0, /* %1111111111000000 */
        0xFE00, /* %1111111000000000 */
        0xFE00, /* %1111111000000000 */
        0xEF00, /* %1110111100000000 */
        0x0F00, /* %0000111100000000 */
        0x0780, /* %0000011110000000 */
        0x0780, /* %0000011110000000 */
        0x03C0, /* %0000001111000000 */
        0x0000  /* %0000000000000000 */
    },
    /* foreground definition */
    {
        0x4000, /* %0100000000000000 */
        0x6000, /* %0110000000000000 */
        0x7000, /* %0111000000000000 */
        0x7800, /* %0111100000000000 */
        0x7C00, /* %0111110000000000 */
        0x7E00, /* %0111111000000000 */
        0x7F00, /* %0111111100000000 */
        0x7F80, /* %0111111110000000 */
        0x7C00, /* %0111110000000000 */
        0x6C00, /* %0110110000000000 */
        0x4600, /* %0100011000000000 */
        0x0600, /* %0000011000000000 */
        0x0300, /* %0000001100000000 */
        0x0300, /* %0000001100000000 */
        0x0180, /* %0000000110000000 */
        0x0000  /* %0000000000000000 */
    }
};
#define aes_Mouse_getDefaultForm() &vdi_Mouse_arrow
#endif

//void vdi_Mouse_move(void);

// Initializes the mouse (VDI part).
static void vdi_Mouse_initialize(void) {
    static const struct {
        UBYTE topmode;
        UBYTE buttons;
        UBYTE xparam;
        UBYTE yparam;
    } mouse_params = {0, 0, 1, 1};

    /* Input must be initialized here and not in init_wk */
    lineaVars.keyboard_locatorMode = 0; /* default is request mode  */
    lineaVars.input_valuatorMode = 0; /* default is request mode  */
    lineaVars.keyboard_inputModeChoice = 0; /* default is request mode  */
    lineaVars.input_stringMode = 0; /* default is request mode  */

    /* mouse settings */
    lineaVars.mouse_hiddenCount = 1;               /* mouse is initially hidden */
    lineaVars.mouse_positionX = vdi_deviceResolutionX / 2;           /* initialize the mouse to center */
    lineaVars.mouse_positionY = vdi_deviceResolutionY / 2;

    lineaVars.mouse_handleUserButton = just_rts;
    lineaVars.mouse_handleUserMotion = just_rts;
    lineaVars.mouse_handleUserCursor = vdi_Mouse_handlePositionDefaultAsm;
    #if CONF_WITH_EXTENDED_MOUSE
    vdi_context.mouse.userWheelHandler = just_rts;
    #endif

    /* Move in the default mouse form (presently the arrow) */
    // FIXME: Should not have a dependency to AES here !
    vdi_Mouse_setForm(aes_Mouse_getDefaultForm(), &lineaVars.mouse_data);

    lineaVars.mouse_buttonState = 0; /* clear the mouse button state */
    lineaVars.mouse_currentStatus = 0; /* clear the mouse status */
    lineaVars.mouse_modifyingCount = 0; /* clear the modifying count */
    lineaVars.mouse_drawRequested = 0; /* clear the hide operations counter */
    lineaVars.mouse_newX = 0; /* set cursor x-coordinate to 0 */
    lineaVars.mouse_newY = 0; /* set cursor y-coordinate to 0 */

    vblqueue[0] = vdi_Mouse_handleAtVbl;      /* set GEM VBL-routine to the first VBL slot */

    // Initialize mouse via XBIOS in relative mode.
    Initmous(1, (LONG)&mouse_params, (LONG)vdi_Mouse_handleInterruptAsm);

    #if CONF_WITH_EXTENDED_MOUSE
    {
        struct kbdvecs *kbd_vectors = (struct kbdvecs *)Kbdvbase();
        vdi_context.mouse.savedIkbdStatusRoutine = kbd_vectors->statvec;
        kbd_vectors->statvec = vdi_Mouse_handleWheelInterruptAsm;
        mousexvec = vdi_mousex_handler;
    }
    #endif
}

// Finalize and disable mouse.
static void vdi_Mouse_finalize(void) {
    lineaVars.mouse_handleUserButton = just_rts;
    lineaVars.mouse_handleUserMotion = just_rts;
    lineaVars.mouse_handleUserCursor = just_rts;
    #if CONF_WITH_EXTENDED_MOUSE
    vdi_context.mouse.userWheelHandler = just_rts;
    #endif

    vblqueue[0] = vdi_Mouse_handleAtVbl; // Set GEM VBL-routine to the first VBL slot.

    // Disable mouse via XBIOS.
    Initmous(0, 0, 0);

    #if CONF_WITH_EXTENDED_MOUSE
    {
        struct kbdvecs *kbd_vectors = (struct kbdvecs *)Kbdvbase();
        kbd_vectors->statvec = vdi_context.mouse.savedIkbdStatusRoutine;
    }
    #endif
}

//--------------------------------------------------------------------------------
// GSX escapes for the VDI screen driver
//--------------------------------------------------------------------------------
#define vdi_Escape_lastDri 19 /* last DRI escape = 19. */

/*
 * do console raw i/o
 */
static LONG vdi_Escape_crawio(WORD ch) {
    return Crawio(ch);
}

/*
 * write string to console
 * use BIOS, not GEMDOS, to be uninterruptible by Ctrl+C
 */
static void vdi_Escape_cconws(char *string) {
    while(*string)
        Bconout(2, *string++);
}

/*
 * Stub to simplify lookup table
 */
static void vdi_Escape_function0(vdi_VirtualWorkstation * vwk) {
}

/*
 * Returns the current number of rows and columns
 *
 * outputs:
 *   lineaVars.parameters.contrl->outputIntNb = 2 (# of integers returned)
 *   lineaVars.INTOUT[0] = number of rows
 *   lineaVars.INTOUT[1] = number of columns
 */
static void vdi_Escape_function1(vdi_VirtualWorkstation * vwk) {
    lineaVars.parameters.contrl->outputIntNb = 2;
    lineaVars.INTOUT[0] = lineaVars.font_cellRowNbMinus1 + 1;
    lineaVars.INTOUT[1] = lineaVars.font_cellColumnNbMinus1 + 1;
}

/*
 * Exit alpha mode and enter graphics mode
 *
 * note: if the last thing a program does before entering graphics mode
 * is to write an Esc to the console, the first character of the sequence
 * below will be eaten (and ignored).  concretely, this is most likely
 * to happen when a .TOS/.TTP program exits to the desktop.
 *
 * to circumvent this, we send a harmless escape sequence before we hide
 * the alpha cursor.
 */
static void vdi_Escape_function2(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033H\033f\033E");  /* home, hide alpha cursor, then clear-and-home */
}

/*
 * Enter alpha mode and exit graphics mode
 */
static void vdi_Escape_function3(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033E\033e\015"); /* clear-and-home, then show alpha cursor */
}

/*
 * Moves the alpha cursor up one line
 */
static void vdi_Escape_function4(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033A");
}

/*
 * Moves the alpha cursor down one line
 */
static void vdi_Escape_function5(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033B");
}

/*
 * Moves the alpha cursor right one column
 */
static void vdi_Escape_function6(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033C");
}

/*
 * Moves the alpha cursor left one column
 */
static void vdi_Escape_function7(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033D");
}

/*
 * Moves the alpha cursor home
 */
static void vdi_Escape_function8(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033H");
}

/*
 * Clears screen from cursor position to end of screen
 */
static void vdi_Escape_function9(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033J");
}

/*
 * Clears screen from cursor position to end of line
 */
static void vdi_Escape_function10(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033K");
}

/*
 * Sets the cursor position
 *
 * The cursor will be displayed at the new location,
 * if it is not currently hidden.
 *
 * inputs:
 *   lineaVars.INTIN[0] = cursor row (1 - max_y_cell)
 *   lineaVars.INTIN[1] = cursor column (1 - max_x_cell)
 */
static void vdi_Escape_function11(vdi_VirtualWorkstation * vwk) {
    char out[5];

    /* send string via VT-52 terminal emulation */
    out[0] = '\033';
    out[1] = 'Y';
    out[2] = 0x20 + lineaVars.INTIN[0] - 1;   /* zero-based */
    out[3] = 0x20 + lineaVars.INTIN[1] - 1;
    out[4] = '\0';
    vdi_Escape_cconws(out);
}

/*
 * Outputs cursor addressable alpha text
 *
 * The cursor will be displayed at the new location,
 * if it is not currently hidden.
 *
 * inputs:
 *   lineaVars.parameters.contrl->inputIntNb = character count
 *   lineaVars.INTIN = character array
 */
static void vdi_Escape_function12(vdi_VirtualWorkstation * vwk) {
    int cnt = lineaVars.parameters.contrl->inputIntNb;            /* get the character count */
    WORD *chr = lineaVars.INTIN;                /* address of the character array */
    while (cnt--)
        vdi_Escape_crawio(*chr++);         /* raw i/o to standard input/output */
}

/*
 * Switch to reverse video
 */
static void vdi_Escape_function13(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033p");        /* enter reverse video */
}

/*
 * Switch to normal video
 */
static void vdi_Escape_function14(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_cconws("\033q");        /* enter normal video */
}

/*
 * Returns current row and column of the alpha cursor
 */
static void vdi_Escape_function15(vdi_VirtualWorkstation * vwk) {
    lineaVars.parameters.contrl->outputIntNb = 2;              /* 2 integers are returned */
    lineaVars.INTOUT[0] = lineaVars.cursor_currentY + 1;   /* row (starting at 1) */
    lineaVars.INTOUT[1] = lineaVars.cursor_currentX + 1;   /* column (starting at 1) */
}

/*
 * Returns the availability of a "tablet device"
 *
 * Atari TOS always returns 1, so do we
 *
 * outputs:
 *   lineaVars.parameters.contrl->outputIntNb = 1  (# of parameters returned)
 *   lineaVars.INTOUT[0] = 1  (device is available)
 */
static void vdi_Escape_function16(vdi_VirtualWorkstation * vwk) {
    lineaVars.parameters.contrl->outputIntNb = 1;              /* 1 integer is returned */
    lineaVars.INTOUT[0] = 1;              /* there is a mouse */
}

/*
 * Output screen to printer
 *
 * we call the standard xbios screen dump
 */
static void vdi_Escape_function17(vdi_VirtualWorkstation * vwk) {
    Scrdmp();
}

/*
 * Display the graphics cursor
 *
 * note: although Atari documentation specifies that this call
 * is v_dspcur(handle,x,y), where x/y are the coordinates where
 * the cursor should be displayed, Atari TOS apparently ignores
 * these values, as do we ...
 */
static void vdi_Escape_function18(vdi_VirtualWorkstation * vwk) {
    lineaVars.INTIN[0] = 0;           /* show regardless */
    vdi_v_show_c(vwk);      /* display the graphics cursor */
}

/*
 * Remove the graphics cursor
 */
static void vdi_Escape_function19(vdi_VirtualWorkstation * vwk) {
    vdi_v_hide_c(vwk);      /* hide the graphics cursor */
}

/*
 * function lookup table
 */
static void (* const vdi_Escape_table[])(vdi_VirtualWorkstation *) =
{
    vdi_Escape_function0,
    vdi_Escape_function1,
    vdi_Escape_function2,
    vdi_Escape_function3,
    vdi_Escape_function4,
    vdi_Escape_function5,
    vdi_Escape_function6,
    vdi_Escape_function7,
    vdi_Escape_function8,
    vdi_Escape_function9,
    vdi_Escape_function10,
    vdi_Escape_function11,
    vdi_Escape_function12,
    vdi_Escape_function13,
    vdi_Escape_function14,
    vdi_Escape_function15,
    vdi_Escape_function16,
    vdi_Escape_function17,
    vdi_Escape_function18,
    vdi_Escape_function19
};

/*
 * This routine is called to decode the escape subfunctions
 *
 * The following inputs and outputs may be used by a subfunction:
 *
 * input:
 *   lineaVars.parameters.contrl->subOpcode = escape function ID.
 *   lineaVars.parameters.contrl->workstationHandle = device handle.
 *   lineaVars.INTIN[]   = array of input parameters.
 *
 * output:
 *   lineaVars.parameters.contrl->outputVertexNb = number of output vertices.
 *   lineaVars.parameters.contrl->outputIntNb = number of output parameters.
 *   lineaVars.INTOUT[]  = array of output parameters.
 *   lineaVars.PTSOUT[]  = array of output vertices.
 */
static void vdi_v_escape(vdi_VirtualWorkstation * vwk) {
    UWORD escfun = lineaVars.parameters.contrl->subOpcode;

    KDEBUG(("VDI esc, subfunction %u called\n",escfun));

#if vdi_Bezier_enabled
    if (escfun == 99) {
        v_bez_qual(vwk);        /* set quality of bezier curves */
        return;
    }
#endif

    if (escfun > vdi_Escape_lastDri)
        return;
    (*vdi_Escape_table[escfun])(vwk);
}

// Called by v_opnwk() to enter graphics mode.
static void vdi_Escape_initialize(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_function2(vwk);
}

// Called by v_clswk() to exit graphics mode.
static void vdi_Escape_finalize(vdi_VirtualWorkstation * vwk) {
    vdi_Escape_function3(vwk);
}

//--------------------------------------------------------------------------------
// Workstation.
//--------------------------------------------------------------------------------
static const linea_SizeTable vdi_Workstation_defaultSizeTable = {
    .charWidthMin = 0,
    .charHeightMin = 7,
    .charWidthMax = 0,
    .charHeightMax = 7,
    .lineWidthMin = 1,
    .reserved0 = 0,
    .lineWidthMax = vdi_Line_maxWidth,
    .reserved1 = 0,
    .markerWidthMin = 15,
    .markerHeightMin = 11,
    .markerWidthMax = 120,
    .markerHeightMax = 88
};

/* Here's the template lineaVars.workstation_inquireTable, see lineavars.S for the normal lineaVars.workstation_inquireTable */
static const WORD vdi_Workstation_defaultInquireTable[45] = {
    4,                  /* 0  - type of alpha/graphic controllers */
    1,                  /* 1  - number of background colors  */
    0x1F,               /* 2  - text styles supported        */
    0,                  /* 3  - scale rasters = false        */
    1,                  /* 4  - number of planes         */
    0,                  /* 5  - video lookup table       */
    50,                 /* 6  - performance factor????       */
    1,                  /* 7  - contour fill capability      */
    1,                  /* 8  - character rotation capability    */
    4,                  /* 9  - number of writing modes      */
    2,                  /* 10 - highest input mode       */
    1,                  /* 11 - text alignment flag      */
    0,                  /* 12 - Inking capability        */
    0,                  /* 13 - rubber banding           */
    CONF_VDI_MAX_VERTICES,    /* 14 - maximum vertices         */
    -1,                 /* 15 - maximum intin            */
    1,                  /* 16 - number of buttons on MOUSE   */
    0,                  /* 17 - styles for wide lines            */
    0,                  /* 18 - writing modes for wide lines     */
    0,                  /* 19 - filled in with clipping flag     */

    0,                  /* 20 - extended precision pixel size information */
    0,                  /* 21 - pixel width in 1/10, 1/100 or 1/1000 microns */
    0,                  /* 22 - pixel height in 1/10, 1/100 or 1/1000 microns */
    0,                  /* 23 - horizontal resolution in dpi */
    0,                  /* 24 - vertical resolution in dpi */
    0,                  /* 25 -  */
    0,                  /* 26 -  */
    0,                  /* 27 -  */
    0,                  /* 28 - bezier flag (bit 1) */
    0,                  /* 29 -  */
    0,                  /* 30 - raster flag (bit 0), does vro_cpyfm scaling? */
    0,                  /* 31 -  */
    0,                  /* 32 -  */
    0,                  /* 33 -  */
    0,                  /* 34 -  */
    0,                  /* 35 -  */
    0,                  /* 36 -  */
    0,                  /* 37 -  */
    0,                  /* 38 -  */
    0,                  /* 39 -  */
    0,                  /* 40 - unprintable left border in pixels (printers/plotters) */
    0,                  /* 41 - unprintable upper border in pixels (printers/plotters) */
    0,                  /* 42 - unprintable right border in pixels (printers/plotters) */
    0,                  /* 43 - unprintable lower border in pixels (printers/plotters) */
    0                   /* 44 - page size (printers etc.) */
};

/* Here's the template lineaVars.workstation_deviceTable, see lineavars.S for the normal lineaVars.workstation_deviceTable! */
static const WORD vdi_Workstation_defaultDeviceTable[45] = {
    639,                        /* 0    x resolution             */
    399,                        /* 1    y resolution             */
    0,                          /* 2    device precision 0=exact,1=not exact */
    372,                        /* 3    width of pixel           */
    372,                        /* 4    height of pixel          */
    3,                          /* 5    character sizes          */
    vdi_LineStyle_max,          /* 6    linestyles               */
    0,                          /* 7    linewidth                */
    6,                          /* 8    marker types             */
    8,                          /* 9    marker size              */
    1,                          /* 10   text font                */
    vdi_PatternStyle_max,       /* 11   area patterns             */
    vdi_HatchStyle_max,         /* 12   crosshatch patterns       */
    2,                          /* 13   colors at one time       */
    10,                         /* 14   number of GDP's          */
    1,                          /* 15   GDP bar                  */
    2,                          /* 16   GDP arc                  */
    3,                          /* 17   GDP pie                  */
    4,                          /* 18   GDP circle               */
    5,                          /* 19   GDP ellipse              */
    6,                          /* 20   GDP elliptical arc       */
    7,                          /* 21   GDP elliptical pie       */
    8,                          /* 22   GDP rounded rectangle    */
    9,                          /* 23   GDP filled rounded rectangle */
    10,                         /* 24   GDP #justified text      */
    3,                          /* 25   GDP #1                   */
    0,                          /* 26   GDP #2                   */
    3,                          /* 27   GDP #3                   */
    3,                          /* 28   GDP #4                   */
    3,                          /* 29   GDP #5                   */
    0,                          /* 30   GDP #6                   */
    3,                          /* 31   GDP #7                   */
    0,                          /* 32   GDP #8                   */
    3,                          /* 33   GDP #9                   */
    2,                          /* 34   GDP #10                  */
    0,                          /* 35   Color capability         */
    1,                          /* 36   Text Rotation            */
    1,                          /* 37   Polygonfill              */
    0,                          /* 38   Cell Array               */
    2,                          /* 39   Palette size             */
    2,                          /* 40   # of locator devices 1 = mouse */
    1,                          /* 41   # of valuator devices    */
    1,                          /* 42   # of choice devices      */
    1,                          /* 43   # of string devices      */
    2                           /* 44   Workstation Type 2 = out/in */
};

// Sets vdi_context.planeNbShift from the current value of lineaVars.screen_planeNb
void vdi_setScreenShift(void) {
    vdi_context.planeNbShift = vdi_planeNbToRightShift[lineaVars.screen_planeNb];
}

/*
 * update resolution-dependent VDI/lineA variables
 *
 * this function assumes that linea_Screen_planeNb, linea_Screen_width, linea_Screen_height are already set
 */
static void vdi_updateResolutionDependent(void) {
    WORD planeNb = lineaVars.screen_planeNb;

    lineaVars.screen_lineSize2 = muls((lineaVars.screen_width >> 3), planeNb); /* bytes per line */
    lineaVars.screen_lineSize = lineaVars.screen_lineSize2; /* I think lineaVars.screen_lineSize = lineaVars.screen_lineSize2 (PES) */

#if VDI_EXTENDED_PALETTE
    vdi_context.mouse.saveArea = (planeNb <= 4) ? &lineaVars.mouse_cursorSaveArea : &lineaVars.mouse_cursorSaveAreaExt;
#else
    vdi_context.mouse.saveArea = &lineaVars.mouse_cursorSaveArea;
#endif

    lineaVars.workstation_deviceTable[0] = lineaVars.screen_width - 1;
    lineaVars.workstation_deviceTable[1] = lineaVars.screen_height - 1;

    vdi_context.clippingRectFull.xMin = 0;
    vdi_context.clippingRectFull.yMin = 0;
    vdi_context.clippingRectFull.xMax = vdi_deviceResolutionX;
    vdi_context.clippingRectFull.yMax = vdi_deviceResolutionY;

    get_pixel_size(&lineaVars.workstation_deviceTable[3], &lineaVars.workstation_deviceTable[4]);
    
    lineaVars.workstation_deviceTable[13] = (planeNb<8) ? (1 << planeNb) : 256;
    lineaVars.workstation_deviceTable[35] = (planeNb==1) ? 0 : 1;
    lineaVars.workstation_deviceTable[39] = get_palette(); /* some versions of COLOR.CPX care about this */

    lineaVars.workstation_inquireTable[4] = planeNb;
    lineaVars.workstation_inquireTable[5] = (planeNb == 16 || get_monitor_type() == MON_MONO) ? 0 : 1; // Indicate whether LUT is supported.
}

static vdi_VirtualWorkstation * vdi_Workstation_getByHandle(WORD handle) {
    if (handle < vdi_firstHandle || handle > vdi_lastHandle)
        return NULL;
    return vdi_context.workstations[handle];
}

static void vdi_Workstation_initialize(vdi_VirtualWorkstation * vwk) {
    WORD *pointer = lineaVars.INTIN;
    pointer++;

    WORD l = *pointer++;             /* lineaVars.INTIN[1] */
    if (l > vdi_LineStyle_max || l < vdi_LineStyle_min)
        l = vdi_LineStyle_default;
    vwk->line_index = l - 1;

    l = vdi_Color_validateIndex(*pointer++);   /* lineaVars.INTIN[2] */
    vwk->line_color = vdi_context.palette.penToPaletteTable[l];

    l = *pointer++;             /* lineaVars.INTIN[3] */
    if (l > vdi_MarkerStyle_max || l < vdi_MarkerStyle_min)
        l = vdi_MarkerStyle_default;
    vwk->mark_index = l - 1;

    l = vdi_Color_validateIndex(*pointer++);   /* lineaVars.INTIN[4] */
    vwk->mark_color = vdi_context.palette.penToPaletteTable[l];

    /* You always get the default font */
    pointer++;                  /* lineaVars.INTIN[5] */

    l = vdi_Color_validateIndex(*pointer++);   /* lineaVars.INTIN[6] */
    vwk->text_color = vdi_context.palette.penToPaletteTable[l];

    vwk->mark_height = lineaVars.workstation_sizeTable.markerHeightMin;
    vwk->mark_scale = 1;

    l = *pointer++; /* lineaVars.INTIN[7] */
    vwk->fill_style = vdi_FillPattern_checkStyle(l);
    l = *pointer++; /* lineaVars.INTIN[8] */
    vwk->fill_index = vdi_FillPattern_checkPattern(vwk->fill_style, l) - 1;
    vdi_FillPattern_setUserData(vwk, 16, vdi_FillPattern_defaultUserDefined);
    vdi_FillPattern_update(vwk);

    l = vdi_Color_validateIndex(*pointer++); /* lineaVars.INTIN[9] */
    vwk->fill_color = vdi_context.palette.penToPaletteTable[l];

    vwk->xfm_mode = *pointer; /* lineaVars.INTIN[10] */

    vwk->wrt_mode = WM_REPLACE; /* default is replace mode */
    vwk->line_width = lineaVars.workstation_sizeTable.lineWidthMin;
    vwk->line_beg = vdi_LineEndStyle_square; /* default to squared ends */
    vwk->line_end = vdi_LineEndStyle_square;

    vwk->fill_per = true;

    vwk->clippingEnabled = false;
    vwk->clippingRect = vdi_context.clippingRectFull;
    
    vdi_Text_initialize2(vwk);

    vwk->ud_ls = vdi_Line_predefinedStyles[0];

    copyWords(linea_deviceTableSize, lineaVars.workstation_deviceTable, lineaVars.INTOUT);
    copyWords(linea_sizeTableLength, lineaVars.workstation_sizeTable.array, lineaVars.PTSOUT);

    #if vdi_Bezier_enabled
    vdi_Bezier_initialize(vwk);
    #endif

    vwk->next_work = NULL;  /* neatness */

    vwk->yFlipped = true;
}

// Build a chain of Vwks.
// This links all of the currently allocated Vwks together, as in Atari TOS.
// Some programs may depend on this.
static void vdi_Workstation_buildChain(void) {
    WORD handle;
    vdi_VirtualWorkstation *prev = &vdi_context.physicalWorkstation, **contextSlot;
    for (handle = vdi_physicalHandle + 1, contextSlot = vdi_context.workstations + handle; handle <= vdi_lastHandle; handle++, contextSlot++) {
        if (*contextSlot) {
            prev->next_work = *contextSlot;
            prev = *contextSlot;
        }
    }
    prev->next_work = NULL;
}

static void vdi_v_opnvwk(vdi_VirtualWorkstation * vwk) {
    // Ensure that lineaVars.workstation_current always points to a valid workstation even if v_opnvwk() exits early.
    lineaVars.workstation_current = &vdi_context.physicalWorkstation;

    /* First find a free handle */
    WORD handle;
    vdi_VirtualWorkstation **contextSlot;
    for (handle = vdi_physicalHandle + 1, contextSlot = vdi_context.workstations + handle; handle <= vdi_lastHandle; handle++, contextSlot++) {
        if (!*contextSlot)
            break;
    }
    if (handle > vdi_lastHandle)
        goto on_error; /* No handle available, exit */

    /*
     * Allocate the memory for a virtual workstation
     *
     * The virtual workstations for all programs are chained together by
     * build_vwk_chain(), because some programs (notably Warp9) expect this.
     * To avoid problems when running FreeMiNT with memory protection, we
     * must allocate the virtual workstations in supervisor-accessible memory.
     */
    vwk = (vdi_VirtualWorkstation *)Mxalloc(sizeof(vdi_VirtualWorkstation), MX_SUPER);
    if (vwk == NULL)
        goto on_error; /* No memory available, exit */

    *contextSlot = vwk;
    vwk->handle = lineaVars.parameters.contrl->workstationHandle = handle;
    vdi_Workstation_initialize(vwk);
    vdi_Workstation_buildChain();
    lineaVars.workstation_current = vwk;
    return;
on_error:
    lineaVars.parameters.contrl->workstationHandle = 0;
}

static void vdi_v_clsvwk(vdi_VirtualWorkstation * vwk) {
    /* vwk points to workstation to deallocate */
    WORD handle = vwk->handle;
    if (handle == vdi_physicalHandle || !vdi_context.workstations[handle])
        return; // Cannot close the physical workstation with this function or cannot close an already closed workstation.
    vdi_context.workstations[handle] = NULL; // Set as closed.
    vdi_Workstation_buildChain();

    /*
     * When we close a virtual workstation, Atari TOS and previous versions
     * of EmuTOS update lineaVars.workstation_current (line-A's idea of the current workstation)
     * to point to the precursor of the closed workstation.  This is a bit
     * arbitrary, especially as the workstation being closed isn't necessarily
     * what line-A thinks is the current one.
     *
     * What we must do as a minimum is ensure that lineaVars.workstation_current points to a
     * valid open workstation.  The following does that by pointing it to
     * the physical workstation. That's what NVDI appears to do too.
     */
    lineaVars.workstation_current = &vdi_context.physicalWorkstation;

    Mfree(vwk);
}

static void vdi_v_opnwk(vdi_VirtualWorkstation * vwk) {
    // Programs can request a video mode switch by passing the desired mode + 2 in lineaVars.INTIN[0].
    WORD newrez = lineaVars.INTIN[0] - 2;
    if (
        (newrez == ST_LOW) || (newrez == ST_MEDIUM) || (newrez == ST_HIGH)
#if CONF_WITH_TT_SHIFTER
        || (newrez == TT_LOW) || (newrez == TT_MEDIUM)
#endif
       )
    {
        if (newrez != Getrez())
            Setscreen(-1L, -1L, newrez, 0);
    }

#if CONF_WITH_VIDEL
    if (newrez == FALCON_REZ) {
        /* Atari TOS 4 uses INTOUT (sic!) to pass new Videl mode. */
        WORD newvidel = lineaVars.INTOUT[45];
        WORD curvidel = VsetMode(-1);
        if (curvidel != newvidel)
            Setscreen(0L, 0L, newrez, newvidel);
    }
#endif

    /* We need to copy some initial table data from the ROM */
    for (int i = 0; i < linea_sizeTableLength; i++)
        lineaVars.workstation_sizeTable.array[i] = vdi_Workstation_defaultSizeTable.array[i];

    for (int i = 0; i < 45; i++) {
        lineaVars.workstation_deviceTable[i] = vdi_Workstation_defaultDeviceTable[i];
        lineaVars.workstation_inquireTable[i] = vdi_Workstation_defaultInquireTable[i];
    }

    vdi_updateResolutionDependent(); // If Setscreen() was called above, it has already been called too by linea_init() (which is called by Setscreen()).
    
    /* initialize the vwk pointer array */
    vwk = &vdi_context.physicalWorkstation;
    vdi_context.workstations[vdi_physicalHandle] = vwk;
    lineaVars.parameters.contrl->workstationHandle = vwk->handle = vdi_physicalHandle;
    {
        int i;
        vdi_VirtualWorkstation **p;
        for (i = vdi_physicalHandle+1, p = vdi_context.workstations + i; i <= vdi_lastHandle; i++)
            *p++ = NULL;
    }

    vdi_sharedBuffer.circle.lineWidth = -1; /* invalidate current line width */
    vdi_Palette_initialize(); /* Initialize palette etc. */
    vdi_Text_initialize(); /* initialize the lineaVars.workstation_sizeTable info */
    vdi_Workstation_initialize(vwk);
    vdi_Timer_initialize();
    vdi_Mouse_initialize(); /* initialize mouse */
    vdi_Escape_initialize(vwk); /* enter graphics mode */

    /* Just like TOS 2.06, make the physical workstation the current workstation for Line-A. */
    lineaVars.workstation_current = vwk;
}

// Close workstation.
static void vdi_v_clswk(vdi_VirtualWorkstation * vwk) {
    // Close all open virtual workstations.
    WORD handle;
    vdi_VirtualWorkstation **p;
    for (handle = vdi_physicalHandle+1, p = vdi_context.workstations+handle; handle <= vdi_lastHandle; handle++, p++) {
        if (*p) {
            Mfree(*p);
            *p = NULL;
        }
    }
    lineaVars.workstation_current = vdi_context.workstations[vdi_physicalHandle];
    vdi_Timer_finalize();
    vdi_Mouse_finalize();
    vdi_Escape_finalize(vwk); // Back to console mode.
}

// Clear screen
// Screen is cleared from the base address v_bas_ad.
static void vdi_v_clrwk(vdi_VirtualWorkstation * vwk) {
    /* Calculate screen size */
    ULONG size = (ULONG)lineaVars.screen_lineSize2 * lineaVars.screen_height;
    /* clear the screen */
    bzero(v_bas_ad, size);
}

// Extended workstation inquire.
static void vdi_vq_extnd(vdi_VirtualWorkstation * vwk) {
    vwk->yFlipped = true;
    WORD *src, *dst = lineaVars.PTSOUT;
    if (lineaVars.INTIN[0] == 0) {
        src = lineaVars.workstation_sizeTable.array;
        for (WORD i = 0; i < linea_sizeTableLength; i++)
            *dst++ = *src++;
        src = lineaVars.workstation_deviceTable;
    } else {
        /* copy the clipping ranges to lineaVars.PTSOUT */
        *dst++ = vwk->clippingRect.xMin; /* lineaVars.PTSOUT[0] */
        *dst++ = vwk->clippingRect.yMin; /* lineaVars.PTSOUT[1] */
        *dst++ = vwk->clippingRect.xMax; /* lineaVars.PTSOUT[2] */
        *dst++ = vwk->clippingRect.yMax; /* lineaVars.PTSOUT[3] */
        for (WORD i = 4; i < 12; i++)
            *dst++ = 0;
        src = lineaVars.workstation_inquireTable;
        lineaVars.workstation_inquireTable[19] = vwk->clippingEnabled; /* now update INQTAB */
    }
    /* copy lineaVars.workstation_deviceTable or lineaVars.workstation_inquireTable to lineaVars.INTOUT */
    dst = lineaVars.INTOUT;
    for (WORD i = 0; i < 45; i++)
        *dst++ = *src++;
}

//--------------------------------------------------------------------------------
// Screen driver dispatcher.
//--------------------------------------------------------------------------------
typedef void (*vdi_Driver_Function)(vdi_VirtualWorkstation *); /* Pointer type to VDI operation */
#define vdi_v_nop ((vdi_Driver_Function)just_rts) /* VDI dummy operation */

struct vdi_jmptab {
    unsigned char nptsout;
    unsigned char nintout;
    vdi_Driver_Function op;
};

/* Two main jumptables for VDI functions */
static struct vdi_jmptab const vdi_Driver_jumpTable1[] = {
    { 6, 45, vdi_v_opnwk },            /*   1 */
    { 0,  0, vdi_v_clswk },            /*   2 */
    { 0,  0, vdi_v_clrwk },            /*   3 */
    { 0,  0, vdi_v_nop },              /*   4 - v_updwk not yet implemented */
    { 0,  0, vdi_v_escape },           /*   5 - each escape subfunction has its own call */
    { 0,  0, vdi_v_pline },            /*   6 */
    { 0,  0, vdi_v_pmarker },          /*   7 */
    { 0,  0, vdi_v_gtext },            /*   8 */
    { 0,  0, vdi_v_fillarea },         /*   9 */
    { 0,  0, vdi_v_nop },              /*  10 - v_cellarray(), not usually implemented by drivers */
    { 0,  0, vdi_v_gdp },              /*  11 */
    { 2,  0, vdi_vst_height },         /*  12 */
    { 0,  1, vdi_vst_rotation },       /*  13 */
    { 0,  0, vdi_vs_color },           /*  14 */
    { 0,  1, vdi_vsl_type },           /*  15 */
    { 1,  0, vdi_vsl_width },          /*  16 */
    { 0,  1, vdi_vsl_color },          /*  17 */
    { 0,  1, vdi_vsm_type },           /*  18 */
    { 1,  0, vdi_vsm_height },         /*  19 */
    { 0,  1, vdi_vsm_color },          /*  20 */
    { 0,  1, vdi_vst_font },           /*  21 */
    { 0,  1, vdi_vst_color },          /*  22 */
    { 0,  1, vdi_vsf_interior },       /*  23 */
    { 0,  1, vdi_vsf_style },          /*  24 */
    { 0,  1, vdi_vsf_color },          /*  25 */
    { 0,  4, vdi_vq_color },           /*  26 */
    { 0,  0, vdi_v_nop },              /*  27 - vq_cellarray, not usually implemented by drivers */
    { 0,  0, vdi_v_locator },          /*  28 */
    { 0,  0, vdi_v_nop },              /*  29 - vdi_v_valuator, not usually implemented by drivers */
    { 0,  1, vdi_v_choice },           /*  30 */
    { 0,  0, vdi_v_string },           /*  31 */
    { 0,  1, vdi_vswr_mode },          /*  32 */
    { 0,  1, vdi_vsin_mode },          /*  33 */
    { 0,  0, vdi_v_nop },              /*  34 - does not exist */
    { 1,  3, vdi_vql_attributes },     /*  35 */
    { 1,  3, vdi_vqm_attributes },     /*  36 */
    { 0,  5, vdi_vqf_attributes },     /*  37 */
    { 2,  6, vdi_vqt_attributes },     /*  38 */
    { 0,  2, vdi_vst_alignment }       /*  39 */
};

static struct vdi_jmptab const vdi_Driver_jumpTable2[] = {
    { 6, 45, vdi_v_opnvwk },           /* 100 */
    { 0,  0, vdi_v_clsvwk },           /* 101 */
    { 6, 45, vdi_vq_extnd },           /* 102 */
    { 0,  0, vdi_v_contourfill },      /* 103 */
    { 0,  1, vdi_vsf_perimeter },      /* 104 */
    { 0,  2, vdi_v_get_pixel },        /* 105 */
    { 0,  1, vdi_vst_effects },        /* 106 */
    { 2,  1, vdi_vst_point },          /* 107 */
    { 0,  2, vdi_vsl_ends },           /* 108 */
    { 0,  0, vdi_vro_cpyfm },          /* 109 */
    { 0,  0, vdi_vr_trnfm },           /* 110 */
    { 0,  0, vdi_vsc_form },           /* 111 */
    { 0,  0, vdi_vsf_udpat },          /* 112 */
    { 0,  0, vdi_vsl_udsty },          /* 113 */
    { 0,  0, vdi_vr_recfl },           /* 114 */
    { 0,  1, vdi_vqin_mode },          /* 115 */
    { 4,  0, vdi_vqt_extent },         /* 116 */
    { 3,  1, vdi_vqt_width },          /* 117 */
    { 0,  1, vdi_vex_timv },           /* 118 */
    { 0,  1, vdi_vst_load_fonts },     /* 119 */
    { 0,  0, vdi_vst_unload_fonts },   /* 120 */
    { 0,  0, vdi_vrt_cpyfm },          /* 121 */
    { 0,  0, vdi_v_show_c },           /* 122 */
    { 0,  0, vdi_v_hide_c },           /* 123 */
    { 1,  1, vdi_vq_mouse },           /* 124 */
    { 0,  0, vdi_vex_butv },           /* 125 */
    { 0,  0, vdi_vex_motv },           /* 126 */
    { 0,  0, vdi_vex_curv },           /* 127 */
    { 0,  1, vdi_vq_key_s },           /* 128 */
    { 0,  0, vdi_vs_clip },            /* 129 */
    { 0, 33, vdi_vqt_name },           /* 130 */
    { 5,  2, vdi_vqt_fontinfo },       /* 131 */
#if CONF_WITH_EXTENDED_MOUSE
    { 0,  0, vdi_v_nop },              /* 132 */ /* vqt_justified (PC-GEM) */
    { 0,  0, vdi_v_nop },              /* 133 */ /* vs_grayoverride (PC-GEM/3) */
    { 0,  0, vdi_vex_wheelv }          /* 134 */ /* (Milan), also v_pat_rotate (PC-GEM/3) */
#endif
};

#define JMPTB1_ENTRIES  ARRAY_SIZE(vdi_Driver_jumpTable1)
#define JMPTB2_ENTRIES  ARRAY_SIZE(vdi_Driver_jumpTable2)

/*
 * some VDI opcodes
 */
#define V_OPNWK_OP      1
#define V_CLSWK_OP      2
#define V_OPNVWK_OP     100
#define V_CLSVWK_OP     101

static bool vdi_Driver_enterScreen(Linea *linea, vdi_Contrl *contrl) {
    bool yFlipped = false;
    WORD opcode = contrl->opcode;

    vdi_VirtualWorkstation *vwk = NULL;
    if (opcode != V_OPNWK_OP && opcode != V_OPNVWK_OP) {
        vwk = vdi_Workstation_getByHandle(contrl->workstationHandle);
        if (!vwk)
            goto on_exit;
        vwk->yFlipped = false;
    }

    {
        const struct vdi_jmptab *jmptab;
        if (opcode >= V_OPNWK_OP && opcode < (V_OPNWK_OP + JMPTB1_ENTRIES))
            jmptab = &vdi_Driver_jumpTable1[opcode - V_OPNWK_OP];
        else if (opcode >= V_OPNVWK_OP && opcode < (V_OPNVWK_OP + JMPTB2_ENTRIES))
            jmptab = &vdi_Driver_jumpTable2[opcode - V_OPNVWK_OP];
        else
            goto on_exit;
        contrl->outputVertexNb = jmptab->nptsout;
        contrl->outputIntNb = jmptab->nintout;
        (*jmptab->op)(vwk);
    }
   
    // Set some line-A variables from the vwk info (as long as the workstation is valid).
    if (opcode != V_CLSWK_OP && opcode != V_CLSVWK_OP) { /* if neither v_clswk() nor v_clsvwk() */
        // At this point, for v_opnwk() and v_opnvwk(), vwk is NULL.
        // We must fix this before we use it to set the lineA variables below.
        // Fortunately, v_opnwk() and v_opnvwk() have set lineaVars.workstation_current to a valid value (see vdi_control.c).
        // So we use this to set vwk.
        if (!vwk)
            vwk = linea->workstation_current;
        else
            yFlipped = vwk->yFlipped;
            
        // The following assignments are not required by EmuTOS, but
        // ensure that the values in the line-A variables mirror those
        // in the current virtual workstation, just like in Atari TOS.
        linea->text_currentFont = vwk->cur_font;
        linea->writingMode = vwk->wrt_mode;
        linea->clipping_enabled = vwk->clippingEnabled;
        linea->clipping_rect = vwk->clippingRect;
        linea->workstation_current = vwk;
    }
    return yFlipped;
on_exit:
    /* no ints out & no pts out */
    contrl->outputVertexNb = 0;
    contrl->outputIntNb = 0;
    return false;
}

WORD vdi_dispatch(vdi_Parameters *pb);
WORD vdi_dispatch(vdi_Parameters *pb) {
    vdi_Contrl *contrl = pb->contrl;
    ULONG *ptsinSrc = (ULONG*)pb->ptsin, *ptsinDst = (ULONG*)&vdi_sharedBuffer;
    Linea *linea = &lineaVars;
    linea->CONTRL = (WORD*)contrl;
    linea->INTIN = pb->intin;
    linea->PTSIN = (WORD*)ptsinDst;
    linea->INTOUT = pb->intout;
    linea->PTSOUT = pb->ptsout;
    WORD inputVertexNb = contrl->inputVertexNb;
    // FIXME: Do we need to copy all the vertices ? It is slow.
    if (inputVertexNb > 0) {
        WORD n = inputVertexNb;
        if (n > CONF_VDI_MAX_VERTICES) {
            n = CONF_VDI_MAX_VERTICES;
            contrl->inputVertexNb = n;
        }
        copyLongs(n, (const LONG *)ptsinSrc, (LONG *)ptsinDst);
    }
    bool yFlipped = vdi_Driver_enterScreen(linea, contrl);
    contrl->inputVertexNb = inputVertexNb;
    return yFlipped;
}

#if CONF_WITH_VDI_LINEA

//********************************************************************************
// Line-A.
//********************************************************************************
//--------------------------------------------------------------------------------
// Line-A public functions prototypes.
//--------------------------------------------------------------------------------
void linea_show_mouse(void);
void linea_hide_mouse(void);
void linea_transform_mouse(void);

WORD linea_get_pix(void);

void linea_put_pix(void);

/**
 * Horizontal line drawing.
 */
void linea_hline(void);

/**
 * General line.
 */
void linea_line(void);

/**
 * Fill a rectangle.
 */
void linea_rect(void);

/**
 * Draw a polygon.
 */
void linea_polygon(void);

void linea_blit(vdi_BlitParameters *info);

void linea_raster(void);

/**
 * Flood fill.
 */
void linea_fill(void);

//--------------------------------------------------------------------------------
// Line-A.
//--------------------------------------------------------------------------------
// Compose color for from line-A variables.
static UWORD linea_color(void) {
    UWORD color = 0;
    // Below we use += instead of |= because GCC produces better code especially addq.w instead of ori.w.
    if (lineaVars.color_bit0 != 0)
        color += 1;
    if (lineaVars.color_bit1 != 0)
        color += 2;
    if (lineaVars.color_bit2 != 0)
        color += 4;
    if (lineaVars.color_bit3 != 0)
        color += 8;
    return color;
}

// Sets VwkAttrib fields from line-A variables.
static void linea_convertToAttrib(vdi_DrawContext * RESTRICT dc, UWORD color, bool multiFill) {
    vdi_DrawContext_setupPlaneNb(dc);
    dc->clipping.enabled = lineaVars.clipping_enabled;
    if (dc->clipping.enabled)
        dc->clipping.rect = lineaVars.clipping_rect;
    else
        dc->clipping.rect = vdi_context.clippingRectFull;
    if (lineaVars.pattern_address) {
        dc->pattern.mask = lineaVars.pattern_mask;
        dc->pattern.data = lineaVars.pattern_address;
    } else {
        /* pattern is always needed for rectangle filling, default to solid */
        dc->pattern.mask = 0;
        dc->pattern.data = &vdi_FillPattern_solid;
    }
    dc->mode = lineaVars.writingMode;
    dc->color = color;
    dc->multiFill = multiFill;
}
 
void linea_show_mouse(void) {
    vdi_v_show_c(NULL);
}

void linea_hide_mouse(void) {
    vdi_v_hide_c(NULL);
}

void linea_transform_mouse(void) {
    vdi_vsc_form(NULL);
}

 /*
 * Gets a pixel (just for line-A)
 *
 * input:
 *     lineaVars.PTSIN(0) = x coordinate.
 *     lineaVars.PTSIN(1) = y coordinate.
 * output:
 *     pixel value
 */
WORD linea_get_pix(void) {
    /* return the composed color value */
    return vdi_Pixel_read(lineaVars.PTSIN[0], lineaVars.PTSIN[1]);
}

/*
 * Plot a pixel (just for line-A)
 *
 * input:
 *     lineaVars.INTIN(0) = pixel value.
 *     lineaVars.PTSIN(0) = x coordinate.
 *     lineaVars.PTSIN(1) = y coordinate.
 */
void linea_put_pix(void) {
    vdi_Pixel_write(lineaVars.PTSIN[0], lineaVars.PTSIN[1], lineaVars.INTIN[0]);
}

void linea_hline(void) {
    vdi_DrawContext dc;
    linea_convertToAttrib(&dc, linea_color(), lineaVars.pattern_multiPlaneFillFlag); /* linea4 supports lineaVars.pattern_multiPlaneFillFlag */
    dc.rect = lineaVars.rect;
    dc.rect.y2 = dc.rect.y1;
    vdi_getDriver()->fillRectangle(&dc);
}

void linea_line(void) {
    vdi_DrawContext dc;
    vdi_Line_drawStart(&dc, lineaVars.workstation_current, lineaVars.clipping_enabled, lineaVars.writingMode, linea_color());
    dc.line.line = lineaVars.rect;
    dc.line.lastFlag = lineaVars.line_lastFlag;
    vdi_getDriver()->drawLine(&dc);
    vdi_Line_drawEnd(&dc);
}

void linea_rect(void) {
    vdi_DrawContext dc;
    linea_convertToAttrib(&dc, linea_color(), lineaVars.pattern_multiPlaneFillFlag); /* linea5 supports lineaVars.pattern_multiPlaneFillFlag */
    dc.rect = lineaVars.rect;
    vdi_getDriver()->fillRectangle(&dc);
}

void linea_polygon(void) {
    vdi_DrawContext dc;
    linea_convertToAttrib(&dc, linea_color(), false); /* linea6 does not support lineaVars.pattern_multiPlaneFillFlag */
    dc.polygon.points = (Point*)lineaVars.PTSIN;
    dc.polygon.pointNb = lineaVars.parameters.contrl->inputVertexNb;
    dc.polygon.currentY = lineaVars.rect.y1;
    vdi_getDriver()->fillPolygonSpan(&dc);
}

void linea_fill(void) {
    vdi_DrawContext dc;
    linea_convertToAttrib(&dc, lineaVars.workstation_current->fill_color, false); /* lineaf does not support lineaVars.pattern_multiPlaneFillFlag */
    dc.seedFilling.abort = vdi_SeedFilling_noAbort;
    dc.seedFilling.startX = lineaVars.PTSIN[0];
    dc.seedFilling.startY = lineaVars.PTSIN[1];
    dc.seedFilling.searchColor = lineaVars.INTIN[0];
    vdi_getDriver()->seedFill(&dc);
}

void linea_raster(void) {
    struct vdi_RasterInfos raster;
    raster.clippingRect = NULL;
    raster.clippingEnabled = false;
    raster.multifill = lineaVars.pattern_multiPlaneFillFlag;
    raster.transparent = lineaVars.raster_transparent;
    vdi_Raster_copy(&raster);
}

void linea_blit(vdi_BlitParameters *info) {
    /* with line-A, need to calculate these for bit_blt() (whereas VDI needs to calculate wd & ht) */
    info->s_xmax = info->s_xmin + info->b_wd - 1;
    info->s_ymax = info->s_ymin + info->b_ht - 1;
    info->d_xmax = info->d_xmin + info->b_wd - 1;
    info->d_ymax = info->d_ymin + info->b_ht - 1;
    vdi_BitBlt_blit(info);
}

#if 0
// Currently not reimplemented from assembly because d0-d1/a0-a1 must be transferred to the Line-A function
// and I don't know how this can be done when mixing C and assembly with Gcc.
__attribute__((interrupt))
void linea_handleException(UWORD status, UWORD *pc) {
    UWORD opcode = *pc;
    #ifdef __mcoldfire__
    /* On ColdFire, all the standard Line A opcodes conflict with valid MAC instructions. Therefore they can't be used.
     * Fortunately, the 0xA92x opcodes are still illegal and trigger the Line A exception.
     * So programs can use 0xA92x instead of 0xA00x to call the Line A.
     * Thus we must keep only the last digit as function number. */
    opcode &= 0x000f; // Keep only the function number.
    #else
    opcode &= 0x0fff; // Keep only the function number.
    #endif
    if (
    
}
#endif

void* linea_getVariablesBase(void);
void* linea_getVariablesBase(void) {
    return &lineaVars.base;
}

// Init linea variables.
void linea_init(void) {
    screen_get_current_mode_info(&lineaVars.screen_planeNb, &lineaVars.screen_width, &lineaVars.screen_height);
    // Precalculate shift value to optimize pixel address calculations.
    vdi_setScreenShift();
    // Update resolution-dependent values.
    vdi_updateResolutionDependent();
    KDEBUG(("linea_init(): %dx%d %d-plane pitch=%d\n", lineaVars.screen_width, lineaVars.screen_height, lineaVars.screen_planeNb, lineaVars.screen_lineSize));
}

#endif
