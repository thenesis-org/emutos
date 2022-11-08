/*
 * blitter.h - header for blitter routines
 *
 * Copyright (C) 2017-2020 The EmuTOS development team
 *
 * Authors:
 *  RFB   Roger Burrows
 *
 * This file is distributed under the GPL, version 2 or at your
 * option any later version.  See doc/license.txt for details.
 */
#ifndef blitter_h
#define blitter_h

#include "emutos.h"

// The following are used by the blitter and the blitter emulation code.

// Bitblt modes. Keep this exact order, this is used also for the blitter hardware.
typedef enum {
    BM_ALL_WHITE,
    BM_S_AND_D,
    BM_S_AND_NOTD,
    BM_S_ONLY,
    BM_NOTS_AND_D,
    BM_D_ONLY,
    BM_S_XOR_D,
    BM_S_OR_D,
    BM_NOT_SORD,
    BM_NOT_SXORD,
    BM_NOT_D,
    BM_S_OR_NOTD,
    BM_NOT_S,
    BM_NOTS_OR_D,
    BM_NOT_SANDD,
    BM_ALL_BLACK
} BlitterOp;

typedef struct {
    UWORD           halftone[16];       /* halftone RAM */
    WORD            src_x_inc;         /* source X increment */
    WORD            src_y_inc;         /* source Y increment */
    volatile UWORD  *src_addr;          /* source address */
    UWORD           endmask_1;          /* for first write of line */
    UWORD           endmask_2;          /* for other writes */
    UWORD           endmask_3;          /* for last write of line */
    WORD            dst_x_inc;         /* destination X increment */
    WORD            dst_y_inc;         /* destination Y increment */
    volatile UWORD  *dst_addr;          /* destination address */
    volatile UWORD  x_count;            /* X count */
    volatile UWORD  y_count;            /* Y count */
    UBYTE           hop;                /* HOP */
    UBYTE           op;                 /* OP */
    volatile UBYTE  status;             /* status bits & line# */
    UBYTE           skew;               /* FXSR, NFSR, & skew */
} BLIT;

#if CONF_WITH_BLITTER
#define BLITTER ((BLIT * RESTRICT)0xFFFF8A00L)
#endif

// Values for hop
#define HOP_ALL_ONES            0
#define HOP_HALFTONE_ONLY       1
#define HOP_SOURCE_ONLY         2
#define HOP_SOURCE_AND_HALFTONE 3

// Values for status
#define BUSY        0x80
#define HOG         0x40
#define SMUDGE      0x20
#define LINENO      0x0f

// Values for skew
#define FXSR    0x80
#define NFSR    0x40
#define SKEW    0x0f

#endif
