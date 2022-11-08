// This file contains the private declarations used internally by the VDI. It should not be used outside the VDI.
#ifndef vdi_internal_h
#define vdi_internal_h

#include "vdi/vdi_interface.h"

#include "asm.h"
#include "blitter.h"
#include "has.h"
#include "intmath.h"
#include "tosvars.h"

#include <stdlib.h>
#include <stdint.h>

#define vdi_Soft_asmEnabled 1 // Enable or disable inline assembly.

//--------------------------------------------------------------------------------
// Target specific loops.
//--------------------------------------------------------------------------------
#define vdi_blitterOnly 0
#define vdi_drawLine_lastLineLegacy 1

#define LOOP_DO(counter, count) { WORD counter = (count) - 1; do // -1 so that the compiler can use dbra.
#define LOOP_WHILE(counter) while (--counter != -1); } // Instead of "--plane >= 0" so that GCC generates a dbra.

//#define LOOP_DBF_DO(counter, count, label) { register WORD counter asm("d7") = count - 1; label:
//#define LOOP_DBF_WHILE(counter, label) __asm__ goto ("dbra d7,%0\n\t" : : : "d7" : label); }
//#define LOOP_DBF_DO(counter, count, label) { WORD counter = count - 1; label:
//#define LOOP_DBF_WHILE(counter, label) __asm__ goto ("dbra %0,%1\n\t" : : "d" (counter) :  : label); }

//--------------------------------------------------------------------------------
// Workstation.
//--------------------------------------------------------------------------------
#if CONF_WITH_VIDEL
# define vdi_userDefinedPattern_planeNb (2 * 16) // Actually 16, but each plane occupies 2 WORDs.
#elif CONF_WITH_TT_SHIFTER
# define vdi_userDefinedPattern_planeNb 8
#else
# define vdi_userDefinedPattern_planeNb 4
#endif

/* Structure to hold data for a virtual workstation */
/* NOTE 1: for backwards compatibility with all versions of TOS, the
 * field 'fill_color' must remain at offset 0x1e, because the line-A
 * flood fill function uses the fill colour from the currently-open
 * virtual workstation, and it is documented that users can provide
 * a fake virtual workstation by pointing lineaVars.workstation_current to a 16-element
 * WORD array whose last element contains the fill colour.
 */
struct vdi_VirtualWorkstation_ {
    WORD chup;                  /* Character Up vector */
    WORD clippingEnabled;       /* Clipping Flag */
    const Fonthead *cur_font;   /* Pointer to current font */
    UWORD dda_inc;              /* Fraction to be added to the DDA */
    BOOL multiFillAvailable; // True if the user pattern supports multi fill.
    BOOL multiFillEnabled; // True if multi fill is currently enabled.
    UWORD patmsk;               /* Current pattern mask */
    UWORD *patptr;              /* Current pattern pointer */
    WORD pts_mode;              /* true if height set in points mode */
    WORD *scrtchp;              /* Pointer to text scratch buffer */
    WORD scrpt2;                /* Offset to large text buffer */
    WORD style;                 /* Current text style */
    WORD t_sclsts;              /* true if scaling up */
    WORD fill_color;            /* Current fill color (PEL value): see NOTE 1 above */
    WORD fill_index;            /* Current fill index */
    WORD fill_per;              /* true if fill area outlined */
    WORD fill_style;            /* Current fill style */
    WORD h_align;               /* Current text horizontal alignment */
    WORD handle;                /* The handle this attribute area is for */
    WORD line_beg;              /* Beginning line endstyle */
    WORD line_color;            /* Current line color (PEL value) */
    WORD line_end;              /* Ending line endstyle */
    WORD line_index;            /* Current line style */
    WORD line_width;            /* Current line width */
    const Fonthead *loaded_fonts; /* Pointer to first loaded font */
    WORD mark_color;            /* Current marker color (PEL value)     */
    WORD mark_height;           /* Current marker height        */
    WORD mark_index;            /* Current marker style         */
    WORD mark_scale;            /* Current scale factor for marker data */
    vdi_VirtualWorkstation *next_work;             /* Pointer to next virtual workstation  */
    WORD num_fonts;             /* Total number of faces available  */
    WORD scaled;                /* true if font scaled in any way   */
    Fonthead scratch_head;      /* Holder for the doubled font data */
    WORD text_color;            /* Current text color (PEL value)   */
    WORD ud_ls;                 /* User defined linestyle       */
    WORD ud_patrn[vdi_userDefinedPattern_planeNb * 16]; /* User defined pattern */
    WORD v_align;               /* Current text vertical alignment  */
    WORD wrt_mode;              /* Current writing mode         */
    WORD xfm_mode;              /* Transformation mode requested (NDC) */
    ClippingRect clippingRect; // Current clipping rectangle.
    /* newly added */
    #if vdi_Bezier_enabled
    WORD bez_qual;              /* actual quality for bezier curves */
    #endif
    bool yFlipped; // True if magnitudes being returned.
};

//--------------------------------------------------------------------------------
// Global context.
//--------------------------------------------------------------------------------
#define vdi_firstHandle 1
#define vdi_lastHandle (vdi_firstHandle + NUM_VDI_HANDLES - 1)
#define vdi_physicalHandle vdi_firstHandle

#define vdi_Palette_extendedEnabled (CONF_WITH_VIDEL || CONF_WITH_TT_SHIFTER)
#if vdi_Palette_extendedEnabled
#define vdi_Palette_maxColours  256
#else
#define vdi_Palette_maxColours  16
#endif

typedef struct {
    struct {
        bool processing; /* flag, if we are still running */
    } timer;

    // Entry n in the following array points to the vdi_VirtualWorkstation corresponding to VDI handle n. Entry 0 is unused.
    vdi_VirtualWorkstation *workstations[1 + NUM_VDI_HANDLES];
    vdi_VirtualWorkstation physicalWorkstation;
    ClippingRect clippingRectFull; // Clipping rectangle for the full device.
    UBYTE planeNbShift; // To get the address of a pixel x in a scan line, use the formula: (x&0xfff0)>>vdi_context.planeNbShift

    struct {
        WORD penToPaletteTable[vdi_Palette_maxColours]; /* maps vdi pen -> hardware register */
        WORD paletteToPenTable[vdi_Palette_maxColours]; /* maps hardware register -> vdi pen */
    } palette;
    
    struct {
        vdi_MouseCursorSaveArea *saveArea; // Ptr to current mouse cursor save area, based on lineaVars.screen_planeNb.
        PFVOID userWheelHandler; // User mouse wheel vector provided by vdi_vex_wheelv().
        PFVOID savedIkbdStatusRoutine; // Original IKBD status packet routine.
    } mouse;
} vdi_Context;

extern vdi_Context vdi_context;

//--------------------------------------------------------------------------------
// Screen.
//--------------------------------------------------------------------------------
extern const UBYTE vdi_planeNbToRightShift[9];

extern const UBYTE vdi_planeNbToLeftShift[9];

forceinline WORD vdi_scaleWordByPlaneNb(WORD w, WORD planeNb) {
    return w << vdi_planeNbToLeftShift[planeNb];
}

UWORD * vdi_getPixelAddress(WORD x, WORD y);

//--------------------------------------------------------------------------------
// Pixel access.
//--------------------------------------------------------------------------------
forceinline UWORD vdi_getPixelColor(WORD planeNb, UWORD mask, UWORD * addr) {
    addr += planeNb; // We go from high to low order bit for more efficiency.
    UWORD color = 0;
    LOOP_DO(planeIndex, planeNb) {
        color <<= 1;
        if (*--addr & mask)
            color |= 1;
    } LOOP_WHILE(planeIndex);
    return color;
}

forceinline void vdi_setPixelColor(WORD planeNb, UWORD mask, UWORD * addr, UWORD color) {
    LOOP_DO(planeIndex, planeNb) {
        UWORD w = *addr;
        if (color & 0x0001)
            w |= mask;
        else
            w &= ~mask;
        *addr++ = w;
        color >>= 1;
    } LOOP_WHILE(planeIndex);
}

forceinline bool vdi_checkPixelCoordinates(WORD x, WORD y) {
    return x >= 0 && x < lineaVars.screen_width && y >= 0 && y < lineaVars.screen_height;
}

forceinline UWORD mergePixel(UWORD pixelsOld, UWORD pixelsNew, UWORD mask) {
    #if 0
    return (pixelsNew & mask) | (pixelsOld & ~mask);
    #else
    // This version gives the same result but has one less operation.
    // Gcc generates automically this version from the above one anyway.
    // (pixelsNew & mask) | (pixelsOld & !mask) <=> ((pixelsNew ^ pixelsOld) & endMask) ^ pixelsOld
    return ((pixelsNew ^ pixelsOld) & mask) ^ pixelsOld;
    #endif
}

forceinline ULONG mergePixel32(ULONG pixelsOld, ULONG pixelsNew, ULONG mask) {
    #if 0
    return (pixelsNew & mask) | (pixelsOld & ~mask);
    #else
    // This version gives the same result but has one less operation.
    // Gcc generates automically this version from the above one anyway.
    // (pixelsNew & mask) | (pixelsOld & !mask) <=> ((pixelsNew ^ pixelsOld) & endMask) ^ pixelsOld
    return ((pixelsNew ^ pixelsOld) & mask) ^ pixelsOld;
    #endif
}

//--------------------------------------------------------------------------------
// FillingInfos.
//--------------------------------------------------------------------------------
typedef struct {
    WORD x1, x2; // Ordered x position.
    WORD y1, y2; // Ordered y position.
    WORD width, height;
    WORD wordNb; // Line width in words.
    UWORD leftMask, rightMask; // Endmasks.
    void *addr; // Starting screen address.
    WORD stride; // Distance in bytes between 2 destination lines.
    WORD planeNb; // Number of planes.
} vdi_FillingInfos;

// Coordinates must be sorted before the call: x1 < x2, y1 < y2.
// This is inline because the compiler will optimize out several variables depending of input values.
forceinline void vdi_FillingInfos_setup(vdi_FillingInfos * RESTRICT b, WORD x1, WORD x2, WORD y1, WORD y2) {
    b->x1 = x1; b->x2 = x2;
    b->y1 = y1; b->y2 = y2;
    b->width = x2 - x1 + 1;
    b->height = y2 - y1 + 1;
    b->wordNb = (x2 >> 4) - (x1 >> 4) + 1;
    b->leftMask = 0xffff >> (x1 & 0x0f); // TODO: Use table ?
    b->rightMask = 0xffff << (15 - (x2 & 0x0f));
    if (b->wordNb == 1) {
        b->leftMask &= b->rightMask;
        b->rightMask = 0;
    }
    b->addr = vdi_getPixelAddress(x1, y1);
    b->stride = lineaVars.screen_lineSize2;
    b->planeNb = lineaVars.screen_planeNb;
}

//--------------------------------------------------------------------------------
// Writing mode and blit operation.
//--------------------------------------------------------------------------------
// Internal values for writing modes.
enum {
    WM_REPLACE = vdi_WritingMode_replace-1,
    WM_TRANS   = vdi_WritingMode_transparent-1,
    WM_XOR     = vdi_WritingMode_xor-1,
    WM_ERASE   = vdi_WritingMode_erasing-1
};

forceinline WORD translateWritingModeToOp(WORD writingMode, UWORD colorFg, UWORD colorBg) {
    extern const uint8_t vdi_writingModeToOpTable[4*4 + 16*4];
    WORD blitModeIndex = writingMode; // 000xxxxx (0x00-0x13 are valid)
    blitModeIndex = (blitModeIndex << 1) | colorFg; // Plane foreground bit: 00xxxxxf.
    blitModeIndex = (blitModeIndex << 1) | colorBg; // Plane background bit: 0xxxxxfb.
    return vdi_writingModeToOpTable[blitModeIndex];
}

forceinline UWORD doPixelOp4(WORD op, UWORD s, UWORD d) {
    switch (op) {
    default:
    case WM_REPLACE: return  s;
    case WM_XOR:     return  s ^  d;
    case WM_TRANS:   return  s |  d;
    case WM_ERASE:   return ~s |  d;
    }
}

forceinline UWORD doPixelOp(WORD op, UWORD s, UWORD d) {
    switch (op) {
    default:
    case BM_ALL_WHITE:  return    0x0000;  // Case 0: no source or destination use.
    case BM_S_AND_D:    return    s &  d;  // Case 1: source op destination.
    case BM_S_AND_NOTD: return    s & ~d;  // Case 2: source op not destination, not source op destination.
    case BM_S_ONLY:     return    s;       // Case 4: source.
    case BM_NOTS_AND_D: return   ~s &  d;  // Case 2: source op not destination, not source op destination.
    case BM_D_ONLY:     return         d;  // Case 5: destination.
    case BM_S_XOR_D:    return    s ^  d;  // Case 1: source op destination.
    case BM_S_OR_D:     return    s |  d;  // Case 1: source op destination.
    case BM_NOT_SORD:   return ~( s |  d); // Case 3: not (source op destination).
    case BM_NOT_SXORD:  return ~( s ^  d); // Case 3: not (source op destination).
    case BM_NOT_D:      return        ~d;  // Case 7: not destination.
    case BM_S_OR_NOTD:  return    s | ~d;  // Case 2: source op not destination, not source op destination.
    case BM_NOT_S:      return   ~s;       // Case 6: not destination.
    case BM_NOTS_OR_D:  return   ~s |  d;  // Case 2: source op not destination, not source op destination.
    case BM_NOT_SANDD:  return ~( s &  d); // Case 3: not (source op destination).
    case BM_ALL_BLACK:  return    0xffff;  // Case 0: no source or destination use.
    }
}

forceinline UWORD doPixelOp4WithMask(WORD op, UWORD s, UWORD d, UWORD mask) {
    return mergePixel(d, doPixelOp4(op, s, d), mask);
}

forceinline UWORD doPixelOpWithMask(WORD op, UWORD s, UWORD d, UWORD mask) {
    return mergePixel(d, doPixelOp(op, s, d), mask);
}

//--------------------------------------------------------------------------------
// Line tools.
//--------------------------------------------------------------------------------
forceinline bool vdi_setupHorizontalLine(const Line *line, bool lastLineFlag, WORD mode, vdi_FillingInfos * RESTRICT fi) {
    WORD x1 = line->x1, x2 = line->x2;
    if (x1 > x2) { WORD t = x1; x1 = x2; x2 = t; }
    #if vdi_drawLine_lastLineLegacy
    // Copy a DRI kludge: if we're in XOR mode, avoid XORing intermediate points in a polyline.
    // We do it slightly differently than DRI with slightly differing results - but it's a kludge in either case.
    if (mode == WM_XOR && !lastLineFlag && x1 != x2)
        x2--;
    #else
    x2 -= !lastLineFlag;
    if (x1 > x2)
        return true;
    #endif
    vdi_FillingInfos_setup(fi, x1, x2, line->y1, line->y1);
    return false;
}

forceinline bool vdi_setupVerticalLine(const Line *line, bool lastLineFlag, WORD mode, vdi_FillingInfos * RESTRICT fi) {
    WORD dstStride = lineaVars.screen_lineSize2;
    WORD h = line->y2 - line->y1;
    if (h < 0) { h = -h; dstStride = -dstStride; }
    #if vdi_drawLine_lastLineLegacy
    // Copy a DRI kludge: if we're in XOR mode, avoid XORing intermediate points in a polyline.
    // We do it slightly differently than DRI with slightly differing results - but it's a kludge in either case.
    if (mode == WM_XOR && !lastLineFlag && h > 0)
        h--;
    #else
    h -= !lastLineFlag;
    if (h < 0)
        return true;
    #endif
    fi->height = h + 1;
    fi->addr = vdi_getPixelAddress(line->x1, line->y1);
    fi->stride = dstStride;
    fi->planeNb = lineaVars.screen_planeNb;
    return false;
}

//--------------------------------------------------------------------------------
// Driver.
//--------------------------------------------------------------------------------
/*
 * Small subset of vdi_VirtualWorkstation data.
 * Used by vdi_Rectangle_fillInternal to hide VDI/Line-A specific details from rectangle & polygon drawing.
 */
typedef struct {
    WORD clip;           /* polygon clipping on/off */
    WORD multifill;      /* Multi-plane fill flag   */
    UWORD patmsk;        /* Current pattern mask    */
    const UWORD *patptr; /* Current pattern pointer */
    WORD wrt_mode;       /* Current writing mode    */
    UWORD color;         /* fill color */
} VwkAttrib;

typedef struct vdi_Driver_ {
    void (*fillRectangle)(const vdi_FillingInfos * RESTRICT b, const VwkAttrib * RESTRICT attr);

    void (*drawLine)(const Line * RESTRICT line, WORD mode, UWORD color, bool lastLineFlag);
    void (*drawGeneralLine)(const Line * RESTRICT line, WORD mode, UWORD color, bool lastLineFlag);
    void (*drawVerticalLine)(const Line * RESTRICT line, WORD mode, UWORD color, bool lastLineFlag);
    void (*drawHorizontalLine)(const Line * RESTRICT line, WORD mode, UWORD color, bool lastLineFlag);
    
    void (*blit)(const vdi_BlitParameters * RESTRICT blit_info);
    void (*blitAll)(const vdi_BlitParameters * RESTRICT blit_info, BLIT * RESTRICT blt);
    void (*blitGeneral)(const vdi_BlitParameters * RESTRICT blit_info, BLIT * RESTRICT blt);
    void (*blitPlane)(BLIT * RESTRICT blt);
} vdi_Driver;

void vdi_Soft_fillRectangle(const vdi_FillingInfos * RESTRICT b, const VwkAttrib * RESTRICT attr);

void vdi_Soft_drawLine(const Line *line, WORD mode, UWORD color, bool lastLineFlag);
void vdi_Soft_drawGeneralLine(const Line *line, WORD mode, UWORD color, bool lastLineFlag);
void vdi_Soft_drawVerticalLine(const Line *line, WORD mode, UWORD color, bool lastLineFlag);
void vdi_Soft_drawHorizontalLine(const Line *line, WORD mode, UWORD color, bool lastLineFlag);

void vdi_Soft_blit(const vdi_BlitParameters *blit_info);
void vdi_Soft_blitAll(const vdi_BlitParameters *blit_info, BLIT * RESTRICT blt);
void vdi_Soft_blitGeneral(const vdi_BlitParameters *blit_info, BLIT * RESTRICT blt);
void vdi_Soft_blitPlane(BLIT * restrict blt);

#if CONF_WITH_BLITTER
extern const vdi_Driver vdi_Blitter_driver;
#endif
extern const vdi_Driver vdi_Soft_driver;

forceinline const vdi_Driver* vdi_getDriver(void) {
    #if CONF_WITH_BLITTER
    #if vdi_blitterOnly
    return vdi_Blitter_driver;
    #else
    return blitter_is_enabled ? &vdi_Blitter_driver : &vdi_Soft_driver;
    #endif
    #else
    return &vdi_Soft_driver;
    #endif
}

#endif
