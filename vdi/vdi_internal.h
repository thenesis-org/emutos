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

//********************************************************************************
// Tools.
//********************************************************************************
//--------------------------------------------------------------------------------
// Loops.
//--------------------------------------------------------------------------------
#define LOOP_DO(counter, count) { WORD counter = (count) - 1; do // -1 so that the compiler can use dbra.
#define LOOP_WHILE(counter) while (--counter != -1); } // Instead of "--plane >= 0" so that GCC generates a dbra.

//#define LOOP_DBF_DO(counter, count, label) { register WORD counter asm("d7") = count - 1; label:
//#define LOOP_DBF_WHILE(counter, label) __asm__ goto ("dbra d7,%0\n\t" : : : "d7" : label); }
//#define LOOP_DBF_DO(counter, count, label) { WORD counter = count - 1; label:
//#define LOOP_DBF_WHILE(counter, label) __asm__ goto ("dbra %0,%1\n\t" : : "d" (counter) :  : label); }

//--------------------------------------------------------------------------------
// Memory tools.
//--------------------------------------------------------------------------------
forceinline void copyWords(WORD n, const WORD *src, WORD *dst) {
    LOOP_DO(i, n) {
        *dst++ = *src++;
    } LOOP_WHILE(i);
}

forceinline void copyLongs(WORD n, const LONG *src, LONG *dst) {
    LOOP_DO(i, n) {
        *dst++ = *src++;
    } LOOP_WHILE(i);
}

//--------------------------------------------------------------------------------
// Math tools.
//--------------------------------------------------------------------------------
forceinline WORD clipWord(WORD v, WORD vMin, WORD vMax) {
    if (v < vMin) 
        v = vMin;
    if (v > vMax)
        v = vMax;
    return v;
}

forceinline WORD checkRangeWord(WORD v, WORD vMin, WORD vMax, WORD vDefault) {
    if (v < vMin || v > vMax) 
        v = vDefault;
    return v;
}

/*
 * Absolute for LONG
 */
forceinline LONG absLong(LONG x) {
    return (x < 0) ? -x : x;
}

//--------------------------------------------------------------------------------
// Geometric tools.
//--------------------------------------------------------------------------------
forceinline bool vdi_ClipRect_checkPointX(const ClippingRect *clippingRect, WORD x) {
    return x < clippingRect->xMin || x > clippingRect->xMax;
}

forceinline bool vdi_ClipRect_checkPointY(const ClippingRect *clippingRect, WORD y) {
    return y < clippingRect->yMin || y > clippingRect->yMax;
}

forceinline bool vdi_ClipRect_checkRect(const ClippingRect *clippingRect, WORD xMin, WORD yMin, WORD xMax, WORD yMax) {
    return xMax < clippingRect->xMin || xMin > clippingRect->xMax || yMax < clippingRect->yMin || yMin > clippingRect->xMax;
}

// Coordinates must be sorted.
bool vdi_ClipRect_clipSingleCoordinate(WORD xMin, WORD xMax, WORD *x);

forceinline void vdi_Rect_sortX(Rect *rect) {
    WORD x1 = rect->x1, x2 = rect->x2;
    if (x1 > x2) { rect->x1 = x2; rect->x2 = x1; }
}

forceinline void vdi_Rect_sortY(Rect *rect) {
    WORD y1 = rect->y1, y2 = rect->y2;
    if (y1 > y2) { rect->y1 = y2; rect->y2 = y1; }
}

// Sort the corners. Raster (ll, ur) format is desired.
void vdi_Rect_sortCorners(Rect *rect);

// Clip a rectangle. Coordinates must be sorted.
bool vdi_Rect_clip(Rect * RESTRICT rect, const ClippingRect * RESTRICT clip);

void vdi_Line_sortVertices(Line *line);

//********************************************************************************
// Platform independent.
//********************************************************************************
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
// DrawContext.
//--------------------------------------------------------------------------------
// Common settings needed both by VDI and line-A raster operations, but being given through different means.
struct vdi_RasterInfos {
    ClippingRect *clippingRect;
    bool clippingEnabled;
    bool multifill;
    bool transparent;
};

#define vdi_Line_maxWidth 40

#define vdi_Circle_maxPixelAspectRatio 2 /* max expected value of vdi_deviceSizeX/vdi_deviceSizeY */
#define vdi_Circle_maxLines ((vdi_Line_maxWidth * vdi_Circle_maxPixelAspectRatio) / 2 + 1)
typedef struct {
    WORD lineWidth;
    WORD lineNb;
    /*
     * The following array holds values that allow vdi_WideLine_draw() to draw a rasterized circle.
     * The values are actually those required for a quarter of the circle, specifically quadrant 1.
     * [Quadrants are numbered 1-4, beginning with the "south-east" quadrant, and travelling clockwise].
     *
     * q_circle[n] contains the offset of the edge of the circle (from a vertical line through the centre of the circle),
     * for the nth line (counting from a horizontal line through the centre of the circle).
     */
    WORD offsets[vdi_Circle_maxLines]; // Holds the circle DDA.
} vdi_Circle;

// TODO: This structure should replace / contain vdi_FillingInfos and VwkAttrib.
// TODO: Separate common and device specific variables.
typedef struct vdi_DrawContext_ {
    // Pixel format.
    UBYTE planeNb; // Number of planes.
    UBYTE planeNbShift; // .

    struct {
        ClippingRect rect;
        bool enabled;
    } clipping;

    UWORD mode; // Drawing mode.
    UWORD color;
    bool multiFill; // Multi-plane fill flag.

    struct {
        UWORD mask; // Pattern mask.
        const UWORD *data; // Pattern data.
    } pattern;

    // Rectangle is common to most other primitives so it cannot be in the union.
    Rect rect; // Coordinates must be ordered: x1 <= x2 and y1 <= y2.

    // Line is common to several other primitives so it cannot be in the union.
    struct {
        Line line;
        UWORD mask;
        bool lastFlag;
    } line;

    // Primitives parameters (mutually exclusive).
    union {
        struct {
            const Point *points;
            WORD pointNb;
            WORD currentY;
        } polygon;
        struct {
            Point center;
            // FIXME: Move this out ?
            vdi_Circle *dda;
        } disk;
        struct {
            WORD (*abort)(void);
            WORD startX, startY;
            WORD searchColor;
        } seedFilling;
    };

    // Internal and device specific variables. TODO: Move this outside.

    // Frame buffer. Should be moved outside because it is device specific.
//    void *frameBuffer; // Framebuffer base address.
//    WORD stride; // Distance in bytes between 2 destination lines.
} vdi_DrawContext;

forceinline void vdi_DrawContext_setupPlaneNb(vdi_DrawContext * RESTRICT dc) {
    dc->planeNb = lineaVars.screen_planeNb;
    dc->planeNbShift = vdi_context.planeNbShift;
}

forceinline void vdi_DrawContext_setupClipping(vdi_DrawContext * RESTRICT dc, const vdi_VirtualWorkstation * RESTRICT vwk) {
    dc->clipping.enabled = vwk->clippingEnabled;
    if (vwk->clippingEnabled)
        dc->clipping.rect = vwk->clippingRect;
    else
        dc->clipping.rect = vdi_context.clippingRectFull;
}

forceinline void vdi_DrawContext_setupFilling(vdi_DrawContext * RESTRICT dc, const vdi_VirtualWorkstation * RESTRICT vwk, const UWORD color) {
    dc->multiFill = vwk->multiFillEnabled;
    dc->pattern.mask = vwk->patmsk;
    dc->pattern.data = vwk->patptr;
    dc->mode = vwk->wrt_mode;
    dc->color = color;
}

//--------------------------------------------------------------------------------
// Driver functions.
//--------------------------------------------------------------------------------
typedef struct vdi_Driver_ {
    void (*fillSpan)(vdi_DrawContext * RESTRICT dc);

    void (*fillRectangle)(vdi_DrawContext * RESTRICT dc);

    void (*drawLine)(vdi_DrawContext * RESTRICT dc);
    void (*drawGeneralLine)(vdi_DrawContext * RESTRICT dc);
    void (*drawVerticalLine)(vdi_DrawContext * RESTRICT dc);
    void (*drawHorizontalLine)(vdi_DrawContext * RESTRICT dc);
    
    void (*fillPolygonSpan)(vdi_DrawContext * RESTRICT dc);
    void (*fillPolygon)(vdi_DrawContext * RESTRICT dc);

    void (*fillDisk)(vdi_DrawContext * RESTRICT dc);

    void (*seedFill)(vdi_DrawContext * RESTRICT dc);
    
    void (*blit)(const vdi_BlitParameters * RESTRICT blit_info);
    void (*blitAll)(const vdi_BlitParameters * RESTRICT blit_info, BLIT * RESTRICT blt);
    void (*blitGeneral)(const vdi_BlitParameters * RESTRICT blit_info, BLIT * RESTRICT blt);
    void (*blitPlane)(BLIT * RESTRICT blt);
} vdi_Driver;

#define vdi_blitterOnly 0

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

//********************************************************************************
// Graphic device specific.
//********************************************************************************
//--------------------------------------------------------------------------------
// Screen.
//--------------------------------------------------------------------------------
UWORD * vdi_getPixelAddress(WORD x, WORD y);

forceinline bool vdi_checkPixelCoordinates(WORD x, WORD y) {
    return x >= 0 && x < lineaVars.screen_width && y >= 0 && y < lineaVars.screen_height;
}

//--------------------------------------------------------------------------------
// Shared buffer.
//--------------------------------------------------------------------------------
#define vdi_SpanBuffer_maxSpans 256 // Maximum number of spans that can be added before flushing.
#define vdi_Text_scratchBufferSize (2*212) // Text scratch buffer size (in bytes).

/*
 * A shared work area.
 * It is only used during a VDI or Line-A call.
 */
typedef struct {
    vdi_Circle circle;
    union {
        struct {
            // These buffers can be used simultaneously.
            WORD inputPoints[2*CONF_VDI_MAX_VERTICES]; // Used by GSX_ENTRY(), must be at offset 0.
            WORD polygonPoints[2 * CONF_VDI_MAX_VERTICES]; // Used by vdi_Polygon_fillSpan().
            Rect spans[vdi_SpanBuffer_maxSpans]; // Used for the span buffer.
        };
        WORD deftxbuf[vdi_Text_scratchBufferSize/sizeof(WORD)]; /* text scratch buffer */
    } common;
} vdi_SharedBuffer;

extern vdi_SharedBuffer vdi_sharedBuffer;

/* aliases for different table positions */
#define vdi_deviceResolutionX lineaVars.workstation_deviceTable[0]
#define vdi_deviceResolutionY lineaVars.workstation_deviceTable[1]
#define vdi_deviceSizeX lineaVars.workstation_deviceTable[3]
#define vdi_deviceSizeY lineaVars.workstation_deviceTable[4]
#define vdi_deviceColorNum lineaVars.workstation_deviceTable[13]

// Thickness of outline.
#define vdi_outlineThickness   1

//********************************************************************************
// Generic drawing tools.
//********************************************************************************
//--------------------------------------------------------------------------------
// Pixel access.
//--------------------------------------------------------------------------------
extern const UBYTE vdi_planeNbToRightShift[9];
extern const UBYTE vdi_planeNbToLeftShift[9];

forceinline WORD vdi_scaleWordByPlaneNb(WORD w, WORD planeNb) {
    return w << vdi_planeNbToLeftShift[planeNb];
}

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

UWORD vdi_Pixel_read(WORD x, WORD y);

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
// Software driver functions and tools.
//--------------------------------------------------------------------------------
#define vdi_Soft_asmEnabled 1 // Enable or disable inline assembly.

#define vdi_drawLine_lastLineLegacy 1

typedef struct vdi_SpanBuffer_ {
    WORD capacity, length;
    Rect *spans;
    vdi_DrawContext *dc;
} vdi_SpanBuffer;

void vdi_SpanBuffer_flush(vdi_SpanBuffer * RESTRICT spanBuffer);
Rect* vdi_SpanBuffer_add(vdi_SpanBuffer * RESTRICT spanBuffer);
forceinline void vdi_SpanBuffer_begin(vdi_SpanBuffer *spanBuffer, vdi_DrawContext * RESTRICT dc) {
    spanBuffer->capacity = vdi_SpanBuffer_maxSpans;
    spanBuffer->length = 0;
    spanBuffer->spans = vdi_sharedBuffer.common.spans;
    spanBuffer->dc = dc;
}
forceinline void vdi_SpanBuffer_end(vdi_SpanBuffer *spanBuffer) {
    vdi_SpanBuffer_flush(spanBuffer);
}

typedef struct vdi_FillingInfos_ {
    WORD width, height;
    WORD wordNb; // Line width in words.
    UWORD leftMask, rightMask; // Endmasks.
    WORD stride; // Distance in bytes between 2 destination lines.
    void *addr; // Starting screen address.
} vdi_FillingInfos;

// Coordinates must be sorted before the call: x1 < x2, y1 < y2.
void vdi_DrawContext_setupRectangle(vdi_DrawContext * RESTRICT dc, vdi_FillingInfos * RESTRICT fi);
bool vdi_DrawContext_setupHorizontalLine(vdi_DrawContext * RESTRICT dc, vdi_FillingInfos * RESTRICT fi);
bool vdi_DrawContext_setupVerticalLine(vdi_DrawContext * RESTRICT dc, vdi_FillingInfos * RESTRICT fi);

void vdi_Soft_fillRectangle(vdi_DrawContext * RESTRICT dc);

void vdi_Soft_drawLine(vdi_DrawContext * RESTRICT dc);
void vdi_Soft_drawGeneralLine(vdi_DrawContext * RESTRICT dc);
void vdi_Soft_drawVerticalLine(vdi_DrawContext * RESTRICT dc);
void vdi_Soft_drawHorizontalLine(vdi_DrawContext * RESTRICT dc);

void vdi_Soft_fillPolygonSpan(vdi_DrawContext * RESTRICT dc);
void vdi_Soft_fillPolygon(vdi_DrawContext * RESTRICT dc);

void vdi_Soft_fillDisk(vdi_DrawContext * RESTRICT dc);

void vdi_Soft_seedFill(vdi_DrawContext * RESTRICT dc);

void vdi_Soft_blit(const vdi_BlitParameters *blit_info);
void vdi_Soft_blitAll(const vdi_BlitParameters *blit_info, BLIT * RESTRICT blt);
void vdi_Soft_blitGeneral(const vdi_BlitParameters *blit_info, BLIT * RESTRICT blt);
void vdi_Soft_blitPlane(BLIT * restrict blt);

//--------------------------------------------------------------------------------
// VwkAttrib. TODO: remove and merge with vdi_DrawContext.
//--------------------------------------------------------------------------------
#if 0

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

/*
 * TODO: Remove this.
 * Helper to copy relevant vdi_VirtualWorkstation members to the VwkAttrib struct, which is
 * used to pass the required vdi_VirtualWorkstation info from VDI/Line-A polygon drawing to
 * vdi_Rectangle_fill().
 */
static inline void vdi_convertVwkToAttrib(const vdi_VirtualWorkstation * RESTRICT vwk, VwkAttrib * RESTRICT attr, const UWORD color) {
    // In the same order as in vdi_VirtualWorkstation, so that GCC can use longs for copying words
    attr->clip = vwk->clippingEnabled;
    attr->multifill = vwk->multiFillEnabled;
    attr->patmsk = vwk->patmsk;
    attr->patptr = vwk->patptr;
    attr->wrt_mode = vwk->wrt_mode;
    attr->color = color;
}
#endif


#endif
